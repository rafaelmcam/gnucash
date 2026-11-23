#!/usr/bin/env python3
"""Incremental fetcher and local CSV cache for GnuCash investment benchmarks."""

import argparse
import bisect
import csv
import datetime
import json
import math
import os
import re
import sys
import urllib.error
import urllib.parse
import urllib.request

USER_AGENT = "Mozilla/5.0"
LOOKBACK_DAYS = 10
BCB_CHUNK_DAYS = 1800


def spec_to_key(spec):
    if spec in ("CDI", "IBOVESPA", "SP500_BRL"):
        return spec
    if spec.startswith("YAHOO:"):
        ticker = spec[len("YAHOO:"):].strip()
        if not ticker:
            raise ValueError("Empty Yahoo Finance ticker")
        safe = re.sub(r"[^A-Za-z0-9._-]", "_", ticker)
        return f"YAHOO_{safe}"
    raise ValueError(f"Unsupported benchmark spec: {spec}")


def load_cache(cache_path):
    """Return (fetched_start, fetched_end, rows_dict) from cache CSV."""
    fetched_start = None
    fetched_end = None
    rows = {}
    if not os.path.exists(cache_path):
        return fetched_start, fetched_end, rows
    with open(cache_path, "r", encoding="utf-8", newline="") as fh:
        lines = fh.readlines()
    data_lines = []
    for line in lines:
        stripped = line.strip()
        if stripped.startswith("# fetched:"):
            m = re.match(
                r"^#\s*fetched:\s*(\d{4}-\d{2}-\d{2})\.\.(\d{4}-\d{2}-\d{2})$",
                stripped,
            )
            if m:
                fetched_start = datetime.date.fromisoformat(m.group(1))
                fetched_end = datetime.date.fromisoformat(m.group(2))
        elif stripped and not stripped.startswith("#"):
            data_lines.append(line)
    if data_lines:
        reader = csv.DictReader(data_lines)
        for row in reader:
            d = (row.get("date") or "").strip()
            v = (row.get("value") or "").strip()
            if d and v:
                try:
                    val = float(v)
                    if not math.isnan(val):
                        rows[d] = val
                except ValueError:
                    continue
    return fetched_start, fetched_end, rows


def save_cache(cache_path, fetched_start, fetched_end, rows):
    os.makedirs(os.path.dirname(cache_path), exist_ok=True)
    tmp_path = f"{cache_path}.tmp.{os.getpid()}"
    with open(tmp_path, "w", encoding="utf-8", newline="") as fh:
        if fetched_start is not None and fetched_end is not None:
            fh.write(f"# fetched: {fetched_start.isoformat()}..{fetched_end.isoformat()}\n")
        writer = csv.writer(fh)
        writer.writerow(["date", "value"])
        for d in sorted(rows.keys()):
            writer.writerow([d, f"{rows[d]:.8f}".rstrip("0").rstrip(".") or "0"])
    os.replace(tmp_path, cache_path)


def compute_missing_intervals(start_eff, end_eff, fetched_start, fetched_end):
    if start_eff > end_eff:
        return []
    if fetched_start is None or fetched_end is None:
        return [(start_eff, end_eff)]
    intervals = []
    if start_eff < fetched_start:
        intervals.append((start_eff, fetched_start - datetime.timedelta(days=1)))
    if end_eff > fetched_end:
        intervals.append((fetched_end + datetime.timedelta(days=1), end_eff))
    return [(a, b) for (a, b) in intervals if a <= b]


def http_get_json(url, headers=None, timeout=20):
    req_headers = {"User-Agent": USER_AGENT, "Accept": "application/json"}
    if headers:
        req_headers.update(headers)
    req = urllib.request.Request(url, headers=req_headers)
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        return json.loads(resp.read().decode("utf-8"))


def fetch_cdi(start_d, end_d):
    """Fetch daily CDI % rates from BCB SGS series 12."""
    out = {}
    cur = start_d
    while cur <= end_d:
        chunk_end = min(end_d, cur + datetime.timedelta(days=BCB_CHUNK_DAYS))
        url = (
            "https://api.bcb.gov.br/dados/serie/bcdata.sgs.12/dados"
            f"?formato=json&dataInicial={cur.strftime('%d/%m/%Y')}"
            f"&dataFinal={chunk_end.strftime('%d/%m/%Y')}"
        )
        try:
            payload = http_get_json(url)
        except urllib.error.HTTPError as exc:
            if exc.code == 404 and (chunk_end - cur).days <= 7:
                payload = []
            else:
                raise
        if isinstance(payload, dict) and ("error" in payload or "erro" in payload):
            if (chunk_end - cur).days <= 7:
                payload = []
            else:
                raise RuntimeError(payload.get("error") or payload.get("erro") or "BCB API error")
        if isinstance(payload, list):
            for item in payload:
                raw_date = item.get("data")
                raw_val = item.get("valor")
                if raw_date and raw_val is not None:
                    d_iso = datetime.datetime.strptime(raw_date, "%d/%m/%Y").date().isoformat()
                    val = float(str(raw_val).replace(",", "."))
                    if not math.isnan(val):
                        out[d_iso] = val
        cur = chunk_end + datetime.timedelta(days=1)
    return out


def fetch_yahoo(ticker, start_d, end_d):
    """Fetch daily adjusted close (fallback close) prices from Yahoo Finance."""
    p1 = int(datetime.datetime(start_d.year, start_d.month, start_d.day, tzinfo=datetime.timezone.utc).timestamp())
    end_plus = end_d + datetime.timedelta(days=1)
    p2 = int(datetime.datetime(end_plus.year, end_plus.month, end_plus.day, tzinfo=datetime.timezone.utc).timestamp())
    quoted = urllib.parse.quote(ticker, safe="")
    url = (
        f"https://query1.finance.yahoo.com/v8/finance/chart/{quoted}"
        f"?period1={p1}&period2={p2}&interval=1d&events=history"
    )
    payload = http_get_json(url)
    chart = payload.get("chart", {})
    if chart.get("error"):
        err = chart["error"]
        raise RuntimeError(err.get("description") or err.get("code") or "Yahoo Finance error")
    results = chart.get("result") or []
    if not results:
        return {}
    res0 = results[0]
    gmtoffset = res0.get("meta", {}).get("gmtoffset", 0) or 0
    timestamps = res0.get("timestamp") or []
    indicators = res0.get("indicators") or {}
    adj_list = (indicators.get("adjclose") or [{}])[0].get("adjclose") or []
    close_list = (indicators.get("quote") or [{}])[0].get("close") or []

    out = {}
    for idx, ts in enumerate(timestamps):
        if ts is None:
            continue
        val = adj_list[idx] if idx < len(adj_list) else None
        if val is None and idx < len(close_list):
            val = close_list[idx]
        if val is None:
            continue
        fval = float(val)
        if math.isnan(fval) or fval <= 0.0:
            continue
        local_dt = datetime.datetime.fromtimestamp(ts + gmtoffset, tz=datetime.timezone.utc)
        out[local_dt.date().isoformat()] = fval
    return out


def fetch_sp500_brl(start_d, end_d):
    """Fetch ^GSPC multiplied by latest USDBRL=X on or before each date."""
    fx_start = start_d - datetime.timedelta(days=LOOKBACK_DAYS)
    gspc = fetch_yahoo("^GSPC", start_d, end_d)
    usdbrl = fetch_yahoo("USDBRL=X", fx_start, end_d)
    if not gspc or not usdbrl:
        return {}
    fx_dates = sorted(usdbrl.keys())
    out = {}
    start_iso = start_d.isoformat()
    end_iso = end_d.isoformat()
    for d_iso, sp_val in gspc.items():
        if d_iso < start_iso or d_iso > end_iso:
            continue
        pos = bisect.bisect_right(fx_dates, d_iso) - 1
        fx_key = fx_dates[pos] if pos >= 0 else fx_dates[0]
        out[d_iso] = sp_val * usdbrl[fx_key]
    return out


def fetch_spec_interval(spec, start_d, end_d):
    if spec == "CDI":
        return fetch_cdi(start_d, end_d)
    if spec == "IBOVESPA":
        return fetch_yahoo("^BVSP", start_d, end_d)
    if spec == "SP500_BRL":
        return fetch_sp500_brl(start_d, end_d)
    if spec.startswith("YAHOO:"):
        ticker = spec[len("YAHOO:"):].strip()
        return fetch_yahoo(ticker, start_d, end_d)
    raise ValueError(f"Unsupported benchmark spec: {spec}")


def update_benchmark(spec, cache_dir, start_date, end_date, today=None):
    if today is None:
        today = datetime.date.today()
    key = spec_to_key(spec)
    cache_path = os.path.join(cache_dir, f"{key}.csv")
    end_eff = min(end_date, today)
    start_eff = min(start_date - datetime.timedelta(days=LOOKBACK_DAYS), end_eff)

    fetched_start, fetched_end, rows = load_cache(cache_path)
    missing = compute_missing_intervals(start_eff, end_eff, fetched_start, fetched_end)
    if not missing:
        return {
            "spec": spec,
            "key": key,
            "status": "cached",
            "points": len(rows),
            "added": 0,
        }

    added = 0
    for m_start, m_end in missing:
        new_rows = fetch_spec_interval(spec, m_start, m_end)
        for k, v in new_rows.items():
            if k not in rows:
                added += 1
            rows[k] = v
        fetched_start = m_start if fetched_start is None else min(fetched_start, m_start)
        fetched_end = m_end if fetched_end is None else max(fetched_end, m_end)

    save_cache(cache_path, fetched_start, fetched_end, rows)
    return {
        "spec": spec,
        "key": key,
        "status": "fetched",
        "points": len(rows),
        "added": added,
    }


def main(argv=None):
    parser = argparse.ArgumentParser(description="Fetch benchmark series into local CSV cache.")
    parser.add_argument("--cache-dir", required=True)
    parser.add_argument("--start", required=True)
    parser.add_argument("--end", required=True)
    parser.add_argument("specs", nargs="+")
    args = parser.parse_args(argv)

    start_date = datetime.date.fromisoformat(args.start)
    end_date = datetime.date.fromisoformat(args.end)
    results = []
    for spec in args.specs:
        try:
            results.append(update_benchmark(spec, args.cache_dir, start_date, end_date))
        except Exception as exc:
            results.append({
                "spec": spec,
                "status": "error",
                "error": str(exc),
            })
    print(json.dumps({"results": results}))
    return 0


if __name__ == "__main__":
    sys.exit(main())
