#!/usr/bin/env python3
"""Fetch latest security, Tesouro Direto, and FX quotes for GnuCash PriceDB."""

import concurrent.futures
import datetime
import json
import math
import re
import sys
import unicodedata
import urllib.error
import urllib.parse
import urllib.request

USER_AGENT = "Mozilla/5.0"
TESOURO_CSV_URL = (
    "https://www.tesourotransparente.gov.br/ckan/dataset/"
    "df56aa42-484a-4a59-8184-7676580c81e3/resource/"
    "796d2059-14e9-44e3-80c9-2d9e30b405c1/download/"
    "precotaxatesourodireto.csv"
)
TESOURO_MAX_DATES = 10
TESOURO_MAX_LINES = 2000
SUBUNIT_CURRENCIES = {
    "GBp": ("GBP", 0.01),
    "ZAc": ("ZAR", 0.01),
    "ILA": ("ILS", 0.01),
}


def format_price(val):
    """Format a positive float as a plain decimal string without exponent."""
    fval = float(val)
    if not math.isfinite(fval) or fval <= 0.0:
        raise ValueError(f"Non-positive price value: {val}")
    fmt = f"{fval:.8f}" if fval < 1.0 else f"{fval:.6f}"
    return fmt.rstrip("0").rstrip(".") or "0"


def http_get_json(url, timeout=20):
    req = urllib.request.Request(
        url, headers={"User-Agent": USER_AGENT, "Accept": "application/json"}
    )
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        return json.loads(resp.read().decode("utf-8"))


def fetch_yahoo_latest(query, timeout=20):
    """Fetch nominal latest price, quote date, and native currency from Yahoo."""
    quoted = urllib.parse.quote(query, safe="")
    url = (
        f"https://query1.finance.yahoo.com/v8/finance/chart/{quoted}"
        "?range=10d&interval=1d"
    )
    payload = http_get_json(url, timeout=timeout)
    chart = payload.get("chart", {})
    if chart.get("error"):
        err = chart["error"]
        raise RuntimeError(err.get("description") or err.get("code") or "Yahoo Finance error")
    results = chart.get("result") or []
    if not results:
        raise RuntimeError(f"No Yahoo Finance result for {query}")

    res0 = results[0]
    meta = res0.get("meta") or {}
    raw_currency = (meta.get("currency") or "").strip()
    scale = 1.0
    if raw_currency in SUBUNIT_CURRENCIES:
        currency, scale = SUBUNIT_CURRENCIES[raw_currency]
    else:
        currency = raw_currency.upper()
    if not currency:
        raise RuntimeError(f"Missing currency metadata for {query}")

    gmtoffset = meta.get("gmtoffset", 0) or 0
    timestamps = res0.get("timestamp") or []
    indicators = res0.get("indicators") or {}
    close_list = (indicators.get("quote") or [{}])[0].get("close") or []

    bar_date = None
    bar_close = None
    for idx in range(min(len(timestamps), len(close_list)) - 1, -1, -1):
        ts = timestamps[idx]
        cval = close_list[idx]
        if ts is None or cval is None:
            continue
        fval = float(cval)
        if not math.isfinite(fval) or fval <= 0.0:
            continue
        bar_date = datetime.datetime.fromtimestamp(
            ts + gmtoffset, tz=datetime.timezone.utc
        ).date()
        bar_close = fval
        break

    rm_price = meta.get("regularMarketPrice")
    rm_time = meta.get("regularMarketTime")
    chosen_date = bar_date
    chosen_price = bar_close

    if rm_price is not None and rm_time is not None:
        rm_fval = float(rm_price)
        if math.isfinite(rm_fval) and rm_fval > 0.0:
            rm_date = datetime.datetime.fromtimestamp(
                rm_time + gmtoffset, tz=datetime.timezone.utc
            ).date()
            if bar_date is None or rm_date >= bar_date:
                chosen_date = rm_date
                chosen_price = rm_fval

    if chosen_date is None or chosen_price is None:
        raise RuntimeError(f"No valid closing price available for {query}")

    return {
        "date": chosen_date.isoformat(),
        "price": format_price(chosen_price * scale),
        "currency": currency,
    }


def fetch_fx_rate(from_curr, to_curr, timeout=20):
    """Fetch FX rate from_curr -> to_curr using Yahoo Finance with inverse fallback."""
    fc = from_curr.strip().upper()
    tc = to_curr.strip().upper()
    if not fc or not tc:
        raise ValueError("Empty currency code for FX lookup")
    if fc == tc:
        return {
            "date": datetime.date.today().isoformat(),
            "price": "1",
            "currency": tc,
        }
    try:
        res = fetch_yahoo_latest(f"{fc}{tc}=X", timeout=timeout)
        res["currency"] = tc
        return res
    except Exception as direct_err:
        try:
            inv = fetch_yahoo_latest(f"{tc}{fc}=X", timeout=timeout)
            inv_val = float(inv["price"])
            if inv_val <= 0.0:
                raise direct_err
            return {
                "date": inv["date"],
                "price": format_price(1.0 / inv_val),
                "currency": tc,
            }
        except Exception:
            raise direct_err


def normalize_text(text):
    decomposed = unicodedata.normalize("NFKD", text)
    ascii_text = "".join(ch for ch in decomposed if not unicodedata.combining(ch))
    lowered = ascii_text.lower().replace("_", " ")
    lowered = re.sub(r"\bigp-m\b", "igpm", lowered)
    return re.sub(r"\s+", " ", lowered).strip()


def _parse_br_decimal(raw):
    cleaned = raw.strip().replace(".", "").replace(",", ".")
    return float(cleaned)


def parse_tesouro_csv_lines(lines, max_dates=TESOURO_MAX_DATES, max_lines=TESOURO_MAX_LINES):
    """Parse streamed Tesouro Transparente lines into latest-by-(tipo, venc_year)."""
    table = {}
    dates_seen = set()
    rows_seen = 0

    for raw_line in lines:
        line = raw_line.strip()
        if not line or line.startswith("Tipo Titulo"):
            continue
        parts = line.split(";")
        if len(parts) < 8:
            continue

        rows_seen += 1
        if rows_seen > max_lines:
            break

        tipo = parts[0].strip()
        venc_str = parts[1].strip()
        dbase_str = parts[2].strip()
        pu_venda_str = parts[6].strip()
        pu_base_str = parts[7].strip()

        try:
            venc_date = datetime.datetime.strptime(venc_str, "%d/%m/%Y").date()
            dbase_date = datetime.datetime.strptime(dbase_str, "%d/%m/%Y").date()
        except ValueError:
            continue

        if dbase_date not in dates_seen:
            if len(dates_seen) >= max_dates:
                break
            dates_seen.add(dbase_date)

        pu = None
        for candidate_pu in (pu_venda_str, pu_base_str):
            if not candidate_pu:
                continue
            try:
                val = _parse_br_decimal(candidate_pu)
                if math.isfinite(val) and val > 0.0:
                    pu = val
                    break
            except ValueError:
                continue
        if pu is None:
            continue

        norm_tipo = normalize_text(tipo)
        key = (norm_tipo, venc_date.year)
        date_iso = dbase_date.isoformat()
        venc_iso = venc_date.isoformat()
        existing = table.get(key)
        if existing is None or (date_iso, venc_iso) > (existing["date"], existing["venc"]):
            table[key] = {
                "date": date_iso,
                "price": format_price(pu),
                "venc": venc_iso,
                "tipo": tipo,
            }

    return table


def fetch_tesouro_table(timeout=30):
    """Stream recent Tesouro Transparente CSV header rows without downloading history."""
    req = urllib.request.Request(
        TESOURO_CSV_URL, headers={"User-Agent": USER_AGENT, "Accept": "text/csv"}
    )
    with urllib.request.urlopen(req, timeout=timeout) as resp:
        decoded_lines = (raw.decode("utf-8-sig", errors="replace") for raw in resp)
        return parse_tesouro_csv_lines(decoded_lines)


def _extract_tesouro_type_and_year(candidate):
    date_match = re.search(r"\b\d{2}/\d{2}/((?:19|20)\d{2})\b", candidate)
    if date_match:
        year = int(date_match.group(1))
        type_part = candidate[: date_match.start()] + " " + candidate[date_match.end() :]
    else:
        year_matches = list(re.finditer(r"(?<!\d)((?:19|20)\d{2})(?!\d)", candidate))
        if not year_matches:
            return None, None
        ym = year_matches[-1]
        year = int(ym.group(1))
        type_part = candidate[: ym.start()] + " " + candidate[ym.end() :]

    type_part = re.sub(r"[-/]+", " ", type_part)
    norm_type = normalize_text(type_part)
    if norm_type == "tesouro renda+":
        norm_type = "tesouro renda+ aposentadoria extra"
    return norm_type, year


def match_tesouro_item(symbol, fullname, table):
    """Match a GnuCash Tesouro Direto commodity symbol/fullname to Tesouro table."""
    candidates = []
    for text in (symbol, fullname):
        cleaned = (text or "").strip()
        if cleaned and cleaned not in candidates:
            candidates.append(cleaned)

    for cand in candidates:
        norm_type, year = _extract_tesouro_type_and_year(cand)
        if not norm_type or year is None:
            continue
        if norm_type == "tesouro renda+ aposentadoria extra":
            target_years = [year + 19, year]
        elif norm_type == "tesouro educa+":
            target_years = [year + 4, year]
        else:
            target_years = [year]

        for target_year in target_years:
            hit = table.get((norm_type, target_year))
            if hit is not None:
                return {
                    "date": hit["date"],
                    "price": hit["price"],
                    "currency": "BRL",
                    "matched_tipo": hit["tipo"],
                    "matched_venc": hit["venc"],
                }

    raise ValueError(
        f"Tesouro Direto bond not found for symbol='{symbol}' fullname='{fullname}'"
    )


def _run_item(item, book_currency, tesouro_table, tesouro_error):
    item_id = str(item.get("id", ""))
    kind = item.get("kind", "")
    try:
        if kind == "yahoo":
            quote = fetch_yahoo_latest(item["query"])
            return {"id": item_id, "status": "ok", **quote}
        if kind == "fx":
            from_curr = (item.get("symbol") or "").strip().upper()
            to_curr = (item.get("target_currency") or book_currency).strip().upper()
            quote = fetch_fx_rate(from_curr, to_curr)
            return {"id": item_id, "status": "ok", **quote}
        if kind == "tesouro":
            if tesouro_error is not None:
                raise RuntimeError(tesouro_error)
            quote = match_tesouro_item(
                item.get("symbol", ""), item.get("fullname", ""), tesouro_table or {}
            )
            return {"id": item_id, "status": "ok", **quote}
        raise ValueError(f"Unknown price target kind: {kind}")
    except Exception as exc:
        return {"id": item_id, "status": "error", "error": str(exc)}


def process_request(payload):
    book_currency = (payload.get("book_currency") or "BRL").strip().upper()
    items = payload.get("items") or []

    tesouro_table = None
    tesouro_error = None
    if any(item.get("kind") == "tesouro" for item in items):
        try:
            tesouro_table = fetch_tesouro_table()
        except Exception as exc:
            tesouro_error = str(exc)

    results = [None] * len(items)
    if items:
        with concurrent.futures.ThreadPoolExecutor(max_workers=6) as executor:
            future_map = {
                executor.submit(
                    _run_item, item, book_currency, tesouro_table, tesouro_error
                ): idx
                for idx, item in enumerate(items)
            }
            for future in concurrent.futures.as_completed(future_map):
                results[future_map[future]] = future.result()

    requested_fx = {
        (item.get("symbol") or "").strip().upper()
        for item in items
        if item.get("kind") == "fx" and (item.get("symbol") or "").strip()
    }

    extra_currencies = []
    seen_extra = set()
    for item, res in zip(items, results):
        if item.get("kind") == "yahoo" and res.get("status") == "ok":
            curr = (res.get("currency") or "").strip().upper()
            if (
                curr
                and curr != book_currency
                and curr not in requested_fx
                and curr not in seen_extra
            ):
                seen_extra.add(curr)
                extra_currencies.append(curr)

    extra_fx = []
    if extra_currencies:
        def _fetch_extra(curr):
            try:
                quote = fetch_fx_rate(curr, book_currency)
                return {
                    "from_currency": curr,
                    "to_currency": book_currency,
                    "status": "ok",
                    "date": quote["date"],
                    "price": quote["price"],
                }
            except Exception as exc:
                return {
                    "from_currency": curr,
                    "to_currency": book_currency,
                    "status": "error",
                    "error": str(exc),
                }

        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as executor:
            extra_fx = list(executor.map(_fetch_extra, extra_currencies))

    return {"results": results, "extra_fx": extra_fx}


def main():
    raw_input = sys.stdin.read()
    payload = json.loads(raw_input) if raw_input.strip() else {}
    output = process_request(payload)
    sys.stdout.write(json.dumps(output))
    sys.stdout.flush()
    return 0


if __name__ == "__main__":
    sys.exit(main())
