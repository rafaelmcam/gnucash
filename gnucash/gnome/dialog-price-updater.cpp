/********************************************************************\
 * dialog-price-updater.cpp -- Update online prices for active      *
 * commodities and currencies                                       *
 *                                                                  *
 * This program is free software; you can redistribute it and/or    *
 * modify it under the terms of the GNU General Public License as   *
 * published by the Free Software Foundation; either version 2 of   *
 * the License, or (at your option) any later version.              *
 *                                                                  *
 * This program is distributed in the hope that it will be useful,  *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of   *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    *
 * GNU General Public License for more details.                     *
 *                                                                  *
 * You should have received a copy of the GNU General Public License*
 * along with this program; if not, contact:                        *
 *                                                                  *
 * Free Software Foundation           Voice:  +1-617-542-5942       *
 * 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652       *
 * Boston, MA  02110-1301,  USA       gnu@gnu.org                   *
\********************************************************************/

#include <config.h>

#include <gtk/gtk.h>
#include <gio/gio.h>
#include <glib/gi18n.h>

#include <algorithm>
#include <cstdlib>
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <boost/algorithm/string.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/property_tree/ptree.hpp>

#include "Account.h"
#include "dialog-price-updater.h"
#include "gnc-component-manager.h"
#include "gnc-datetime.hpp"
#include "gnc-filepath-utils.h"
#include "gnc-gui-query.h"
#include "gnc-numeric.hpp"
#include "gnc-pricedb.h"
#include "gnc-session.h"
#include "gnc-ui-util.h"
#include "gnc-ui.h"

namespace bpt = boost::property_tree;

static QofLogModule log_module = GNC_MOD_GUI;

static bool
is_ignored_account_type (GNCAccountType type)
{
    switch (type)
    {
        case ACCT_TYPE_TRADING:
        case ACCT_TYPE_INCOME:
        case ACCT_TYPE_EXPENSE:
        case ACCT_TYPE_EQUITY:
        case ACCT_TYPE_ROOT:
        case ACCT_TYPE_NONE:
            return true;
        default:
            return false;
    }
}

static std::string
json_escape (const std::string &input)
{
    std::string out;
    out.reserve (input.size () + 4);
    for (unsigned char ch : input)
    {
        switch (ch)
        {
            case '"':  out += "\\\""; break;
            case '\\': out += "\\\\"; break;
            case '\b': out += "\\b"; break;
            case '\f': out += "\\f"; break;
            case '\n': out += "\\n"; break;
            case '\r': out += "\\r"; break;
            case '\t': out += "\\t"; break;
            default:
                if (ch < 0x20)
                {
                    char buf[8];
                    g_snprintf (buf, sizeof (buf), "\\u%04x", ch);
                    out += buf;
                }
                else
                {
                    out.push_back (static_cast<char> (ch));
                }
                break;
        }
    }
    return out;
}

static const char*
target_kind_to_str (PriceTargetKind kind)
{
    switch (kind)
    {
        case PriceTargetKind::YAHOO:   return "yahoo";
        case PriceTargetKind::TESOURO: return "tesouro";
        case PriceTargetKind::FX:      return "fx";
    }
    return "yahoo";
}

ActiveTargetsResult
gnc_collect_active_price_targets (QofBook *book, gnc_commodity *book_currency)
{
    ActiveTargetsResult res;
    if (!book)
        return res;

    Account *root = gnc_book_get_root_account (book);
    if (!root)
        return res;

    std::string book_curr_str;
    if (book_currency && gnc_commodity_get_mnemonic (book_currency))
    {
        book_curr_str = gnc_commodity_get_mnemonic (book_currency);
        boost::trim (book_curr_str);
        boost::to_upper (book_curr_str);
    }

    std::unordered_set<const gnc_commodity*> seen_commodities;
    GList *descendants = gnc_account_get_descendants (root);
    for (GList *node = descendants; node != nullptr; node = node->next)
    {
        Account *acc = GNC_ACCOUNT (node->data);
        if (!acc || is_ignored_account_type (xaccAccountGetType (acc)))
            continue;

        gnc_numeric bal = xaccAccountGetBalance (acc);
        if (!gnc_numeric_positive_p (bal))
            continue;

        gnc_commodity *comm = xaccAccountGetCommodity (acc);
        if (!comm || !seen_commodities.insert (comm).second)
            continue;

        const char *raw_mnemonic = gnc_commodity_get_mnemonic (comm);
        const char *raw_ns = gnc_commodity_get_namespace (comm);
        const char *raw_fullname = gnc_commodity_get_fullname (comm);
        std::string mnemonic = raw_mnemonic ? raw_mnemonic : "";
        std::string ns = raw_ns ? raw_ns : "";
        std::string fullname = raw_fullname ? raw_fullname : "";
        boost::trim (mnemonic);
        boost::trim (ns);
        boost::trim (fullname);

        if (mnemonic.empty () || mnemonic == "XXX")
            continue;

        if (gnc_commodity_is_currency (comm))
        {
            if (book_currency && !gnc_commodity_equiv (comm, book_currency) && !book_curr_str.empty ())
            {
                std::string upper_curr = boost::to_upper_copy (mnemonic);
                PriceTarget target;
                target.kind = PriceTargetKind::FX;
                target.commodity = comm;
                target.symbol = upper_curr;
                target.fullname = fullname;
                target.ns = ns;
                target.query = upper_curr + book_curr_str + "=X";
                target.target_currency = book_curr_str;
                res.targets.push_back (std::move (target));
            }
            continue;
        }

        std::string lower_ns = boost::to_lower_copy (ns);
        if (lower_ns == "tesouro direto")
        {
            PriceTarget target;
            target.kind = PriceTargetKind::TESOURO;
            target.commodity = comm;
            target.symbol = mnemonic;
            target.fullname = fullname;
            target.ns = ns;
            target.query = mnemonic;
            target.target_currency = "BRL";
            res.targets.push_back (std::move (target));
            continue;
        }

        std::string upper_sym = boost::to_upper_copy (mnemonic);
        if (upper_sym == "BTC")
        {
            PriceTarget target;
            target.kind = PriceTargetKind::YAHOO;
            target.commodity = comm;
            target.symbol = mnemonic;
            target.fullname = fullname;
            target.ns = ns;
            target.query = "BTC-USD";
            res.targets.push_back (std::move (target));
            continue;
        }

        if (mnemonic.find ('.') != std::string::npos)
        {
            PriceTarget target;
            target.kind = PriceTargetKind::YAHOO;
            target.commodity = comm;
            target.symbol = mnemonic;
            target.fullname = fullname;
            target.ns = ns;
            target.query = mnemonic;
            res.targets.push_back (std::move (target));
            continue;
        }

        res.skipped.push_back ({
            ns,
            mnemonic,
            _("No '.' in ticker symbol and not under 'Tesouro Direto'")
        });
    }
    g_list_free (descendants);

    for (std::size_t i = 0; i < res.targets.size (); ++i)
        res.targets[i].id = std::to_string (i);

    return res;
}

std::string
gnc_build_price_fetch_request_json (const ActiveTargetsResult &active,
                                    const std::string &book_currency)
{
    std::ostringstream oss;
    oss << "{\"book_currency\":\"" << json_escape (book_currency) << "\",\"items\":[";
    for (std::size_t i = 0; i < active.targets.size (); ++i)
    {
        if (i > 0)
            oss << ",";
        const auto &t = active.targets[i];
        oss << "{"
            << "\"id\":\"" << json_escape (t.id) << "\","
            << "\"kind\":\"" << target_kind_to_str (t.kind) << "\","
            << "\"symbol\":\"" << json_escape (t.symbol) << "\","
            << "\"fullname\":\"" << json_escape (t.fullname) << "\","
            << "\"query\":\"" << json_escape (t.query) << "\","
            << "\"target_currency\":\"" << json_escape (t.target_currency) << "\""
            << "}";
    }
    oss << "]}";
    return oss.str ();
}

static bool
upsert_price_overwrite (QofBook *book,
                        GNCPriceDB *pdb,
                        gnc_commodity *commodity,
                        gnc_commodity *currency,
                        const std::string &date_iso,
                        const std::string &price_str,
                        std::string &error_out)
{
    if (!book || !pdb || !commodity || !currency)
    {
        error_out = _("Invalid commodity or currency");
        return false;
    }

    time64 quote_t64 = 0;
    try
    {
        GncDateTime dt (GncDate (date_iso, "y-m-d"), DayPart::neutral);
        quote_t64 = static_cast<time64> (dt);
    }
    catch (const std::exception &err)
    {
        error_out = std::string (_("Invalid quote date '")) + date_iso + "': " + err.what ();
        return false;
    }

    GncNumeric num_price;
    try
    {
        num_price = GncNumeric (price_str);
    }
    catch (const std::exception &err)
    {
        error_out = std::string (_("Invalid price '")) + price_str + "': " + err.what ();
        return false;
    }

    if (num_price <= GncNumeric (0, 1))
    {
        error_out = std::string (_("Non-positive price '")) + price_str + "'";
        return false;
    }

    while (GNCPrice *old_price = gnc_pricedb_lookup_day_t64 (pdb, commodity, currency, quote_t64))
    {
        gnc_pricedb_remove_price (pdb, old_price);
        gnc_price_unref (old_price);
    }

    GNCPrice *price = gnc_price_create (book);
    gnc_price_begin_edit (price);
    gnc_price_set_commodity (price, commodity);
    gnc_price_set_currency (price, currency);
    gnc_price_set_time64 (price, quote_t64);
    gnc_price_set_source (price, PRICE_SOURCE_FQ);
    gnc_price_set_typestr (price, PRICE_TYPE_LAST);
    gnc_price_set_value (price, static_cast<gnc_numeric> (num_price));
    gnc_price_commit_edit (price);

    if (!gnc_pricedb_add_price (pdb, price))
    {
        gnc_price_unref (price);
        error_out = _("Failed to insert price into Price Database");
        return false;
    }
    return true;
}

PriceUpdateSummary
gnc_apply_price_fetch_response (QofBook *book,
                                const std::vector<PriceTarget> &targets,
                                const std::string &response_json)
{
    PriceUpdateSummary summary;
    if (!book)
    {
        summary.failed = targets.size ();
        summary.error_details.emplace_back (_("No open GnuCash book."));
        return summary;
    }

    bpt::ptree root;
    try
    {
        std::istringstream iss (response_json);
        bpt::read_json (iss, root);
    }
    catch (const std::exception &err)
    {
        summary.failed = targets.size ();
        summary.error_details.push_back (
            std::string (_("Failed to parse fetcher JSON response: ")) + err.what ());
        return summary;
    }

    std::unordered_map<std::string, const PriceTarget*> target_by_id;
    target_by_id.reserve (targets.size ());
    for (const auto &t : targets)
        target_by_id.emplace (t.id, &t);

    GNCPriceDB *pdb = gnc_pricedb_get_db (book);
    gnc_commodity_table *comm_table = gnc_commodity_table_get_table (book);

    if (auto results_opt = root.get_child_optional ("results"))
    {
        for (const auto &kv : *results_opt)
        {
            const bpt::ptree &item = kv.second;
            std::string id = item.get<std::string> ("id", "");
            auto it = target_by_id.find (id);
            if (it == target_by_id.end ())
                continue;
            const PriceTarget *target = it->second;

            std::string status = item.get<std::string> ("status", "error");
            if (status != "ok")
            {
                std::string err = item.get<std::string> ("error", _("Unknown error"));
                ++summary.failed;
                summary.error_details.push_back (target->symbol + ": " + err);
                continue;
            }

            std::string date_iso = item.get<std::string> ("date", "");
            std::string price_str = item.get<std::string> ("price", "");
            std::string curr_code = item.get<std::string> ("currency", target->target_currency);
            boost::trim (curr_code);
            boost::to_upper (curr_code);

            gnc_commodity *quote_curr = gnc_commodity_table_lookup (
                comm_table, GNC_COMMODITY_NS_CURRENCY, curr_code.c_str ());
            if (!quote_curr)
            {
                ++summary.failed;
                summary.error_details.push_back (
                    target->symbol + ": " + std::string (_("Unknown ISO currency ")) + curr_code);
                continue;
            }

            std::string err_msg;
            if (upsert_price_overwrite (book, pdb, target->commodity, quote_curr,
                                        date_iso, price_str, err_msg))
            {
                ++summary.updated;
                summary.updated_details.push_back (
                    target->symbol + ": " + price_str + " " + curr_code + " (" + date_iso + ")");
            }
            else
            {
                ++summary.failed;
                summary.error_details.push_back (target->symbol + ": " + err_msg);
            }
        }
    }

    if (auto extra_opt = root.get_child_optional ("extra_fx"))
    {
        for (const auto &kv : *extra_opt)
        {
            const bpt::ptree &fx = kv.second;
            std::string from_code = boost::to_upper_copy (
                boost::trim_copy (fx.get<std::string> ("from_currency", "")));
            std::string to_code = boost::to_upper_copy (
                boost::trim_copy (fx.get<std::string> ("to_currency", "")));
            std::string label = from_code + "/" + to_code;

            std::string status = fx.get<std::string> ("status", "error");
            if (status != "ok")
            {
                std::string err = fx.get<std::string> ("error", _("Unknown error"));
                ++summary.failed;
                summary.error_details.push_back (label + ": " + err);
                continue;
            }

            gnc_commodity *from_comm = gnc_commodity_table_lookup (
                comm_table, GNC_COMMODITY_NS_CURRENCY, from_code.c_str ());
            gnc_commodity *to_comm = gnc_commodity_table_lookup (
                comm_table, GNC_COMMODITY_NS_CURRENCY, to_code.c_str ());
            if (!from_comm || !to_comm)
            {
                ++summary.failed;
                summary.error_details.push_back (
                    label + ": " + _("Unknown currency in book commodity table"));
                continue;
            }

            std::string date_iso = fx.get<std::string> ("date", "");
            std::string price_str = fx.get<std::string> ("price", "");
            std::string err_msg;
            if (upsert_price_overwrite (book, pdb, from_comm, to_comm,
                                        date_iso, price_str, err_msg))
            {
                ++summary.updated;
                summary.updated_details.push_back (
                    label + ": " + price_str + " (" + date_iso + ")");
            }
            else
            {
                ++summary.failed;
                summary.error_details.push_back (label + ": " + err_msg);
            }
        }
    }

    return summary;
}

static std::string
read_trimmed_first_line (const std::string &path)
{
    std::ifstream ifs (path);
    if (!ifs.is_open ())
        return "";
    std::string line;
    if (!std::getline (ifs, line))
        return "";
    boost::trim (line);
    return line;
}

static std::string
resolve_price_python ()
{
    if (const char *env_py = g_getenv ("GNC_BENCHMARK_PYTHON"))
    {
        if (*env_py != '\0' && g_file_test (env_py, G_FILE_TEST_EXISTS))
            return env_py;
    }

    gchar *venv_py = gnc_build_scm_path ("gnucash/reports/support/.venv/bin/python");
    if (venv_py)
    {
        bool exists = g_file_test (venv_py, G_FILE_TEST_EXISTS);
        std::string res = venv_py;
        g_free (venv_py);
        if (exists)
            return res;
    }

    gchar *cfg_path = gnc_build_scm_path ("gnucash/reports/support/benchmark-python.txt");
    if (cfg_path)
    {
        std::string configured = read_trimmed_first_line (cfg_path);
        g_free (cfg_path);
        if (!configured.empty () && g_file_test (configured.c_str (), G_FILE_TEST_EXISTS))
            return configured;
    }

    return "python3";
}

static void
show_summary_dialog (GtkWindow *parent,
                     const PriceUpdateSummary &summary,
                     const std::vector<SkippedTarget> &skipped)
{
    GtkWidget *dialog = gtk_dialog_new_with_buttons (
        _("Update Online Prices"),
        parent,
        static_cast<GtkDialogFlags> (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT),
        _("_OK"),
        GTK_RESPONSE_OK,
        nullptr);
    gtk_window_set_default_size (GTK_WINDOW (dialog), 620, 420);

    GtkWidget *content = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
    gtk_container_set_border_width (GTK_CONTAINER (content), 12);
    gtk_box_set_spacing (GTK_BOX (content), 8);

    gchar *header_text = g_strdup_printf (
        _("Updated: %zu  |  Errors: %zu  |  Skipped: %zu"),
        summary.updated, summary.failed, skipped.size ());
    GtkWidget *header_lbl = gtk_label_new (header_text);
    g_free (header_text);
    gtk_widget_set_halign (header_lbl, GTK_ALIGN_START);
    gtk_box_pack_start (GTK_BOX (content), header_lbl, FALSE, FALSE, 0);

    std::ostringstream body;
    if (!summary.updated_details.empty ())
    {
        body << _("Updated prices:") << "\n";
        for (const auto &line : summary.updated_details)
            body << "  - " << line << "\n";
        body << "\n";
    }
    if (!summary.error_details.empty ())
    {
        body << _("Errors:") << "\n";
        for (const auto &line : summary.error_details)
            body << "  - " << line << "\n";
        body << "\n";
    }
    if (!skipped.empty ())
    {
        body << _("Skipped commodities:") << "\n";
        for (const auto &item : skipped)
        {
            body << "  - ";
            if (!item.ns.empty ())
                body << "[" << item.ns << "] ";
            body << item.symbol << ": " << item.reason << "\n";
        }
    }

    GtkWidget *scrolled = gtk_scrolled_window_new (nullptr, nullptr);
    gtk_scrolled_window_set_policy (
        GTK_SCROLLED_WINDOW (scrolled), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
    gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolled), GTK_SHADOW_IN);

    GtkWidget *text_view = gtk_text_view_new ();
    gtk_text_view_set_editable (GTK_TEXT_VIEW (text_view), FALSE);
    gtk_text_view_set_cursor_visible (GTK_TEXT_VIEW (text_view), FALSE);
    gtk_text_view_set_wrap_mode (GTK_TEXT_VIEW (text_view), GTK_WRAP_WORD_CHAR);
    GtkTextBuffer *buf = gtk_text_view_get_buffer (GTK_TEXT_VIEW (text_view));
    std::string body_str = body.str ();
    gtk_text_buffer_set_text (buf, body_str.c_str (), -1);

    gtk_container_add (GTK_CONTAINER (scrolled), text_view);
    gtk_box_pack_start (GTK_BOX (content), scrolled, TRUE, TRUE, 0);

    gtk_widget_show_all (dialog);
    gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);
}

void
gnc_update_active_prices_dialog (GtkWindow *parent)
{
    ENTER (" ");
    QofBook *book = gnc_get_current_book ();
    if (!book)
    {
        LEAVE ("no open book");
        return;
    }

    if (qof_book_is_readonly (book))
    {
        gnc_error_dialog (parent, "%s", _("Book is read-only. Prices cannot be updated."));
        LEAVE ("read-only");
        return;
    }

    gnc_commodity *book_currency = gnc_default_currency ();
    if (!book_currency)
    {
        if (Account *root = gnc_book_get_root_account (book))
            book_currency = xaccAccountGetCommodity (root);
    }

    std::string book_curr_code =
        (book_currency && gnc_commodity_get_mnemonic (book_currency))
            ? boost::to_upper_copy (boost::trim_copy (std::string (gnc_commodity_get_mnemonic (book_currency))))
            : "BRL";

    ActiveTargetsResult active = gnc_collect_active_price_targets (book, book_currency);
    if (active.targets.empty ())
    {
        std::ostringstream msg;
        msg << _("No eligible commodities or foreign currencies with positive balance were found.");
        if (!active.skipped.empty ())
        {
            msg << "\n\n" << _("Skipped commodities:") << "\n";
            for (const auto &item : active.skipped)
                msg << "  - " << item.symbol << " (" << item.reason << ")\n";
        }
        gnc_info_dialog (parent, "%s", msg.str ().c_str ());
        LEAVE ("no eligible targets");
        return;
    }

    gchar *script_cstr = gnc_build_scm_path ("gnucash/reports/support/price-fetch.py");
    std::string script_path = script_cstr ? script_cstr : "";
    g_free (script_cstr);
    if (script_path.empty () || !g_file_test (script_path.c_str (), G_FILE_TEST_EXISTS))
    {
        gnc_error_dialog (parent, _("Price fetcher script not found: %s"), script_path.c_str ());
        LEAVE ("missing script");
        return;
    }

    std::string python_exe = resolve_price_python ();
    std::string request_json = gnc_build_price_fetch_request_json (active, book_curr_code);

    while (gtk_events_pending ())
        gtk_main_iteration ();

    gnc_set_busy_cursor (nullptr, TRUE);
    GError *gerr = nullptr;
    const gchar *argv[] = { python_exe.c_str (), script_path.c_str (), nullptr };
    GSubprocess *proc = g_subprocess_newv (
        argv,
        static_cast<GSubprocessFlags> (
            G_SUBPROCESS_FLAGS_STDIN_PIPE |
            G_SUBPROCESS_FLAGS_STDOUT_PIPE |
            G_SUBPROCESS_FLAGS_STDERR_PIPE),
        &gerr);

    gchar *stdout_buf = nullptr;
    gchar *stderr_buf = nullptr;
    gboolean comm_ok = FALSE;
    gboolean proc_ok = FALSE;

    if (proc)
    {
        comm_ok = g_subprocess_communicate_utf8 (
            proc, request_json.c_str (), nullptr, &stdout_buf, &stderr_buf, &gerr);
        if (comm_ok)
            proc_ok = g_subprocess_get_successful (proc);
    }
    gnc_unset_busy_cursor (nullptr);

    if (!proc || !comm_ok || !proc_ok)
    {
        std::string err_msg = gerr ? gerr->message : _("Price fetcher subprocess failed.");
        if (stderr_buf && *stderr_buf)
        {
            err_msg += "\n\n";
            err_msg += stderr_buf;
        }
        if (gerr)
            g_error_free (gerr);
        g_free (stdout_buf);
        g_free (stderr_buf);
        if (proc)
            g_object_unref (proc);
        gnc_error_dialog (parent, _("Failed to fetch online prices:\n%s"), err_msg.c_str ());
        LEAVE ("subprocess failed");
        return;
    }

    std::string response_json = stdout_buf ? stdout_buf : "";
    g_free (stdout_buf);
    g_free (stderr_buf);
    g_object_unref (proc);

    PriceUpdateSummary summary = gnc_apply_price_fetch_response (
        book, active.targets, response_json);
    gnc_gui_refresh_all ();
    show_summary_dialog (parent, summary, active.skipped);
    LEAVE ("updated=%zu failed=%zu", summary.updated, summary.failed);
}
