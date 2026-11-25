/********************************************************************\
 * dialog-price-updater.h -- Update online prices for active        *
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

#ifndef DIALOG_PRICE_UPDATER_H
#define DIALOG_PRICE_UPDATER_H

#include <gtk/gtk.h>

#ifdef __cplusplus
#include <cstddef>
#include <string>
#include <vector>
#include "gnc-commodity.h"
#include "qof.h"

enum class PriceTargetKind
{
    YAHOO,
    TESOURO,
    FX
};

struct PriceTarget
{
    std::string id;
    PriceTargetKind kind;
    gnc_commodity *commodity;
    std::string symbol;
    std::string fullname;
    std::string ns;
    std::string query;
    std::string target_currency;
};

struct SkippedTarget
{
    std::string ns;
    std::string symbol;
    std::string reason;
};

struct ActiveTargetsResult
{
    std::vector<PriceTarget> targets;
    std::vector<SkippedTarget> skipped;
};

struct PriceUpdateSummary
{
    std::size_t updated = 0;
    std::size_t failed = 0;
    std::vector<std::string> updated_details;
    std::vector<std::string> error_details;
};

ActiveTargetsResult gnc_collect_active_price_targets (QofBook *book,
                                                      gnc_commodity *book_currency);

std::string gnc_build_price_fetch_request_json (const ActiveTargetsResult &active,
                                                const std::string &book_currency);

PriceUpdateSummary gnc_apply_price_fetch_response (QofBook *book,
                                                   const std::vector<PriceTarget> &targets,
                                                   const std::string &response_json);

extern "C" {
#endif

void gnc_update_active_prices_dialog (GtkWindow *parent);

#ifdef __cplusplus
}
#endif

#endif /* DIALOG_PRICE_UPDATER_H */
