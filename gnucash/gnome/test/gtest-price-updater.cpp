/********************************************************************\
 * gtest-price-updater.cpp -- Unit tests for dialog-price-updater  *
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
#include <glib.h>

#include <algorithm>
#include <memory>
#include <string>
#include <unordered_map>

#include "dialog-price-updater.h"
#include "Account.h"
#include "Split.h"
#include "Transaction.h"
#include "gnc-commodity.h"
#include "gnc-datetime.hpp"
#include "gnc-numeric.hpp"
#include "gnc-pricedb.h"
#include "gnc-pricedb-p.h"

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wcpp"
#include <gtest/gtest.h>
#pragma GCC diagnostic pop

namespace
{

Account*
create_account (QofBook *book, Account *parent, const char *name,
                GNCAccountType type, gnc_commodity *comm)
{
    Account *acc = xaccMallocAccount (book);
    xaccAccountBeginEdit (acc);
    xaccAccountSetName (acc, name);
    xaccAccountSetType (acc, type);
    xaccAccountSetCommodity (acc, comm);
    gnc_account_append_child (parent, acc);
    xaccAccountCommitEdit (acc);
    return acc;
}

void
add_split_txn (QofBook *book, Account *equity, Account *acc,
               gnc_commodity *txn_curr, gnc_numeric amt, gnc_numeric val)
{
    Transaction *txn = xaccMallocTransaction (book);
    xaccTransBeginEdit (txn);
    xaccTransSetCurrency (txn, txn_curr);
    xaccTransSetDate (txn, 1, 11, 2026);

    Split *s1 = xaccMallocSplit (book);
    xaccSplitSetParent (s1, txn);
    xaccSplitSetAccount (s1, equity);
    xaccSplitSetValue (s1, gnc_numeric_neg (val));
    xaccSplitSetAmount (s1, gnc_numeric_neg (val));

    Split *s2 = xaccMallocSplit (book);
    xaccSplitSetParent (s2, txn);
    xaccSplitSetAccount (s2, acc);
    xaccSplitSetValue (s2, val);
    xaccSplitSetAmount (s2, amt);

    xaccTransCommitEdit (txn);
}

class PriceUpdaterTest : public ::testing::Test
{
protected:
    QofBook *m_book = nullptr;
    Account *m_root = nullptr;
    gnc_commodity *m_brl = nullptr;
    gnc_commodity *m_usd = nullptr;
    gnc_commodity *m_b5p211 = nullptr;
    gnc_commodity *m_btc = nullptr;
    gnc_commodity *m_petr4 = nullptr;
    gnc_commodity *m_old = nullptr;
    gnc_commodity *m_custom = nullptr;
    gnc_commodity *m_ipca29 = nullptr;
    gnc_commodity *m_renda50 = nullptr;

    void SetUp () override
    {
        qof_init ();
        qof_book_register ();
        gnc_pricedb_register ();
        gnc_commodity_table_register ();

        m_book = qof_book_new ();
        m_root = gnc_account_create_root (m_book);

        gnc_commodity_table *tbl = gnc_commodity_table_get_table (m_book);
        m_brl = gnc_commodity_table_lookup (tbl, GNC_COMMODITY_NS_CURRENCY, "BRL");
        m_usd = gnc_commodity_table_lookup (tbl, GNC_COMMODITY_NS_CURRENCY, "USD");
        ASSERT_NE (m_brl, nullptr);
        ASSERT_NE (m_usd, nullptr);

        auto insert_comm = [this, tbl](const char *fullname, const char *ns,
                                       const char *mnemonic, int fraction) {
            gnc_commodity *c = gnc_commodity_new (
                m_book, fullname, ns, mnemonic, "", fraction);
            return gnc_commodity_table_insert (tbl, c);
        };

        m_b5p211 = insert_comm ("It Now B5P211 ETF", "B3", "B5P211.SA", 100);
        m_btc = insert_comm ("Bitcoin", "CRYPTO", "BTC", 100000000);
        m_petr4 = insert_comm ("Petrobras", "B3", "PETR4.SA", 100);
        m_old = insert_comm ("Old Trading Asset", "B3", "OLD.SA", 100);
        m_custom = insert_comm ("Fundo Privado", "FUND", "CUSTOM_NO_DOT", 100);
        m_ipca29 = insert_comm ("Tesouro IPCA+ 2029", "Tesouro Direto", "Tesouro IPCA+ 2029", 100);
        m_renda50 = insert_comm ("Tesouro Renda+ Aposentadoria Extra 2050",
                                 "Tesouro Direto",
                                 "Tesouro Renda+ Aposentadoria Extra 2050", 100);

        Account *equity = create_account (m_book, m_root, "Equity", ACCT_TYPE_EQUITY, m_brl);
        Account *bank_brl = create_account (m_book, m_root, "Bank BRL", ACCT_TYPE_BANK, m_brl);
        Account *bank_usd = create_account (m_book, m_root, "Bank USD", ACCT_TYPE_BANK, m_usd);
        Account *etf1 = create_account (m_book, m_root, "ETF Broker 1", ACCT_TYPE_STOCK, m_b5p211);
        Account *etf2 = create_account (m_book, m_root, "ETF Broker 2", ACCT_TYPE_STOCK, m_b5p211);
        Account *btc_acc = create_account (m_book, m_root, "BTC Wallet", ACCT_TYPE_ASSET, m_btc);
        Account *petr_acc = create_account (m_book, m_root, "PETR4 Sold", ACCT_TYPE_STOCK, m_petr4);
        Account *trading_acc = create_account (m_book, m_root, "Trading OLD", ACCT_TYPE_TRADING, m_old);
        Account *income_acc = create_account (m_book, m_root, "Income Div", ACCT_TYPE_INCOME, m_custom);
        Account *fund_acc = create_account (m_book, m_root, "Private Fund", ACCT_TYPE_MUTUAL, m_custom);
        Account *ipca_acc = create_account (m_book, m_root, "TD IPCA 2029", ACCT_TYPE_ASSET, m_ipca29);
        Account *renda_acc = create_account (m_book, m_root, "TD Renda 2050", ACCT_TYPE_ASSET, m_renda50);

        // Book currency bank account (BRL) -> ignored
        add_split_txn (m_book, equity, bank_brl, m_brl,
                       gnc_numeric_create (500000, 100), gnc_numeric_create (500000, 100));
        // Foreign currency bank account (USD = 100.00) -> FX target
        add_split_txn (m_book, equity, bank_usd, m_brl,
                       gnc_numeric_create (10000, 100), gnc_numeric_create (58000, 100));
        // Duplicate active commodity across two accounts (10 + 5 B5P211.SA) -> single target
        add_split_txn (m_book, equity, etf1, m_brl,
                       gnc_numeric_create (1000, 100), gnc_numeric_create (115000, 100));
        add_split_txn (m_book, equity, etf2, m_brl,
                       gnc_numeric_create (500, 100), gnc_numeric_create (57500, 100));
        // Active BTC (0.5 BTC) -> BTC-USD target
        add_split_txn (m_book, equity, btc_acc, m_brl,
                       gnc_numeric_create (50000000, 100000000), gnc_numeric_create (15000000, 100));
        // Sold-out PETR4.SA (buy 10 then sell 10 => balance 0) -> excluded
        add_split_txn (m_book, equity, petr_acc, m_brl,
                       gnc_numeric_create (1000, 100), gnc_numeric_create (30000, 100));
        add_split_txn (m_book, equity, petr_acc, m_brl,
                       gnc_numeric_create (-1000, 100), gnc_numeric_create (-30000, 100));
        // Ignored account types -> excluded
        add_split_txn (m_book, equity, trading_acc, m_brl,
                       gnc_numeric_create (1000, 100), gnc_numeric_create (10000, 100));
        add_split_txn (m_book, equity, income_acc, m_brl,
                       gnc_numeric_create (1000, 100), gnc_numeric_create (10000, 100));
        // Active commodity without '.' and not Tesouro Direto -> skipped
        add_split_txn (m_book, equity, fund_acc, m_brl,
                       gnc_numeric_create (1000, 100), gnc_numeric_create (10000, 100));
        // Active Tesouro Direto bonds -> tesouro targets
        add_split_txn (m_book, equity, ipca_acc, m_brl,
                       gnc_numeric_create (200, 100), gnc_numeric_create (798000, 100));
        add_split_txn (m_book, equity, renda_acc, m_brl,
                       gnc_numeric_create (500, 100), gnc_numeric_create (229860, 100));
    }

    void TearDown () override
    {
        if (m_book)
            qof_book_destroy (m_book);
        qof_close ();
    }
};

TEST_F (PriceUpdaterTest, CollectActivePriceTargets)
{
    ActiveTargetsResult res = gnc_collect_active_price_targets (m_book, m_brl);

    EXPECT_EQ (res.targets.size (), 5u);
    EXPECT_EQ (res.skipped.size (), 1u);
    EXPECT_EQ (res.skipped[0].symbol, "CUSTOM_NO_DOT");
    EXPECT_EQ (res.skipped[0].ns, "FUND");

    std::unordered_map<std::string, const PriceTarget*> by_sym;
    for (const auto &t : res.targets)
        by_sym.emplace (t.symbol, &t);

    ASSERT_EQ (by_sym.count ("B5P211.SA"), 1u);
    EXPECT_EQ (by_sym["B5P211.SA"]->kind, PriceTargetKind::YAHOO);
    EXPECT_EQ (by_sym["B5P211.SA"]->query, "B5P211.SA");

    ASSERT_EQ (by_sym.count ("BTC"), 1u);
    EXPECT_EQ (by_sym["BTC"]->kind, PriceTargetKind::YAHOO);
    EXPECT_EQ (by_sym["BTC"]->query, "BTC-USD");

    ASSERT_EQ (by_sym.count ("USD"), 1u);
    EXPECT_EQ (by_sym["USD"]->kind, PriceTargetKind::FX);
    EXPECT_EQ (by_sym["USD"]->query, "USDBRL=X");
    EXPECT_EQ (by_sym["USD"]->target_currency, "BRL");

    ASSERT_EQ (by_sym.count ("Tesouro IPCA+ 2029"), 1u);
    EXPECT_EQ (by_sym["Tesouro IPCA+ 2029"]->kind, PriceTargetKind::TESOURO);

    ASSERT_EQ (by_sym.count ("Tesouro Renda+ Aposentadoria Extra 2050"), 1u);
    EXPECT_EQ (by_sym["Tesouro Renda+ Aposentadoria Extra 2050"]->kind,
               PriceTargetKind::TESOURO);

    EXPECT_EQ (by_sym.count ("PETR4.SA"), 0u);
    EXPECT_EQ (by_sym.count ("OLD.SA"), 0u);
    EXPECT_EQ (by_sym.count ("BRL"), 0u);

    std::string req = gnc_build_price_fetch_request_json (res, "BRL");
    EXPECT_NE (req.find ("\"book_currency\":\"BRL\""), std::string::npos);
    EXPECT_NE (req.find ("\"query\":\"BTC-USD\""), std::string::npos);
}

TEST_F (PriceUpdaterTest, ApplyPriceResponseAndOverwrite)
{
    ActiveTargetsResult res = gnc_collect_active_price_targets (m_book, m_brl);
    ASSERT_EQ (res.targets.size (), 5u);

    // Pre-insert an existing highest-precedence (EDIT_DLG) price on 2026-11-25 for B5P211.SA
    GNCPriceDB *pdb = gnc_pricedb_get_db (m_book);
    time64 day_25 = static_cast<time64> (GncDateTime (GncDate ("2026-11-25", "y-m-d"), DayPart::neutral));
    time64 day_19 = static_cast<time64> (GncDateTime (GncDate ("2026-11-19", "y-m-d"), DayPart::neutral));

    GNCPrice *old_price = gnc_price_create (m_book);
    gnc_price_begin_edit (old_price);
    gnc_price_set_commodity (old_price, m_b5p211);
    gnc_price_set_currency (old_price, m_brl);
    gnc_price_set_time64 (old_price, day_25);
    gnc_price_set_source (old_price, PRICE_SOURCE_EDIT_DLG);
    gnc_price_set_typestr (old_price, PRICE_TYPE_LAST);
    gnc_price_set_value (old_price, gnc_numeric_create (10000, 100));
    gnc_price_commit_edit (old_price);
    ASSERT_TRUE (gnc_pricedb_add_price (pdb, old_price));

    // Build mock response using dynamic target IDs
    auto find_id = [&res](const std::string &sym) -> std::string {
        for (const auto &t : res.targets)
        {
            if (t.symbol == sym)
                return t.id;
        }
        return "";
    };

    std::string response_json =
        "{\"results\":["
        "{\"id\":\"" + find_id ("B5P211.SA") + "\",\"status\":\"ok\",\"date\":\"2026-11-25\",\"price\":\"115.02\",\"currency\":\"BRL\"},"
        "{\"id\":\"" + find_id ("BTC") + "\",\"status\":\"ok\",\"date\":\"2026-11-25\",\"price\":\"60161.42\",\"currency\":\"USD\"},"
        "{\"id\":\"" + find_id ("Tesouro IPCA+ 2029") + "\",\"status\":\"ok\",\"date\":\"2026-11-19\",\"price\":\"3990.87\",\"currency\":\"BRL\"},"
        "{\"id\":\"" + find_id ("Tesouro Renda+ Aposentadoria Extra 2050") + "\",\"status\":\"ok\",\"date\":\"2026-11-19\",\"price\":\"459.72\",\"currency\":\"BRL\"},"
        "{\"id\":\"" + find_id ("USD") + "\",\"status\":\"ok\",\"date\":\"2026-11-25\",\"price\":\"5.8232\",\"currency\":\"BRL\"}"
        "],\"extra_fx\":[]}";

    PriceUpdateSummary summary = gnc_apply_price_fetch_response (m_book, res.targets, response_json);
    EXPECT_EQ (summary.updated, 5u);
    EXPECT_EQ (summary.failed, 0u);

    // Confirm B5P211.SA was overwritten (exact count 1 and value 115.02)
    PriceList *b5_list = gnc_pricedb_get_prices (pdb, m_b5p211, m_brl);
    EXPECT_EQ (g_list_length (b5_list), 1u);
    gnc_price_list_destroy (b5_list);

    GNCPrice *p_b5 = gnc_pricedb_lookup_day_t64 (pdb, m_b5p211, m_brl, day_25);
    ASSERT_NE (p_b5, nullptr);
    EXPECT_TRUE (gnc_numeric_equal (gnc_price_get_value (p_b5), gnc_numeric_create (11502, 100)));
    gnc_price_unref (p_b5);

    // Confirm BTC stored in USD
    GNCPrice *p_btc = gnc_pricedb_lookup_day_t64 (pdb, m_btc, m_usd, day_25);
    ASSERT_NE (p_btc, nullptr);
    EXPECT_TRUE (gnc_numeric_equal (gnc_price_get_value (p_btc), gnc_numeric_create (6016142, 100)));
    gnc_price_unref (p_btc);

    // Confirm USD/BRL FX stored
    GNCPrice *p_usd = gnc_pricedb_lookup_day_t64 (pdb, m_usd, m_brl, day_25);
    ASSERT_NE (p_usd, nullptr);
    EXPECT_TRUE (gnc_numeric_equal (gnc_price_get_value (p_usd), gnc_numeric_create (58232, 10000)));
    gnc_price_unref (p_usd);

    // Confirm Tesouro bonds stored in BRL on 2026-11-19
    GNCPrice *p_ipca = gnc_pricedb_lookup_day_t64 (pdb, m_ipca29, m_brl, day_19);
    ASSERT_NE (p_ipca, nullptr);
    EXPECT_TRUE (gnc_numeric_equal (gnc_price_get_value (p_ipca), gnc_numeric_create (399087, 100)));
    gnc_price_unref (p_ipca);

    GNCPrice *p_renda = gnc_pricedb_lookup_day_t64 (pdb, m_renda50, m_brl, day_19);
    ASSERT_NE (p_renda, nullptr);
    EXPECT_TRUE (gnc_numeric_equal (gnc_price_get_value (p_renda), gnc_numeric_create (45972, 100)));
    gnc_price_unref (p_renda);

    // Also test extra_fx path when BTC-USD adds USD -> BRL automatically
    std::vector<PriceTarget> btc_only_targets;
    for (const auto &t : res.targets)
    {
        if (t.symbol == "BTC")
            btc_only_targets.push_back (t);
    }
    std::string extra_fx_json =
        "{\"results\":[{\"id\":\"" + btc_only_targets[0].id +
        "\",\"status\":\"ok\",\"date\":\"2026-11-26\",\"price\":\"61000.00\",\"currency\":\"USD\"}],"
        "\"extra_fx\":[{\"from_currency\":\"USD\",\"to_currency\":\"BRL\",\"status\":\"ok\",\"date\":\"2026-11-26\",\"price\":\"5.8500\"}]}";
    PriceUpdateSummary s2 = gnc_apply_price_fetch_response (m_book, btc_only_targets, extra_fx_json);
    EXPECT_EQ (s2.updated, 2u);
    EXPECT_EQ (s2.failed, 0u);
    time64 day_26 = static_cast<time64> (GncDateTime (GncDate ("2026-11-26", "y-m-d"), DayPart::neutral));
    GNCPrice *p_usd_26 = gnc_pricedb_lookup_day_t64 (pdb, m_usd, m_brl, day_26);
    ASSERT_NE (p_usd_26, nullptr);
    EXPECT_TRUE (gnc_numeric_equal (gnc_price_get_value (p_usd_26), gnc_numeric_create (585, 100)));
    gnc_price_unref (p_usd_26);
}

} // namespace
