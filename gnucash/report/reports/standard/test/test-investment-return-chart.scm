;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-investment-return-chart.scm: Tests for Investment Return
;; Over Time (TWR) chart report.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (gnucash core-utils))
(use-modules (gnucash engine))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (gnucash report stylesheets plain))
(use-modules (gnucash reports standard investment-return-chart))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-64))
(use-modules (tests test-engine-extras))
(use-modules (tests srfi64-extras))
(use-modules (tests test-report-extras))

(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-investment-return-chart")
  (qof-date-format-set QOF-DATE-FORMAT-ISO)
  (null-test)
  (stock-twr-with-contribution-test)
  (asset-interest-twr-test)
  (parent-weighted-twr-test)
  (internal-transfer-test)
  (bank-account-twr-test)
  (stale-price-large-deposit-test)
  (missing-initial-price-test)
  (deep-leaf-stock-selection-test)
  (zero-to-first-deposit-stale-price-test)
  (excluded-income-expense-accounts-test)
  (benchmark-cdi-and-ticker-test)
  (report-action-anchor-test)
  (csv-export-test)
  (test-end "test-investment-return-chart"))

(define (set-option! options section name value)
  (if (gnc-lookup-option (gnc:optiondb options) section name)
      (gnc-set-option (gnc:optiondb options) section name value)
      (test-assert (format #f "wrong-option ~a ~a" section name) #f)))

(define (teardown)
  (gnc-clear-current-session))

(define (null-test)
  (test-group-with-cleanup "null-test"
    (let* ((book (gnc-get-current-book))
           (options (gnc:make-report-options investment-return-chart-uuid)))
      (test-assert "null-test renders"
        (gnc:options->sxml investment-return-chart-uuid options
                           "test-investment-return-chart" "null-test"
                           #:strip-tag "script")))
    (teardown)))

(define (setup-stock-contribution-env)
  (let* ((env (create-test-env))
         (book (gnc-get-current-book))
         (USD (gnc-default-report-currency))
         (comm-table (gnc-commodity-table-get-table book))
         (AAPL (gnc-commodity-new book "Apple" "NASDAQ" "AAPL" "" 1))
         (structure
          (list "Assets" (list (cons 'type ACCT-TYPE-ASSET))
                (list "Bank" (list (cons 'type ACCT-TYPE-BANK)))
                (list "Broker"
                      (list "AAPL" (list (cons 'type ACCT-TYPE-STOCK))))
                (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY)))))
         (account-alist (env-create-account-structure-alist env structure))
         (bank (cdr (assoc "Bank" account-alist)))
         (equity (cdr (assoc "Equity" account-alist)))
         (aapl (cdr (assoc "AAPL" account-alist))))
    (gnc-commodity-table-insert comm-table AAPL)
    (xaccAccountSetCommodity aapl AAPL)

    (env-transfer env 01 01 2020 equity bank 10000)
    ;; Day 1: Buy 10 AAPL @ $100 ($1000)
    (env-create-multisplit-transaction
     env 01 01 2020
     (list (vector bank -1000 -1000)
           (vector aapl  1000    10))
     #:description "buy 10 AAPL @ $100")
    ;; Day 2: Price is $110 (+10%), buy 10 more AAPL @ $110 ($1100 deposit)
    (env-create-multisplit-transaction
     env 02 01 2020
     (list (vector bank -1100 -1100)
           (vector aapl  1100    10))
     #:description "buy 10 AAPL @ $110")
    ;; Day 3: Price rises to $121 (+10% on total -> cumulative TWR +21.00%)
    (gnc-pricedb-create USD AAPL (gnc-dmy2time64 03 01 2020) 121)
    account-alist))

(define (stock-twr-with-contribution-test)
  (test-group-with-cleanup "stock-twr-with-contribution"
    (let* ((account-alist (setup-stock-contribution-env))
           (aapl (cdr (assoc "AAPL" account-alist)))
           (options (gnc:make-report-options investment-return-chart-uuid)))
      (set-option! options gnc:pagename-general "Start Date"
                   (cons 'absolute (gnc-dmy2time64 01 01 2020)))
      (set-option! options gnc:pagename-general "End Date"
                   (cons 'absolute (gnc-dmy2time64 03 01 2020)))
      (set-option! options gnc:pagename-general "Step Size" 'DayDelta)
      (set-option! options gnc:pagename-general "Price Source" 'pricedb-before)
      (set-option! options gnc:pagename-general "Report's currency"
                   (gnc-default-report-currency))
      (set-option! options gnc:pagename-accounts "Accounts" (list aapl))
      (set-option! options gnc:pagename-accounts "Levels of Subaccounts" 'all)
      (set-option! options gnc:pagename-display "Show table" #t)

      (let ((sxml (gnc:options->sxml investment-return-chart-uuid options
                                     "test-investment-return-chart"
                                     "stock-twr" #:strip-tag "script")))
        (test-equal "headers"
          '("Date" "AAPL")
          (sxml->table-row-col sxml 1 0 #f))
        (test-equal "day 1 0.00%"
          '("2020-01-01" "0.00%")
          (sxml->table-row-col sxml 1 1 #f))
        (test-equal "day 2 10.00%"
          '("2020-01-02" "10.00%")
          (sxml->table-row-col sxml 1 2 #f))
        (test-equal "day 3 21.00%"
          '("2020-01-03" "21.00%")
          (sxml->table-row-col sxml 1 3 #f))))
    (teardown)))

(define (asset-interest-twr-test)
  (test-group-with-cleanup "asset-interest-twr"
    (let* ((env (create-test-env))
           (book (gnc-get-current-book))
           (structure
            (list "Assets" (list (cons 'type ACCT-TYPE-ASSET))
                  (list "Bank" (list (cons 'type ACCT-TYPE-BANK)))
                  (list "BondAsset" (list (cons 'type ACCT-TYPE-ASSET)))
                  (list "Income" (list (cons 'type ACCT-TYPE-INCOME)))
                  (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY)))))
           (account-alist (env-create-account-structure-alist env structure))
           (bank (cdr (assoc "Bank" account-alist)))
           (bond (cdr (assoc "BondAsset" account-alist)))
           (income (cdr (assoc "Income" account-alist)))
           (equity (cdr (assoc "Equity" account-alist)))
           (options (gnc:make-report-options investment-return-chart-uuid)))
      (env-transfer env 01 01 2020 equity bank 5000)
      (env-transfer env 01 01 2020 bank bond 1000)
      (env-transfer env 02 01 2020 income bond 50)
      (env-transfer env 03 01 2020 bank bond 200)

      (set-option! options gnc:pagename-general "Start Date"
                   (cons 'absolute (gnc-dmy2time64 01 01 2020)))
      (set-option! options gnc:pagename-general "End Date"
                   (cons 'absolute (gnc-dmy2time64 03 01 2020)))
      (set-option! options gnc:pagename-general "Step Size" 'DayDelta)
      (set-option! options gnc:pagename-general "Price Source" 'pricedb-before)
      (set-option! options gnc:pagename-accounts "Accounts" (list bond))
      (set-option! options gnc:pagename-accounts "Levels of Subaccounts" 'all)
      (set-option! options gnc:pagename-display "Show table" #t)

      (let ((sxml (gnc:options->sxml investment-return-chart-uuid options
                                     "test-investment-return-chart"
                                     "asset-interest" #:strip-tag "script")))
        (test-equal "day 1 0.00%"
          '("2020-01-01" "0.00%")
          (sxml->table-row-col sxml 1 1 #f))
        (test-equal "day 2 5.00% interest"
          '("2020-01-02" "5.00%")
          (sxml->table-row-col sxml 1 2 #f))
        (test-equal "day 3 5.00% after deposit"
          '("2020-01-03" "5.00%")
          (sxml->table-row-col sxml 1 3 #f))))
    (teardown)))

(define (parent-weighted-twr-test)
  (test-group-with-cleanup "parent-weighted-twr"
    (let* ((env (create-test-env))
           (book (gnc-get-current-book))
           (USD (gnc-default-report-currency))
           (comm-table (gnc-commodity-table-get-table book))
           (STKA (gnc-commodity-new book "Stock A" "NASDAQ" "STKA" "" 1))
           (STKB (gnc-commodity-new book "Stock B" "NASDAQ" "STKB" "" 1))
           (structure
            (list "Investments" (list (cons 'type ACCT-TYPE-ASSET))
                  (list "StockA" (list (cons 'type ACCT-TYPE-STOCK)))
                  (list "StockB" (list (cons 'type ACCT-TYPE-STOCK)))))
           (account-alist (env-create-account-structure-alist env structure))
           (investments (cdr (assoc "Investments" account-alist)))
           (stock-a (cdr (assoc "StockA" account-alist)))
           (stock-b (cdr (assoc "StockB" account-alist)))
           (bank (env-create-root-account env ACCT-TYPE-BANK USD))
           (equity (env-create-root-account env ACCT-TYPE-EQUITY USD)))
      (gnc-commodity-table-insert comm-table STKA)
      (gnc-commodity-table-insert comm-table STKB)
      (xaccAccountSetCommodity stock-a STKA)
      (xaccAccountSetCommodity stock-b STKB)

      (env-transfer env 01 01 2020 equity bank 10000)
      ;; Initial allocation: StockA = $1000 (25%), StockB = $3000 (75%)
      (env-create-multisplit-transaction
       env 01 01 2020
       (list (vector bank -1000 -1000)
             (vector stock-a 1000 10)))
      (env-create-multisplit-transaction
       env 01 01 2020
       (list (vector bank -3000 -3000)
             (vector stock-b 3000 30)))
      ;; Day 2: StockA +20% ($120), StockB 0% ($100) => weighted return = +5.00%
      (gnc-pricedb-create USD STKA (gnc-dmy2time64 02 01 2020) 120)
      (gnc-pricedb-create USD STKB (gnc-dmy2time64 02 01 2020) 100)

      ;; 1) Parent selected directly without selecting children
      (let ((options (gnc:make-report-options investment-return-chart-uuid)))
        (set-option! options gnc:pagename-general "Start Date"
                     (cons 'absolute (gnc-dmy2time64 01 01 2020)))
        (set-option! options gnc:pagename-general "End Date"
                     (cons 'absolute (gnc-dmy2time64 02 01 2020)))
        (set-option! options gnc:pagename-general "Step Size" 'DayDelta)
        (set-option! options gnc:pagename-general "Price Source" 'pricedb-before)
        (set-option! options gnc:pagename-accounts "Accounts" (list investments))
        (set-option! options gnc:pagename-accounts "Levels of Subaccounts" 'all)
        (set-option! options gnc:pagename-display "Show table" #t)

        (let ((sxml (gnc:options->sxml investment-return-chart-uuid options
                                       "test-investment-return-chart"
                                       "parent-only" #:strip-tag "script")))
          (test-equal "parent-only headers"
            '("Date" "Investments")
            (sxml->table-row-col sxml 1 0 #f))
          (test-equal "parent-only day 1"
            '("2020-01-01" "0.00%")
            (sxml->table-row-col sxml 1 1 #f))
          (test-equal "parent-only weighted 5.00%"
            '("2020-01-02" "5.00%")
            (sxml->table-row-col sxml 1 2 #f))))

      ;; 2) Parent + children selected with Levels of Subaccounts = 1
      (let ((options (gnc:make-report-options investment-return-chart-uuid)))
        (set-option! options gnc:pagename-general "Start Date"
                     (cons 'absolute (gnc-dmy2time64 01 01 2020)))
        (set-option! options gnc:pagename-general "End Date"
                     (cons 'absolute (gnc-dmy2time64 02 01 2020)))
        (set-option! options gnc:pagename-general "Step Size" 'DayDelta)
        (set-option! options gnc:pagename-general "Price Source" 'pricedb-before)
        (set-option! options gnc:pagename-accounts "Accounts"
                     (list investments stock-a stock-b))
        (set-option! options gnc:pagename-accounts "Levels of Subaccounts" 1)
        (set-option! options gnc:pagename-display "Show table" #t)

        (let ((sxml (gnc:options->sxml investment-return-chart-uuid options
                                       "test-investment-return-chart"
                                       "parent-level-1" #:strip-tag "script")))
          (test-equal "level-1 weighted 5.00%"
            '("2020-01-02" "5.00%")
            (sxml->table-row-col sxml 1 2 #f))))

      ;; 3) Children + Consolidated Portfolio
      (let ((options (gnc:make-report-options investment-return-chart-uuid)))
        (set-option! options gnc:pagename-general "Start Date"
                     (cons 'absolute (gnc-dmy2time64 01 01 2020)))
        (set-option! options gnc:pagename-general "End Date"
                     (cons 'absolute (gnc-dmy2time64 02 01 2020)))
        (set-option! options gnc:pagename-general "Step Size" 'DayDelta)
        (set-option! options gnc:pagename-general "Price Source" 'pricedb-before)
        (set-option! options gnc:pagename-accounts "Accounts"
                     (list investments stock-a stock-b))
        (set-option! options gnc:pagename-accounts "Levels of Subaccounts" 'all)
        (set-option! options gnc:pagename-display "Show Consolidated Portfolio" #t)
        (set-option! options gnc:pagename-display "Show table" #t)

        (let ((sxml (gnc:options->sxml investment-return-chart-uuid options
                                       "test-investment-return-chart"
                                       "consolidated" #:strip-tag "script")))
          (test-equal "consolidated headers"
            '("Date" "StockA" "StockB" "Total Portfolio")
            (sxml->table-row-col sxml 1 0 #f))
          (test-equal "consolidated day 2"
            '("2020-01-02" "20.00%" "0.00%" "5.00%")
            (sxml->table-row-col sxml 1 2 #f)))))
    (teardown)))

(define (internal-transfer-test)
  (test-group-with-cleanup "internal-transfer"
    (let* ((env (create-test-env))
           (book (gnc-get-current-book))
           (USD (gnc-default-report-currency))
           (comm-table (gnc-commodity-table-get-table book))
           (STKA (gnc-commodity-new book "Stock A" "NASDAQ" "STKA" "" 1))
           (structure
            (list "Investments" (list (cons 'type ACCT-TYPE-ASSET))
                  (list "BrokerCash" (list (cons 'type ACCT-TYPE-ASSET)))
                  (list "StockA" (list (cons 'type ACCT-TYPE-STOCK)))))
           (account-alist (env-create-account-structure-alist env structure))
           (investments (cdr (assoc "Investments" account-alist)))
           (cash (cdr (assoc "BrokerCash" account-alist)))
           (stock-a (cdr (assoc "StockA" account-alist)))
           (bank (env-create-root-account env ACCT-TYPE-BANK USD))
           (equity (env-create-root-account env ACCT-TYPE-EQUITY USD))
           (options (gnc:make-report-options investment-return-chart-uuid)))
      (gnc-commodity-table-insert comm-table STKA)
      (xaccAccountSetCommodity stock-a STKA)

      (env-transfer env 01 01 2020 equity bank 5000)
      ;; Day 1: External deposit $1000 into BrokerCash
      (env-transfer env 01 01 2020 bank cash 1000)
      ;; Day 2: Internal transfer $500 from BrokerCash to buy 5 STKA @ $100
      (env-create-multisplit-transaction
       env 02 01 2020
       (list (vector cash -500 -500)
             (vector stock-a 500 5)))
      ;; Day 3: STKA rises to $110 ($550 stock + $500 cash = $1050 => +5.00%)
      (gnc-pricedb-create USD STKA (gnc-dmy2time64 03 01 2020) 110)

      (set-option! options gnc:pagename-general "Start Date"
                   (cons 'absolute (gnc-dmy2time64 01 01 2020)))
      (set-option! options gnc:pagename-general "End Date"
                   (cons 'absolute (gnc-dmy2time64 03 01 2020)))
      (set-option! options gnc:pagename-general "Step Size" 'DayDelta)
      (set-option! options gnc:pagename-general "Price Source" 'pricedb-before)
      (set-option! options gnc:pagename-accounts "Accounts" (list investments))
      (set-option! options gnc:pagename-accounts "Levels of Subaccounts" 'all)
      (set-option! options gnc:pagename-display "Show table" #t)

      (let ((sxml (gnc:options->sxml investment-return-chart-uuid options
                                     "test-investment-return-chart"
                                     "internal-transfer" #:strip-tag "script")))
        (test-equal "day 1 0.00%"
          '("2020-01-01" "0.00%")
          (sxml->table-row-col sxml 1 1 #f))
        (test-equal "day 2 internal transfer stays 0.00%"
          '("2020-01-02" "0.00%")
          (sxml->table-row-col sxml 1 2 #f))
        (test-equal "day 3 5.00%"
          '("2020-01-03" "5.00%")
          (sxml->table-row-col sxml 1 3 #f))))
    (teardown)))

(define (bank-account-twr-test)
  (test-group-with-cleanup "bank-account-twr"
    (let* ((env (create-test-env))
           (book (gnc-get-current-book))
           (USD (gnc-default-report-currency))
           (structure
            (list "BankFolder" (list (cons 'type ACCT-TYPE-BANK))
                  (list "SavingsCDB" (list (cons 'type ACCT-TYPE-BANK)))))
           (account-alist (env-create-account-structure-alist env structure))
           (bank-folder (cdr (assoc "BankFolder" account-alist)))
           (savings (cdr (assoc "SavingsCDB" account-alist)))
           (wallet (env-create-root-account env ACCT-TYPE-CASH USD))
           (income (env-create-root-account env ACCT-TYPE-INCOME USD))
           (equity (env-create-root-account env ACCT-TYPE-EQUITY USD))
           (options (gnc:make-report-options investment-return-chart-uuid)))
      (env-transfer env 01 01 2020 equity wallet 5000)
      (env-transfer env 01 01 2020 wallet savings 1000)
      (env-transfer env 02 01 2020 income savings 50)
      (env-transfer env 03 01 2020 wallet savings 500)

      (set-option! options gnc:pagename-general "Start Date"
                   (cons 'absolute (gnc-dmy2time64 01 01 2020)))
      (set-option! options gnc:pagename-general "End Date"
                   (cons 'absolute (gnc-dmy2time64 03 01 2020)))
      (set-option! options gnc:pagename-general "Step Size" 'DayDelta)
      (set-option! options gnc:pagename-general "Price Source" 'pricedb-before)
      (set-option! options gnc:pagename-accounts "Accounts" (list bank-folder))
      (set-option! options gnc:pagename-accounts "Levels of Subaccounts" 'all)
      (set-option! options gnc:pagename-display "Show table" #t)

      (let ((sxml (gnc:options->sxml investment-return-chart-uuid options
                                     "test-investment-return-chart"
                                     "bank-account" #:strip-tag "script")))
        (test-equal "bank headers"
          '("Date" "BankFolder")
          (sxml->table-row-col sxml 1 0 #f))
        (test-equal "bank day 1 0.00%"
          '("2020-01-01" "0.00%")
          (sxml->table-row-col sxml 1 1 #f))
        (test-equal "bank day 2 5.00% interest"
          '("2020-01-02" "5.00%")
          (sxml->table-row-col sxml 1 2 #f))
        (test-equal "bank day 3 5.00% after deposit"
          '("2020-01-03" "5.00%")
          (sxml->table-row-col sxml 1 3 #f))))
    (teardown)))

(define (stale-price-large-deposit-test)
  (test-group-with-cleanup "stale-price-large-deposit"
    (let* ((env (create-test-env))
           (book (gnc-get-current-book))
           (USD (gnc-default-report-currency))
           (comm-table (gnc-commodity-table-get-table book))
           (AAPL (gnc-commodity-new book "Apple" "NASDAQ" "AAPL" "" 1))
           (structure
            (list "Assets" (list (cons 'type ACCT-TYPE-ASSET))
                  (list "Bank" (list (cons 'type ACCT-TYPE-BANK)))
                  (list "Broker"
                        (list "AAPL" (list (cons 'type ACCT-TYPE-STOCK))))
                  (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY)))))
           (account-alist (env-create-account-structure-alist env structure))
           (bank (cdr (assoc "Bank" account-alist)))
           (equity (cdr (assoc "Equity" account-alist)))
           (aapl (cdr (assoc "AAPL" account-alist)))
           (options (gnc:make-report-options investment-return-chart-uuid)))
      (gnc-commodity-table-insert comm-table AAPL)
      (xaccAccountSetCommodity aapl AAPL)

      (env-transfer env 01 01 2020 equity bank 50000)
      ;; Day 1: Buy 10 AAPL @ $100 (adds $100 to PriceDB)
      (env-create-multisplit-transaction
       env 01 01 2020
       (list (vector bank -1000 -1000)
             (vector aapl  1000    10))
       #:pricedb? #t)
      ;; Day 2: Buy 100 AAPL @ $105 without updating PriceDB (stale $100)
      (env-create-multisplit-transaction
       env 02 01 2020
       (list (vector bank -10500 -10500)
             (vector aapl  10500    100))
       #:pricedb? #f)
      ;; Day 3: PriceDB updates to $110 (+10%)
      (gnc-pricedb-create USD AAPL (gnc-dmy2time64 03 01 2020) 110)

      (set-option! options gnc:pagename-general "Start Date"
                   (cons 'absolute (gnc-dmy2time64 01 01 2020)))
      (set-option! options gnc:pagename-general "End Date"
                   (cons 'absolute (gnc-dmy2time64 03 01 2020)))
      (set-option! options gnc:pagename-general "Step Size" 'DayDelta)
      (set-option! options gnc:pagename-general "Price Source" 'pricedb-before)
      (set-option! options gnc:pagename-accounts "Accounts" (list aapl))
      (set-option! options gnc:pagename-accounts "Levels of Subaccounts" 'all)
      (set-option! options gnc:pagename-display "Show table" #t)

      (let ((sxml (gnc:options->sxml investment-return-chart-uuid options
                                     "test-investment-return-chart"
                                     "stale-price" #:strip-tag "script")))
        (test-equal "stale price day 1 0.00%"
          '("2020-01-01" "0.00%")
          (sxml->table-row-col sxml 1 1 #f))
        (test-equal "stale price day 2 stays 0.00% (not -50.00%)"
          '("2020-01-02" "0.00%")
          (sxml->table-row-col sxml 1 2 #f))
        (test-equal "stale price day 3 10.00%"
          '("2020-01-03" "10.00%")
          (sxml->table-row-col sxml 1 3 #f))))
    (teardown)))

(define (missing-initial-price-test)
  (test-group-with-cleanup "missing-initial-price"
    (let* ((env (create-test-env))
           (book (gnc-get-current-book))
           (USD (gnc-default-report-currency))
           (comm-table (gnc-commodity-table-get-table book))
           (AAPL (gnc-commodity-new book "Apple" "NASDAQ" "AAPL" "" 1))
           (structure
            (list "Assets" (list (cons 'type ACCT-TYPE-ASSET))
                  (list "Bank" (list (cons 'type ACCT-TYPE-BANK)))
                  (list "Broker"
                        (list "AAPL" (list (cons 'type ACCT-TYPE-STOCK))))
                  (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY)))))
           (account-alist (env-create-account-structure-alist env structure))
           (bank (cdr (assoc "Bank" account-alist)))
           (equity (cdr (assoc "Equity" account-alist)))
           (aapl (cdr (assoc "AAPL" account-alist)))
           (options (gnc:make-report-options investment-return-chart-uuid)))
      (gnc-commodity-table-insert comm-table AAPL)
      (xaccAccountSetCommodity aapl AAPL)

      (env-transfer env 01 01 2020 equity bank 5000)
      ;; Day 1: Buy 10 AAPL without any PriceDB price
      (env-create-multisplit-transaction
       env 01 01 2020
       (list (vector bank -1000 -1000)
             (vector aapl  1000    10))
       #:pricedb? #f)
      ;; Day 2: PriceDB gets $100; Day 3: $110 (+10%)
      (gnc-pricedb-create USD AAPL (gnc-dmy2time64 02 01 2020) 100)
      (gnc-pricedb-create USD AAPL (gnc-dmy2time64 03 01 2020) 110)

      (set-option! options gnc:pagename-general "Start Date"
                   (cons 'absolute (gnc-dmy2time64 01 01 2020)))
      (set-option! options gnc:pagename-general "End Date"
                   (cons 'absolute (gnc-dmy2time64 03 01 2020)))
      (set-option! options gnc:pagename-general "Step Size" 'DayDelta)
      (set-option! options gnc:pagename-general "Price Source" 'pricedb-before)
      (set-option! options gnc:pagename-accounts "Accounts" (list aapl))
      (set-option! options gnc:pagename-accounts "Levels of Subaccounts" 'all)
      (set-option! options gnc:pagename-display "Show table" #t)

      (let ((sxml (gnc:options->sxml investment-return-chart-uuid options
                                     "test-investment-return-chart"
                                     "missing-initial-price" #:strip-tag "script")))
        (test-equal "missing initial price day 1 0.00%"
          '("2020-01-01" "0.00%")
          (sxml->table-row-col sxml 1 1 #f))
        (test-equal "missing initial price day 2 0.00% (not -100%)"
          '("2020-01-02" "0.00%")
          (sxml->table-row-col sxml 1 2 #f))
        (test-equal "missing initial price day 3 10.00%"
          '("2020-01-03" "10.00%")
          (sxml->table-row-col sxml 1 3 #f))))
    (teardown)))

(define (deep-leaf-stock-selection-test)
  (test-group-with-cleanup "deep-leaf-stock-selection"
    (let* ((env (create-test-env))
           (book (gnc-get-current-book))
           (USD (gnc-default-report-currency))
           (comm-table (gnc-commodity-table-get-table book))
           (PETR4 (gnc-commodity-new book "Petrobras" "B3" "PETR4" "" 1))
           (structure
            (list "Assets" (list (cons 'type ACCT-TYPE-ASSET))
                  (list "Bank" (list (cons 'type ACCT-TYPE-BANK)))
                  (list "Investments" (list (cons 'type ACCT-TYPE-ASSET))
                        (list "Stocks" (list (cons 'type ACCT-TYPE-ASSET))
                              (list "PETR4" (list (cons 'type ACCT-TYPE-STOCK)))))
                  (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY)))))
           (account-alist (env-create-account-structure-alist env structure))
           (bank (cdr (assoc "Bank" account-alist)))
           (equity (cdr (assoc "Equity" account-alist)))
           (petr4 (cdr (assoc "PETR4" account-alist)))
           (options (gnc:make-report-options investment-return-chart-uuid)))
      (gnc-commodity-table-insert comm-table PETR4)
      (xaccAccountSetCommodity petr4 PETR4)

      (env-transfer env 01 01 2020 equity bank 5000)
      (env-create-multisplit-transaction
       env 01 01 2020
       (list (vector bank -1000 -1000)
             (vector petr4 1000   100)))
      (gnc-pricedb-create USD PETR4 (gnc-dmy2time64 02 01 2020) 11)

      ;; Select ONLY the depth-4 leaf STOCK account, leaving Levels of Subaccounts at default (2)
      (set-option! options gnc:pagename-general "Start Date"
                   (cons 'absolute (gnc-dmy2time64 01 01 2020)))
      (set-option! options gnc:pagename-general "End Date"
                   (cons 'absolute (gnc-dmy2time64 02 01 2020)))
      (set-option! options gnc:pagename-general "Step Size" 'DayDelta)
      (set-option! options gnc:pagename-general "Price Source" 'pricedb-before)
      (set-option! options gnc:pagename-accounts "Accounts" (list petr4))
      (set-option! options gnc:pagename-display "Show table" #t)

      (let ((sxml (gnc:options->sxml investment-return-chart-uuid options
                                     "test-investment-return-chart"
                                     "deep-leaf-stock" #:strip-tag "script")))
        (test-equal "deep leaf headers"
          '("Date" "PETR4")
          (sxml->table-row-col sxml 1 0 #f))
        (test-equal "deep leaf day 1 0.00%"
          '("2020-01-01" "0.00%")
          (sxml->table-row-col sxml 1 1 #f))
        (test-equal "deep leaf day 2 10.00%"
          '("2020-01-02" "10.00%")
          (sxml->table-row-col sxml 1 2 #f))))
    (teardown)))

(define (zero-to-first-deposit-stale-price-test)
  (test-group-with-cleanup "zero-to-first-deposit-stale-price"
    (let* ((env (create-test-env))
           (book (gnc-get-current-book))
           (USD (gnc-default-report-currency))
           (comm-table (gnc-commodity-table-get-table book))
           (AAPL (gnc-commodity-new book "Apple" "NASDAQ" "AAPL" "" 1))
           (structure
            (list "Assets" (list (cons 'type ACCT-TYPE-ASSET))
                  (list "Bank" (list (cons 'type ACCT-TYPE-BANK)))
                  (list "Broker"
                        (list "AAPL" (list (cons 'type ACCT-TYPE-STOCK))))
                  (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY)))))
           (account-alist (env-create-account-structure-alist env structure))
           (bank (cdr (assoc "Bank" account-alist)))
           (equity (cdr (assoc "Equity" account-alist)))
           (aapl (cdr (assoc "AAPL" account-alist)))
           (options (gnc:make-report-options investment-return-chart-uuid)))
      (gnc-commodity-table-insert comm-table AAPL)
      (xaccAccountSetCommodity aapl AAPL)

      (env-transfer env 01 01 2020 equity bank 5000)
      ;; Day 1: Account has 0 balance while PriceDB already has an older price $90
      (gnc-pricedb-create USD AAPL (gnc-dmy2time64 01 01 2020) 90)
      ;; Day 2: First purchase of 10 AAPL @ $100 (CF = $1000) without updating PriceDB
      (env-create-multisplit-transaction
       env 02 01 2020
       (list (vector bank -1000 -1000)
             (vector aapl  1000    10))
       #:pricedb? #f)
      ;; Day 3: PriceDB updates to $102 (+2.00% over $100 cost basis)
      (gnc-pricedb-create USD AAPL (gnc-dmy2time64 03 01 2020) 102)

      (set-option! options gnc:pagename-general "Start Date"
                   (cons 'absolute (gnc-dmy2time64 01 01 2020)))
      (set-option! options gnc:pagename-general "End Date"
                   (cons 'absolute (gnc-dmy2time64 03 01 2020)))
      (set-option! options gnc:pagename-general "Step Size" 'DayDelta)
      (set-option! options gnc:pagename-general "Price Source" 'pricedb-before)
      (set-option! options gnc:pagename-accounts "Accounts" (list aapl))
      (set-option! options gnc:pagename-accounts "Levels of Subaccounts" 'all)
      (set-option! options gnc:pagename-display "Show table" #t)

      (let ((sxml (gnc:options->sxml investment-return-chart-uuid options
                                     "test-investment-return-chart"
                                     "zero-to-first-deposit" #:strip-tag "script")))
        (test-equal "zero-to-first-deposit day 1 0.00%"
          '("2020-01-01" "0.00%")
          (sxml->table-row-col sxml 1 1 #f))
        (test-equal "zero-to-first-deposit day 2 0.00%"
          '("2020-01-02" "0.00%")
          (sxml->table-row-col sxml 1 2 #f))
        (test-equal "zero-to-first-deposit day 3 2.00% (not 13.33%)"
          '("2020-01-03" "2.00%")
          (sxml->table-row-col sxml 1 3 #f))))
    (teardown)))

(define (excluded-income-expense-accounts-test)
  (test-group-with-cleanup "excluded-income-expense-accounts"
    (let* ((env (create-test-env))
           (book (gnc-get-current-book))
           (structure
            (list "Root" (list (cons 'type ACCT-TYPE-ASSET))
                  (list "Savings" (list (cons 'type ACCT-TYPE-BANK)))
                  (list "Income" (list (cons 'type ACCT-TYPE-INCOME))
                        (list "Interest")
                        (list "Salary"))
                  (list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE))
                        (list "Tax")
                        (list "Groceries"))
                  (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY)))))
           (account-alist (env-create-account-structure-alist env structure))
           (savings (cdr (assoc "Savings" account-alist)))
           (interest (cdr (assoc "Interest" account-alist)))
           (salary (cdr (assoc "Salary" account-alist)))
           (tax (cdr (assoc "Tax" account-alist)))
           (groceries (cdr (assoc "Groceries" account-alist)))
           (equity (cdr (assoc "Equity" account-alist)))
           (options (gnc:make-report-options investment-return-chart-uuid)))
      ;; Day 1: Initial deposit $1000 from Equity -> Savings (0.00%)
      (env-transfer env 01 01 2020 equity savings 1000)
      ;; Day 2: Deselected Income:Salary $1000 -> Savings (external deposit, stays 0.00%, base $2000)
      (env-transfer env 02 01 2020 salary savings 1000)
      ;; Day 3: Selected Income:Interest $100 -> Savings (+5.00% on $2000, base $2100)
      (env-transfer env 03 01 2020 interest savings 100)
      ;; Day 4: Deselected Expenses:Groceries $500 <- Savings (external withdrawal, stays 5.00%, base $1600)
      (env-transfer env 04 01 2020 savings groceries 500)
      ;; Day 5: Selected Expenses:Tax $16 <- Savings (-1.00% fee on $1600 -> 1.05 * 0.99 - 1 = 3.95%)
      (env-transfer env 05 01 2020 savings tax 16)

      (set-option! options gnc:pagename-general "Start Date"
                   (cons 'absolute (gnc-dmy2time64 01 01 2020)))
      (set-option! options gnc:pagename-general "End Date"
                   (cons 'absolute (gnc-dmy2time64 05 01 2020)))
      (set-option! options gnc:pagename-general "Step Size" 'DayDelta)
      (set-option! options gnc:pagename-general "Price Source" 'pricedb-before)
      (set-option! options gnc:pagename-accounts "Accounts" (list savings))
      (set-option! options gnc:pagename-accounts "Levels of Subaccounts" 'all)
      (set-option! options gnc:pagename-accounts "Income/Expense Accounts in Return"
                   (list interest tax))
      (set-option! options gnc:pagename-display "Show table" #t)

      (let ((sxml (gnc:options->sxml investment-return-chart-uuid options
                                     "test-investment-return-chart"
                                     "excluded-inc-exp" #:strip-tag "script")))
        (test-equal "day 1 0.00%"
          '("2020-01-01" "0.00%")
          (sxml->table-row-col sxml 1 1 #f))
        (test-equal "day 2 deselected salary treated as cash flow"
          '("2020-01-02" "0.00%")
          (sxml->table-row-col sxml 1 2 #f))
        (test-equal "day 3 selected interest +5.00%"
          '("2020-01-03" "5.00%")
          (sxml->table-row-col sxml 1 3 #f))
        (test-equal "day 4 deselected groceries treated as withdrawal"
          '("2020-01-04" "5.00%")
          (sxml->table-row-col sxml 1 4 #f))
        (test-equal "day 5 selected tax -1.00% -> 3.95%"
          '("2020-01-05" "3.95%")
          (sxml->table-row-col sxml 1 5 #f))))
    (teardown)))

(define (ensure-dir path)
  (unless (file-exists? path)
    (ensure-dir (dirname path))
    (mkdir path)))

(define (with-temp-file path content thunk)
  (let ((had-prev? (file-exists? path))
        (prev-content #f))
    (when had-prev?
      (set! prev-content
        (call-with-input-file path
          (lambda (p)
            (let lp ((acc '()))
              (let ((c (read-char p)))
                (if (eof-object? c)
                    (list->string (reverse acc))
                    (lp (cons c acc)))))))))
    (dynamic-wind
      (lambda ()
        (ensure-dir (dirname path))
        (call-with-output-file path
          (lambda (p) (display content p))))
      thunk
      (lambda ()
        (if had-prev?
            (call-with-output-file path
              (lambda (p) (display prev-content p)))
            (when (file-exists? path)
              (delete-file path)))))))

(define (benchmark-cdi-and-ticker-test)
  (test-group-with-cleanup "benchmark-cdi-and-ticker"
    (let* ((account-alist (setup-stock-contribution-env))
           (aapl (cdr (assoc "AAPL" account-alist)))
           (bench-dir (gnc-build-userdata-path "benchmarks"))
           (cdi-path (string-append bench-dir "/CDI.csv"))
           (ibov-path (string-append bench-dir "/IBOVESPA.csv"))
           (divo-path (string-append bench-dir "/YAHOO_DIVO11.SA.csv"))
           (cdi-csv (string-append
                     "# fetched: 2019-12-22..2020-01-03\n"
                     "date,value\n"
                     "2020-01-01,10.0\n"
                     "2020-01-02,10.0\n"
                     "2020-01-03,10.0\n"))
           (ibov-csv (string-append
                      "# fetched: 2019-12-22..2020-01-03\n"
                      "date,value\n"
                      "2020-01-01,100000\n"
                      "2020-01-02,105000\n"
                      "2020-01-03,110000\n"))
           (divo-csv (string-append
                      "# fetched: 2019-12-22..2020-01-03\n"
                      "date,value\n"
                      "2020-01-01,50\n"
                      "2020-01-02,51\n"
                      "2020-01-03,55\n"))
           (options (gnc:make-report-options investment-return-chart-uuid)))
      (set-option! options gnc:pagename-general "Start Date"
                   (cons 'absolute (gnc-dmy2time64 01 01 2020)))
      (set-option! options gnc:pagename-general "End Date"
                   (cons 'absolute (gnc-dmy2time64 03 01 2020)))
      (set-option! options gnc:pagename-general "Step Size" 'DayDelta)
      (set-option! options gnc:pagename-general "Price Source" 'pricedb-before)
      (set-option! options gnc:pagename-accounts "Accounts" (list aapl))
      (set-option! options gnc:pagename-accounts "Levels of Subaccounts" 'all)
      (set-option! options gnc:pagename-display "Show table" #t)
      (set-option! options "Benchmarks" "Show CDI" #t)
      (set-option! options "Benchmarks" "Show IBOVESPA" #t)
      (set-option! options "Benchmarks" "Custom Yahoo Finance Tickers" "DIVO11.SA")

      (with-temp-file cdi-path cdi-csv
        (lambda ()
          (with-temp-file ibov-path ibov-csv
            (lambda ()
              (with-temp-file divo-path divo-csv
                (lambda ()
                  (let ((sxml (gnc:options->sxml investment-return-chart-uuid options
                                                 "test-investment-return-chart"
                                                 "benchmark-cdi-ticker"
                                                 #:strip-tag "script")))
                    (test-equal "benchmark headers"
                      '("Date" "AAPL" "CDI" "IBOVESPA" "DIVO11.SA")
                      (sxml->table-row-col sxml 1 0 #f))
                    (test-equal "benchmark day 1 all 0.00%"
                      '("2020-01-01" "0.00%" "0.00%" "0.00%" "0.00%")
                      (sxml->table-row-col sxml 1 1 #f))
                    (test-equal "benchmark day 2"
                      '("2020-01-02" "10.00%" "10.00%" "5.00%" "2.00%")
                      (sxml->table-row-col sxml 1 2 #f))
                    (test-equal "benchmark day 3"
                      '("2020-01-03" "21.00%" "21.00%" "10.00%" "10.00%")
                      (sxml->table-row-col sxml 1 3 #f)))

                  (let* ((template (gnc:find-report-template investment-return-chart-uuid))
                         (report-id (gnc:make-report investment-return-chart-uuid options))
                         (report (gnc-report-find report-id))
                         (export-thunk (gnc:report-template-export-thunk template))
                         (document (export-thunk report 'csv)))
                    (test-assert "benchmark csv has no export error"
                      (not (gnc:html-document-export-error document)))
                    (test-equal "benchmark csv matches series"
                      (string-append
                       "\"Date\",\"AAPL\",\"CDI\",\"IBOVESPA\",\"DIVO11.SA\"\n"
                       "\"2020-01-01\",0.0,0.0,0.0,0.0\n"
                       "\"2020-01-02\",10.0,10.0,5.0,2.0\n"
                       "\"2020-01-03\",21.0,21.0,10.0,10.0")
                      (gnc:html-document-export-string document))))))))))
    (teardown)))

(define (report-action-anchor-test)
  (test-group-with-cleanup "report-action-anchor"
    (let* ((book (gnc-get-current-book))
           (options (gnc:make-report-options investment-return-chart-uuid))
           (report-id (gnc:make-report investment-return-chart-uuid options))
           (report (gnc-report-find report-id))
           (calls 0)
           (url (gnc:make-report-action-anchor
                 report
                 (lambda () (set! calls (1+ calls)))))
           (id-str (substring url (string-contains url "id=")))
           (pipe-pos (string-index id-str #\|))
           (hash-pos (string-index id-str #\# pipe-pos))
           (anchor-id (string->number
                       (substring id-str (1+ pipe-pos)
                                  (or hash-pos (string-length id-str))))))
      (test-equal "first invocation returns src-id"
        report-id
        (gnc:report-get-linked-report report-id anchor-id))
      (test-equal "second invocation returns src-id without re-running thunk"
        report-id
        (gnc:report-get-linked-report report-id anchor-id))
      (test-equal "thunk invoked exactly once"
        1
        calls))
    (teardown)))

(define (csv-export-test)
  (test-group-with-cleanup "csv-export"
    (let* ((account-alist (setup-stock-contribution-env))
           (aapl (cdr (assoc "AAPL" account-alist)))
           (options (gnc:make-report-options investment-return-chart-uuid)))
      (set-option! options gnc:pagename-general "Start Date"
                   (cons 'absolute (gnc-dmy2time64 01 01 2020)))
      (set-option! options gnc:pagename-general "End Date"
                   (cons 'absolute (gnc-dmy2time64 03 01 2020)))
      (set-option! options gnc:pagename-general "Step Size" 'DayDelta)
      (set-option! options gnc:pagename-general "Price Source" 'pricedb-before)
      (set-option! options gnc:pagename-accounts "Accounts" (list aapl))
      (set-option! options gnc:pagename-accounts "Levels of Subaccounts" 'all)

      (let* ((template (gnc:find-report-template investment-return-chart-uuid))
             (report-id (gnc:make-report investment-return-chart-uuid options))
             (report (gnc-report-find report-id))
             (export-thunk (gnc:report-template-export-thunk template))
             (document (export-thunk report 'csv)))
        (test-assert "csv output has no export error"
          (not (gnc:html-document-export-error document)))
        (test-equal "csv output matches TWR series"
          "\"Date\",\"AAPL\"\n\"2020-01-01\",0.0\n\"2020-01-02\",10.0\n\"2020-01-03\",21.0"
          (gnc:html-document-export-string document))))
    (teardown)))
