;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; investment-return-chart.scm : Display a Time-Weighted Return (TWR)
;; line chart for investment accounts over time.
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash reports standard investment-return-chart))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (gnucash json parser))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-13))
(use-modules (srfi srfi-26))
(use-modules (ice-9 format))
(use-modules (ice-9 popen))
(use-modules (ice-9 rdelim))

(export investment-return-chart-uuid)

(define investment-return-chart-uuid "7c4f2a9b1e6d4a83b5c09d2e3f1a6b48")

(define reportname (N_ "Investment Return Over Time"))
(define menuname (N_ "Investment Return Chart"))
(define menutip
  (N_ "Shows a chart with the Time-Weighted Return (TWR) of investments over time"))

(define investment-account-types
  (list ACCT-TYPE-ASSET ACCT-TYPE-BANK ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL))

(define return-account-types
  (list ACCT-TYPE-INCOME ACCT-TYPE-EXPENSE))

(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))
(define optname-stepsize (N_ "Step Size"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))

(define optname-accounts (N_ "Accounts"))
(define optname-levels (N_ "Levels of Subaccounts"))
(define optname-return-accounts (N_ "Income/Expense Accounts in Return"))

(define optname-fullname (N_ "Show long account names"))
(define optname-show-total (N_ "Show Consolidated Portfolio"))
(define optname-show-table (N_ "Show table"))
(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))
(define optname-line-width (N_ "Line Width"))
(define opthelp-line-width (N_ "Set line width in pixels."))
(define optname-y-grid (N_ "Grid"))
(define optname-tooltip-indexed (N_ "Show a grouped (indexed) tooltip"))
(define optname-tooltip-non-zero-only (N_ "Show only non-zero values in a tooltip"))

(define pagename-benchmarks (N_ "Benchmarks"))
(define optname-show-cdi (N_ "Show CDI"))
(define optname-show-ibov (N_ "Show IBOVESPA"))
(define optname-show-sp500 (N_ "Show S&P 500 (BRL)"))
(define optname-custom-tickers (N_ "Custom Yahoo Finance Tickers"))

(define *benchmark-fetch-status* (make-hash-table))

(define (options-generator)
  (let* ((options (gnc-new-optiondb)))

    ;; General tab
    (gnc:options-add-date-interval!
     options gnc:pagename-general
     optname-from-date optname-to-date "a")

    (gnc:options-add-interval-choice!
     options gnc:pagename-general optname-stepsize "b" 'MonthDelta)

    (gnc:options-add-currency!
     options gnc:pagename-general optname-report-currency "c")

    (gnc:options-add-price-source!
     options gnc:pagename-general
     optname-price-source "d" 'pricedb-before)

    ;; Accounts tab
    (gnc-register-account-list-limited-option options
      gnc:pagename-accounts optname-accounts
      "a"
      (N_ "Report on these accounts, if chosen account level allows.")
      (gnc:filter-accountlist-type
       investment-account-types
       (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))
      investment-account-types)

    (gnc:options-add-account-levels!
     options gnc:pagename-accounts optname-levels "c"
     (N_ "Maximum number of levels in the account tree displayed.")
     2)

    (gnc-register-account-list-limited-option options
      gnc:pagename-accounts optname-return-accounts
      "d"
      (N_ "Income and expense accounts whose transactions count toward investment return. Deselected income and expense accounts are treated as external cash flows (deposits or withdrawals).")
      (gnc:filter-accountlist-type
       return-account-types
       (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))
      return-account-types)

    ;; Display tab
    (gnc-register-simple-boolean-option options
      gnc:pagename-display optname-fullname
      "a" (N_ "Show the full account name in legend?") #f)

    (gnc-register-simple-boolean-option options
      gnc:pagename-display optname-show-total
      "b" (N_ "Show a consolidated value-weighted portfolio return series?") #t)

    (gnc-register-simple-boolean-option options
      gnc:pagename-display optname-show-table
      "c" (N_ "Display a table of the selected data.") #f)

    (gnc:options-add-plot-size!
     options gnc:pagename-display
     optname-plot-width optname-plot-height "d"
     (cons 'percent 100.0) (cons 'percent 100.0))

    (gnc-register-number-range-option options
      gnc:pagename-display optname-line-width
      "e" opthelp-line-width
      1.5 0.5 5 0.1)

    (gnc-register-simple-boolean-option options
      gnc:pagename-display optname-y-grid
      "f" (N_ "Add grid lines.") #t)

    (gnc-register-simple-boolean-option options
      gnc:pagename-display optname-tooltip-indexed
      "t1"
      (N_ "Show all items, along the x-axis, in a grouped tooltip. Otherwise - only a single entry, as per cursor's nearest proximity.")
      #t)

    (gnc-register-simple-boolean-option options
      gnc:pagename-display optname-tooltip-non-zero-only
      "t2" (N_ "Show only non-zero values in a tooltip.") #f)

    ;; Benchmarks tab
    (gnc-register-simple-boolean-option options
      pagename-benchmarks optname-show-cdi
      "a" (N_ "Overlay cumulative CDI return (fetched from BCB SGS) on the chart.") #f)

    (gnc-register-simple-boolean-option options
      pagename-benchmarks optname-show-ibov
      "b" (N_ "Overlay IBOVESPA (^BVSP) cumulative return on the chart.") #f)

    (gnc-register-simple-boolean-option options
      pagename-benchmarks optname-show-sp500
      "c" (N_ "Overlay S&P 500 converted to BRL (^GSPC * USDBRL=X) cumulative return on the chart.") #f)

    (gnc-register-string-option options
      pagename-benchmarks optname-custom-tickers
      "d"
      (N_ "Comma-separated Yahoo Finance tickers to compare (e.g. DIVO11.SA, BOVA11.SA).")
      "")

    (gnc:options-set-default-section options gnc:pagename-general)

    options))

(define (dedupe-sorted-numbers lst)
  (let loop ((rest lst) (acc '()))
    (cond
     ((null? rest) (reverse acc))
     ((and (pair? acc) (= (car rest) (car acc)))
      (loop (cdr rest) acc))
     (else
      (loop (cdr rest) (cons (car rest) acc))))))

(define (sanitize-ticker ticker)
  (string-map
   (lambda (c)
     (if (or (and (char>=? c #\A) (char<=? c #\Z))
             (and (char>=? c #\a) (char<=? c #\z))
             (and (char>=? c #\0) (char<=? c #\9))
             (memv c '(#\. #\_ #\-)))
         c
         #\_))
   ticker))

(define (read-all-from-port port)
  (let loop ((lines '()))
    (let ((line (read-line port 'concat)))
      (if (eof-object? line)
          (string-concatenate-reverse lines)
          (loop (cons line lines))))))

(define (read-benchmark-cache cache-path)
  (if (not (file-exists? cache-path))
      (list #f #f '())
      (call-with-input-file cache-path
        (lambda (port)
          (let loop ((f-start #f) (f-end #f) (rows '()))
            (let ((line (read-line port)))
              (cond
               ((eof-object? line)
                (list f-start f-end
                      (sort rows (lambda (a b) (string<? (car a) (car b))))))
               (else
                (let ((trimmed (string-trim-both line)))
                  (cond
                   ((string-prefix? "# fetched:" trimmed)
                    (let* ((range-str (string-trim-both
                                       (substring trimmed (string-length "# fetched:"))))
                           (sep (string-contains range-str "..")))
                      (if sep
                          (loop (substring range-str 0 sep)
                                (substring range-str (+ sep 2))
                                rows)
                          (loop f-start f-end rows))))
                   ((or (string-null? trimmed)
                        (string-prefix? "#" trimmed)
                        (string=? trimmed "date,value"))
                    (loop f-start f-end rows))
                   (else
                    (let ((comma (string-index trimmed #\,)))
                      (if comma
                          (let ((d (string-trim-both (substring trimmed 0 comma)))
                                (v (string->number
                                    (string-trim-both (substring trimmed (1+ comma))))))
                            (if (and (not (string-null? d)) v)
                                (loop f-start f-end (cons (cons d v) rows))
                                (loop f-start f-end rows)))
                          (loop f-start f-end rows))))))))))))))

(define (benchmark-needs-fetch? f-start f-end from-iso to-iso today-iso)
  (let ((eff-end (if (string<=? to-iso today-iso) to-iso today-iso)))
    (and (string<=? from-iso eff-end)
         (or (not f-start)
             (not f-end)
             (string>? f-start from-iso)
             (string<? f-end eff-end)))))

(define (compute-cdi-return-series rows date-isos)
  (if (null? rows)
      #f
      (let* ((t0-iso (car date-isos))
             (tn-iso (last date-isos))
             (active-rows (filter (lambda (r)
                                    (and (string>? (car r) t0-iso)
                                         (string<=? (car r) tn-iso)))
                                  rows)))
        (let loop ((dates date-isos)
                   (rs active-rows)
                   (factor 1.0)
                   (acc '()))
          (if (null? dates)
              (reverse acc)
              (let ((d-iso (car dates)))
                (let inner ((rs rs) (f factor))
                  (if (and (pair? rs) (string<=? (caar rs) d-iso))
                      (inner (cdr rs) (* f (+ 1.0 (/ (exact->inexact (cdar rs)) 100.0))))
                      (loop (cdr dates) rs f (cons (- f 1.0) acc))))))))))

(define (compute-price-benchmark-series rows date-isos)
  (if (null? rows)
      #f
      (let* ((t0-iso (car date-isos))
             (tn-iso (last date-isos))
             (prior (filter (lambda (r) (string<=? (car r) t0-iso)) rows))
             (in-range (filter (lambda (r)
                                 (and (string>? (car r) t0-iso)
                                      (string<=? (car r) tn-iso)))
                               rows))
             (base-row (cond
                        ((pair? prior) (last prior))
                        ((pair? in-range) (car in-range))
                        (else #f))))
        (if (or (not base-row) (<= (cdr base-row) 0.0))
            #f
            (let ((base (exact->inexact (cdr base-row))))
              (let loop ((dates date-isos)
                         (rs rows)
                         (curr-price #f)
                         (acc '()))
                (if (null? dates)
                    (reverse acc)
                    (let ((d-iso (car dates)))
                      (let inner ((rs rs) (p curr-price))
                        (if (and (pair? rs) (string<=? (caar rs) d-iso))
                            (inner (cdr rs) (exact->inexact (cdar rs)))
                            (let ((ret (if p (- (/ p base) 1.0) 0.0)))
                              (loop (cdr dates) rs p (cons ret acc)))))))))))))

(define (read-trimmed-first-line path)
  (and (file-exists? path)
       (call-with-input-file path
         (lambda (port)
           (let ((line (read-line port)))
             (and (string? line)
                  (let ((trimmed (string-trim-both line)))
                    (and (not (string-null? trimmed)) trimmed))))))))

(define (resolve-benchmark-python)
  (let* ((env-py (getenv "GNC_BENCHMARK_PYTHON"))
         (local-venv-py (gnc-build-scm-path "gnucash/reports/support/.venv/bin/python"))
         (configured-py (read-trimmed-first-line
                         (gnc-build-scm-path "gnucash/reports/support/benchmark-python.txt"))))
    (cond
     ((and env-py (not (string-null? env-py)) (file-exists? env-py)) env-py)
     ((file-exists? local-venv-py) local-venv-py)
     ((and configured-py (file-exists? configured-py)) configured-py)
     (else "python3"))))

(define (run-benchmark-fetch cache-dir from-iso to-iso specs)
  (let ((script-path (gnc-build-scm-path "gnucash/reports/support/benchmark-fetch.py"))
        (python-exe (resolve-benchmark-python)))
    (if (not (file-exists? script-path))
        (format #f (G_ "Fetcher script not found: ~a") script-path)
        (catch #t
          (lambda ()
            (let* ((port (apply open-pipe* OPEN_READ
                                python-exe script-path
                                "--cache-dir" cache-dir
                                "--start" from-iso
                                "--end" to-iso
                                specs))
                   (output (read-all-from-port port))
                   (status (close-pipe port))
                   (exit-code (status:exit-val status)))
              (if (and exit-code (zero? exit-code))
                  (let* ((parsed (json-string->scm output))
                         (raw-results (assoc-ref parsed "results"))
                         (results (cond
                                   ((vector? raw-results) (vector->list raw-results))
                                   ((list? raw-results) raw-results)
                                   (else '()))))
                    (string-join
                     (map (lambda (item)
                            (let ((spec (or (assoc-ref item "spec") "?"))
                                  (st (or (assoc-ref item "status") "?"))
                                  (err (assoc-ref item "error")))
                              (cond
                               ((equal? st "error")
                                (format #f "~a: ~a (~a)" spec (G_ "error") (or err "")))
                               ((equal? st "cached")
                                (format #f "~a: ~a" spec (G_ "up to date")))
                               (else
                                (format #f "~a: ~a (+~a)" spec (G_ "updated")
                                        (or (assoc-ref item "added") 0))))))
                          results)
                     " | "))
                  (format #f (G_ "Fetch failed (exit ~a): ~a") exit-code output))))
          (lambda (key . args)
            (format #f (G_ "Fetch error: ~a ~a") key args))))))

(define (investment-return-renderer report-obj export-type)

  (define (get-option section name)
    (gnc-optiondb-lookup-value (gnc:report-options report-obj) section name))

  (gnc:report-starting reportname)
  (let* ((to-date-t64 (gnc:time64-end-day-time
                       (gnc:date-option-absolute-time
                        (get-option gnc:pagename-general optname-to-date))))
         (from-date-t64 (gnc:time64-start-day-time
                         (gnc:date-option-absolute-time
                          (get-option gnc:pagename-general optname-from-date))))
         (interval (get-option gnc:pagename-general optname-stepsize))
         (report-currency (get-option gnc:pagename-general optname-report-currency))
         (price-source (get-option gnc:pagename-general optname-price-source))
         (report-title (get-option gnc:pagename-general gnc:optname-reportname))

         (accounts (get-option gnc:pagename-accounts optname-accounts))
         (account-levels (get-option gnc:pagename-accounts optname-levels))
         (return-accounts (get-option gnc:pagename-accounts optname-return-accounts))

         (show-fullname? (get-option gnc:pagename-display optname-fullname))
         (show-total? (get-option gnc:pagename-display optname-show-total))
         (show-table? (get-option gnc:pagename-display optname-show-table))
         (height (get-option gnc:pagename-display optname-plot-height))
         (width (get-option gnc:pagename-display optname-plot-width))
         (line-width (get-option gnc:pagename-display optname-line-width))
         (y-grid (get-option gnc:pagename-display optname-y-grid))
         (tooltip-indexed (get-option gnc:pagename-display optname-tooltip-indexed))
         (tooltip-non-zero-only (get-option gnc:pagename-display optname-tooltip-non-zero-only))

         (show-cdi? (get-option pagename-benchmarks optname-show-cdi))
         (show-ibov? (get-option pagename-benchmarks optname-show-ibov))
         (show-sp500? (get-option pagename-benchmarks optname-show-sp500))
         (custom-tickers-str (get-option pagename-benchmarks optname-custom-tickers))
         (custom-tickers
          (delete-duplicates
           (filter (negate string-null?)
                   (map string-trim-both
                        (string-split (or custom-tickers-str "") #\,)))
           string=?))
         (enabled-benchmarks
          (delete-duplicates
           (append
            (if show-cdi? (list (list "CDI" "CDI" "CDI" 'cdi)) '())
            (if show-ibov? (list (list "IBOVESPA" "IBOVESPA" "IBOVESPA" 'price)) '())
            (if show-sp500?
                (list (list "SP500_BRL" "SP500_BRL" (G_ "S&P 500 (BRL)") 'price))
                '())
            (map (lambda (t)
                   (list (string-append "YAHOO:" t)
                         (string-append "YAHOO_" (sanitize-ticker t))
                         t
                         'price))
                 custom-tickers))
           (lambda (a b) (string=? (cadr a) (cadr b)))))

         (document (gnc:make-html-document))
         (chart (gnc:make-html-chart)))

    (define (show-acct? a)
      (member a accounts))

    (define (investment-acct? a)
      (member (xaccAccountGetType a) investment-account-types))

    (define tree-depth
      (if (eq? account-levels 'all)
          (gnc:get-current-account-tree-depth)
          account-levels))

    (define acct->name
      (if show-fullname? gnc-account-get-full-name xaccAccountGetName))

    (cond
     ((and (null? accounts) (null? enabled-benchmarks))
      (gnc:html-document-add-object!
       document
       (gnc:html-make-no-account-warning
        report-title (gnc:report-id report-obj))))

     ((<= to-date-t64 from-date-t64)
      (gnc:html-document-add-object!
       document
       (gnc:html-make-generic-warning
        report-title (gnc:report-id report-obj)
        (G_ "Invalid dates")
        (G_ "Start date must be earlier than End date"))))

     (else
      (let* ((all-candidate-accounts
              (if (null? accounts)
                  '()
                  (gnc-accounts-and-all-descendants accounts)))
             (commodity-list (gnc:accounts-get-commodities
                              all-candidate-accounts
                              report-currency))
             (exchange-fn (gnc:case-exchange-time-fn
                           price-source report-currency
                           commodity-list to-date-t64
                           5 15))
             (dates-list (gnc:make-date-list
                          (gnc:time64-end-day-time from-date-t64)
                          (gnc:time64-end-day-time to-date-t64)
                          (gnc:deltasym-to-delta interval)))
             (iso-date (qof-date-format-get-string QOF-DATE-FORMAT-ISO))
             (date-isos (map (cut gnc-print-time64 <> iso-date) dates-list))
             (from-iso (car date-isos))
             (to-iso (last date-isos))
             (today-iso (gnc-print-time64 (gnc:get-today) iso-date))
             (cache-dir (gnc-build-userdata-path "benchmarks"))
             (date-string-list (map qof-print-date dates-list))
             (topl-accounts (filter
                             (lambda (a)
                               (or (investment-acct? a)
                                   (any show-acct? (gnc-account-get-descendants a))))
                             (gnc-account-get-children-sorted
                              (gnc-get-current-root-account))))
             (return-acct-set (let ((h (make-hash-table)))
                                (for-each (cut hash-set! h <> #t) return-accounts)
                                h))
             (work-done 0)
             (work-to-do 0))

        (define (collector->report-currency-amount c date)
          (gnc:gnc-monetary-amount
           (gnc:sum-collector-commodity
            c report-currency
            (lambda (a b) (exchange-fn a b date)))))

        ;; Resolve all accounts represented by a parent or leaf node.
        ;; When a parent has selected descendants, expand only those selected
        ;; descendants; when no descendant is selected, include all investment
        ;; descendants so selecting a parent node computes its value-weighted
        ;; subtree return.
        (define (expand-account-subtree acct)
          (let ((selected-descendants
                 (filter show-acct? (gnc-account-get-descendants acct))))
            (delete-duplicates
             (if (pair? selected-descendants)
                 (cons acct (append-map expand-account-subtree selected-descendants))
                 (cons acct (filter investment-acct?
                                    (gnc-account-get-descendants acct))))
             equal?)))

        ;; Compute Time-Weighted Return (TWR) series over dates-list for
        ;; account-set. Because market values and external cash flows are
        ;; summed across account-set (and internal transfers cancel), a
        ;; parent's return equals the value-weighted return of its children.
        ;; On dates with external cash flows, pre-cash-flow holdings are
        ;; valued at the cash-flow date's price (plus any standalone
        ;; income/expense transactions on that date) so stale or trade-vs-EOD
        ;; price differences do not distort returns on large deposits.
        (define (compute-twr-series account-set)
          (let ((t0 (car dates-list))
                (tn (last dates-list))
                (acct-set (make-hash-table))
                (seen-txns (make-hash-table))
                (cf-by-date (make-hash-table))
                (inc-exp-by-date (make-hash-table)))
            (for-each (cut hash-set! acct-set <> #t) account-set)
            (for-each
             (lambda (acc)
               (for-each
                (lambda (split)
                  (let* ((trans (xaccSplitGetParent split))
                         (tx-date (xaccTransGetDate trans)))
                    (unless (or (hash-ref seen-txns trans)
                                (xaccTransGetIsClosingTxn trans)
                                (<= tx-date t0)
                                (> tx-date tn))
                      (hash-set! seen-txns trans #t)
                      (let* ((trans-curr (xaccTransGetCurrency trans))
                             (splits (xaccTransGetSplitList trans))
                             (ext-sum
                              (fold
                               (lambda (s sum)
                                 (let ((sa (xaccSplitGetAccount s)))
                                   (if (and (not (null? sa))
                                            (not (hash-ref acct-set sa))
                                            (not (eqv? (xaccAccountGetType sa)
                                                       ACCT-TYPE-TRADING))
                                            (not (hash-ref return-acct-set sa)))
                                       (+ sum (xaccSplitGetValue s))
                                       sum)))
                               0
                               splits))
                             (cf-raw (- ext-sum))
                             (cf-date (gnc:time64-end-day-time tx-date)))
                        (cond
                         ((not (zero? cf-raw))
                          (let* ((cf-mon (gnc:make-gnc-monetary trans-curr cf-raw))
                                 (cf-conv (gnc-numeric-convert
                                           (gnc:gnc-monetary-amount
                                            (exchange-fn cf-mon report-currency cf-date))
                                           (gnc-commodity-get-fraction report-currency)
                                           GNC-RND-ROUND)))
                            (hashv-set! cf-by-date cf-date
                                        (+ (or (hashv-ref cf-by-date cf-date) 0)
                                           cf-conv))))
                         ((any (lambda (s)
                                 (let ((sa (xaccSplitGetAccount s)))
                                   (and (not (null? sa))
                                        (hash-ref acct-set sa)
                                        (not (zero? (xaccSplitGetAmount s))))))
                               splits)
                          (let ((net-acct-val
                                 (fold
                                  (lambda (s sum)
                                    (let ((sa (xaccSplitGetAccount s)))
                                      (if (and (not (null? sa))
                                               (hash-ref acct-set sa))
                                          (+ sum (xaccSplitGetValue s))
                                          sum)))
                                  0
                                  splits)))
                            (when (not (zero? net-acct-val))
                              (let* ((ie-mon (gnc:make-gnc-monetary trans-curr net-acct-val))
                                     (ie-conv (gnc-numeric-convert
                                               (gnc:gnc-monetary-amount
                                                (exchange-fn ie-mon report-currency cf-date))
                                               (gnc-commodity-get-fraction report-currency)
                                               GNC-RND-ROUND)))
                                (hashv-set! inc-exp-by-date cf-date
                                            (+ (or (hashv-ref inc-exp-by-date cf-date) 0)
                                               ie-conv)))))))))))
                (xaccAccountGetSplits acc)))
             account-set)

            (let* ((cf-dates (sort (hash-map->list (lambda (k v) k) cf-by-date) <))
                   (v-pre-by-date (make-hash-table))
                   (eval-dates (dedupe-sorted-numbers
                                (sort (append dates-list cf-dates) <)))
                   (account-monetaries
                    (map
                     (lambda (acc)
                       (let ((comm (xaccAccountGetCommodity acc)))
                         (gnc:account-accumulate-at-dates
                          acc eval-dates
                          #:split->elt (lambda (s)
                                         (gnc:make-gnc-monetary
                                          comm (xaccSplitGetNoclosingBalance s)))
                          #:nosplit->elt (gnc:make-gnc-monetary comm 0))))
                     account-set))
                   (date-collectors
                    (apply map gnc:monetaries-add account-monetaries))
                   (values-list
                    (map collector->report-currency-amount
                         date-collectors eval-dates)))
              (when (pair? cf-dates)
                (let* ((pre-dates (map (lambda (d) (- (gnc:time64-start-day-time d) 1))
                                       cf-dates))
                       (pre-monetaries
                        (map
                         (lambda (acc)
                           (let ((comm (xaccAccountGetCommodity acc)))
                             (gnc:account-accumulate-at-dates
                              acc pre-dates
                              #:split->elt (lambda (s)
                                             (gnc:make-gnc-monetary
                                              comm (xaccSplitGetNoclosingBalance s)))
                              #:nosplit->elt (gnc:make-gnc-monetary comm 0))))
                         account-set))
                       (pre-collectors (apply map gnc:monetaries-add pre-monetaries))
                       (pre-values (map collector->report-currency-amount
                                        pre-collectors cf-dates)))
                  (for-each (cut hashv-set! v-pre-by-date <> <>) cf-dates pre-values)))
              (if (every zero? values-list)
                  #f
                  (let ((twr-by-date (make-hash-table)))
                    (hashv-set! twr-by-date (car eval-dates) 0.0)
                    (let loop ((dates (cdr eval-dates))
                               (vals (cdr values-list))
                               (v-prev (exact->inexact (car values-list)))
                               (growth 1.0))
                      (unless (null? dates)
                        (let* ((d (car dates))
                               (v (exact->inexact (car vals)))
                               (cf (exact->inexact (or (hashv-ref cf-by-date d) 0)))
                               (r (if (zero? cf)
                                      (if (and (> v-prev 0.0) (> v 0.0))
                                          (- (/ v v-prev) 1.0)
                                          0.0)
                                      (if (> v-prev 0.0)
                                          (let ((v-pre (exact->inexact
                                                        (or (hashv-ref v-pre-by-date d) 0)))
                                                (inc-exp (exact->inexact
                                                          (or (hashv-ref inc-exp-by-date d) 0))))
                                            (- (/ (+ v-pre inc-exp) v-prev) 1.0))
                                          0.0)))
                               (next-v-prev (cond
                                             ((and (zero? cf) (> v-prev 0.0) (<= v 0.0))
                                              v-prev)
                                             ((and (not (zero? cf)) (<= v-prev 0.0))
                                              (if (> cf 0.0) cf v))
                                             (else v)))
                               (new-growth (* growth (max 0.0 (+ 1.0 r)))))
                          (hashv-set! twr-by-date d (- new-growth 1.0))
                          (loop (cdr dates) (cdr vals) next-v-prev new-growth))))
                    (map (cut hashv-ref twr-by-date <>) dates-list))))))

        (define (count-accounts current-depth accts)
          (fold
           (lambda (a sum)
             (+ sum 1
                (if (and (>= current-depth tree-depth) (show-acct? a))
                    0
                    (count-accounts (1+ current-depth)
                                    (gnc-account-get-children a)))))
           0 accts))

        (define (traverse-accounts current-depth accts)
          (let ((res '()))
            (for-each
             (lambda (a)
               (set! work-done (1+ work-done))
               (gnc:report-percent-done
                (min 80 (+ 20 (* 55 (/ work-done (max 1 work-to-do))))))
               (cond
                ((>= current-depth tree-depth)
                 (if (show-acct? a)
                     (let* ((target-accts (expand-account-subtree a))
                            (twr (compute-twr-series target-accts)))
                       (when twr
                         (set! res (cons (list a target-accts twr) res))))
                     (set! res
                       (append (reverse (traverse-accounts
                                         (1+ current-depth)
                                         (gnc-account-get-children-sorted a)))
                               res))))
                (else
                 (when (show-acct? a)
                   (let* ((children (gnc-account-get-children a))
                          (selected-descendants
                           (filter show-acct? (gnc-account-get-descendants a)))
                          (target-accts
                           (if (and (null? selected-descendants) (pair? children))
                               (expand-account-subtree a)
                               (list a)))
                          (twr (compute-twr-series target-accts)))
                     (when twr
                       (set! res (cons (list a target-accts twr) res)))))
                 (set! res
                   (append (reverse (traverse-accounts
                                     (1+ current-depth)
                                     (gnc-account-get-children-sorted a)))
                           res)))))
             accts)
            (reverse res)))

        (gnc:report-percent-done 15)
        (set! work-to-do (count-accounts 1 topl-accounts))
        (let* ((series-entries
                (if (null? accounts)
                    '()
                    (map (lambda (e)
                           (list 'account (car e) (acct->name (car e)) (caddr e) (cadr e)))
                         (traverse-accounts 1 topl-accounts))))
               (portfolio-entries
                (if (and show-total? (> (length series-entries) 1))
                    (let* ((total-accts (delete-duplicates
                                         (append-map (cut list-ref <> 4) series-entries)
                                         equal?))
                           (total-twr (compute-twr-series total-accts)))
                      (if total-twr
                          (append series-entries
                                  (list (list 'total #f (G_ "Total Portfolio") total-twr total-accts)))
                          series-entries))
                    series-entries))
               (missing-benchmarks '())
               (benchmark-entries
                (filter-map
                 (lambda (b)
                   (let* ((key (cadr b))
                          (label (caddr b))
                          (kind (cadddr b))
                          (cache-path (string-append cache-dir "/" key ".csv"))
                          (cached (read-benchmark-cache cache-path))
                          (f-start (car cached))
                          (f-end (cadr cached))
                          (rows (caddr cached))
                          (series (if (eq? kind 'cdi)
                                      (compute-cdi-return-series rows date-isos)
                                      (compute-price-benchmark-series rows date-isos))))
                     (when (benchmark-needs-fetch? f-start f-end from-iso to-iso today-iso)
                       (set! missing-benchmarks (cons label missing-benchmarks)))
                     (and series (list 'benchmark #f label series '()))))
                 enabled-benchmarks))
               (all-data (append portfolio-entries benchmark-entries)))
          (gnc:report-percent-done 85)

          (when (and (pair? enabled-benchmarks) (not (eq? export-type 'csv)))
            (let* ((fetch-specs (map car enabled-benchmarks))
                   (fetch-url (gnc:make-report-action-anchor
                               report-obj
                               (lambda ()
                                 (let ((msg (run-benchmark-fetch
                                             cache-dir from-iso to-iso fetch-specs)))
                                   (hashv-set! *benchmark-fetch-status*
                                               (gnc:report-id report-obj)
                                               msg)))))
                   (last-status (hashv-ref *benchmark-fetch-status*
                                           (gnc:report-id report-obj)))
                   (cache-summary
                    (if (null? missing-benchmarks)
                        (G_ "All selected benchmarks cached for this period.")
                        (format #f (G_ "Missing cached data for: ~a")
                                (string-join (reverse missing-benchmarks) ", "))))
                   (status-text (if last-status
                                    (string-append last-status " — " cache-summary)
                                    cache-summary)))
              (gnc:html-document-add-object!
               document
               (gnc:make-html-text
                (gnc:html-markup/attr
                 "div"
                 "style='margin-bottom:8px;padding:6px 10px;background:#f5f5f5;border:1px solid #ddd;border-radius:4px;font-size:0.9em;'"
                 (gnc:html-markup/attr
                  "a"
                  (format #f "href=~s style='display:inline-block;padding:4px 10px;background:#0074D9;color:#ffffff;text-decoration:none;border-radius:3px;font-weight:bold;margin-right:10px;'"
                          fetch-url)
                  (G_ "Fetch / Update Benchmarks"))
                 (gnc:html-markup/attr "span" "style='color:#333333;'" status-text))))))

          (cond
           ((null? all-data)
            (gnc:html-document-add-object!
             document
             (gnc:html-make-empty-data-warning
              report-title (gnc:report-id report-obj))))

           (else
            (let* ((series-names (map caddr all-data))
                   (series-values (map cadddr all-data))
                   (list-of-rows (apply zip series-values)))

              (gnc:html-chart-set-type! chart 'line)
              (gnc:html-chart-set-title!
               chart (list report-title
                           (gnc-date-interval-format from-date-t64 to-date-t64)))
              (gnc:html-chart-set-width! chart width)
              (gnc:html-chart-set-height! chart height)
              (gnc:html-chart-set-tooltip-mode!
               chart (if tooltip-indexed 'index 'point))
              (gnc:html-chart-set-tooltip-non-zero-only!
               chart tooltip-non-zero-only)
              (gnc:html-chart-set-tooltip-caretpadding-from-prefs!
               chart #t tooltip-indexed)
              (gnc:html-chart-set-data-labels! chart date-string-list)
              (gnc:html-chart-set-y-axis-label! chart (G_ "Return (%)"))
              (gnc:html-chart-set-grid?! chart y-grid)
              (gnc:html-chart-set-format-style! chart "percent")
              (gnc:html-chart-set-currency-iso!
               chart (gnc-commodity-get-mnemonic report-currency))
              (gnc:html-chart-set-currency-symbol!
               chart (gnc-commodity-get-nice-symbol report-currency))

              (for-each
               (lambda (entry label color)
                 (let* ((kind (car entry))
                        (acct (cadr entry))
                        (twr (cadddr entry))
                        (urls (cond
                               ((not (eq? kind 'account)) #f)
                               ((or (null? (gnc-account-get-children acct))
                                    (>= tree-depth 6))
                                (gnc:account-anchor-text acct))
                               (else
                                (gnc:make-report-anchor
                                 investment-return-chart-uuid report-obj
                                 (list
                                  (list gnc:pagename-accounts optname-accounts
                                        (cons acct (gnc-account-get-children acct)))
                                  (list gnc:pagename-accounts optname-levels
                                        (1+ tree-depth))
                                  (list gnc:pagename-general
                                        gnc:optname-reportname
                                        (acct->name acct))))))))
                   (cond
                    ((eq? kind 'benchmark)
                     (gnc:html-chart-add-data-series!
                      chart label twr color
                      'fill #f
                      'borderWidth line-width
                      'borderDash '(6 4)))
                    (urls
                     (gnc:html-chart-add-data-series!
                      chart label twr color
                      'fill #f
                      'borderWidth line-width
                      'urls urls))
                    (else
                     (gnc:html-chart-add-data-series!
                      chart label twr color
                      'fill #f
                      'borderWidth line-width)))))
               all-data
               series-names
               (gnc:assign-colors (length all-data)))

              (gnc:report-percent-done 95)
              (gnc:html-document-add-object! document chart)

              (when show-table?
                (let ((table (gnc:make-html-table)))
                  (gnc:html-table-set-style!
                   table "table"
                   'attribute (list "border" 0)
                   'attribute (list "cellspacing" 0)
                   'attribute (list "cellpadding" 4))
                  (gnc:html-table-set-col-headers!
                   table (cons (G_ "Date") series-names))
                  (for-each
                   (lambda (date row)
                     (gnc:html-table-append-row!
                      table
                      (cons date
                            (map (lambda (val)
                                   (gnc:make-html-table-cell/markup
                                    "number-cell"
                                    (format #f "~,2f%" (* 100.0 val))))
                                 row))))
                   date-string-list
                   list-of-rows)
                  (gnc:html-document-add-object! document table)))

              (when (eq? export-type 'csv)
                (gnc:html-document-set-export-string
                 document
                 (gnc:lists->csv
                  (cons (cons (G_ "Date") series-names)
                        (map (lambda (d-iso row)
                               (cons d-iso
                                     (map (lambda (val)
                                            (/ (round (* val 1000000.0)) 10000.0))
                                          row)))
                             date-isos
                             list-of-rows))))))))))))

    (unless (gnc:html-document-export-string document)
      (gnc:html-document-set-export-error document (G_ "No exportable data")))

    (gnc:report-finished)
    document))

(gnc:define-report
 'version 1
 'name reportname
 'report-guid investment-return-chart-uuid
 'menu-path (list gnc:menuname-investments)
 'menu-name menuname
 'menu-tip menutip
 'options-generator options-generator
 'export-types '(("CSV" . csv))
 'export-thunk (lambda (report-obj export-type)
                 (investment-return-renderer report-obj export-type))
 'renderer (lambda (report-obj)
             (investment-return-renderer report-obj #f)))
