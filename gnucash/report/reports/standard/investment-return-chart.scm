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
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))
(use-modules (ice-9 format))

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
     ((null? accounts)
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
      (let* ((all-candidate-accounts (gnc-accounts-and-all-descendants accounts))
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
        (let* ((series-entries (traverse-accounts 1 topl-accounts))
               (all-data
                (if (and show-total? (> (length series-entries) 1))
                    (let* ((total-accts (delete-duplicates
                                         (append-map cadr series-entries)
                                         equal?))
                           (total-twr (compute-twr-series total-accts)))
                      (if total-twr
                          (append series-entries
                                  (list (list (G_ "Total Portfolio") total-accts total-twr)))
                          series-entries))
                    series-entries)))
          (gnc:report-percent-done 85)
          (cond
           ((null? all-data)
            (gnc:html-document-add-object!
             document
             (gnc:html-make-empty-data-warning
              report-title (gnc:report-id report-obj))))

           (else
            (let* ((series-names
                    (map (lambda (entry)
                           (let ((acct (car entry)))
                             (if (string? acct) acct (acct->name acct))))
                         all-data))
                   (series-values (map caddr all-data))
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
                 (let* ((acct (car entry))
                        (twr (caddr entry))
                        (urls (cond
                               ((string? acct) #f)
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
                   (if urls
                       (gnc:html-chart-add-data-series!
                        chart label twr color
                        'fill #f
                        'borderWidth line-width
                        'urls urls)
                       (gnc:html-chart-add-data-series!
                        chart label twr color
                        'fill #f
                        'borderWidth line-width))))
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
                (let ((iso-date (qof-date-format-get-string QOF-DATE-FORMAT-ISO)))
                  (gnc:html-document-set-export-string
                   document
                   (gnc:lists->csv
                    (cons (cons (G_ "Date") series-names)
                          (map (lambda (d row)
                                 (cons (gnc-print-time64 d iso-date)
                                       (map (lambda (val)
                                              (/ (round (* val 1000000.0)) 10000.0))
                                            row)))
                               dates-list
                               list-of-rows)))))))))))))

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
