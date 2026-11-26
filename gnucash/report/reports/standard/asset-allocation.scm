;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; asset-allocation.scm : Target vs actual asset allocation
;; report with recursive account tree roll-up and historical chart.
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

(define-module (gnucash reports standard asset-allocation))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-9))
(use-modules (srfi srfi-26))
(use-modules (ice-9 format))

(export asset-allocation-uuid)

(define asset-allocation-uuid "4e9a1c7d3b2f48e6a501c9d2b7f4831a")

(define reportname (N_ "Asset Allocation"))
(define menuname (N_ "Asset Allocation"))
(define menutip
  (N_ "Compare target vs actual asset allocation across accounts and over time"))

(define pagename-allocation (N_ "Allocation"))
(define optname-target-allocations (N_ "Target Allocations"))
(define optname-levels (N_ "Levels of Subaccounts"))

(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))
(define optname-stepsize (N_ "Step Size"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))

(define optname-fullname (N_ "Show long account names"))
(define optname-show-bar-chart (N_ "Show current allocation bar chart"))
(define optname-show-time-chart (N_ "Show allocation over time chart"))
(define optname-show-table (N_ "Show table"))
(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))
(define optname-line-width (N_ "Line Width"))
(define opthelp-line-width (N_ "Set line width in pixels."))
(define optname-y-grid (N_ "Grid"))

(define-record-type <alloc-node>
  (make-alloc-node acct depth direct effective universe-accts
                   val-series actual-pct-series
                   target-val actual-val diff-val
                   actual-pct diff-pct-pts rel-diff-pct)
  alloc-node?
  (acct node-acct)
  (depth node-depth)
  (direct node-direct)
  (effective node-effective)
  (universe-accts node-universe-accts)
  (val-series node-val-series set-node-val-series!)
  (actual-pct-series node-actual-pct-series set-node-actual-pct-series!)
  (target-val node-target-val set-node-target-val!)
  (actual-val node-actual-val set-node-actual-val!)
  (diff-val node-diff-val set-node-diff-val!)
  (actual-pct node-actual-pct set-node-actual-pct!)
  (diff-pct-pts node-diff-pct-pts set-node-diff-pct-pts!)
  (rel-diff-pct node-rel-diff-pct set-node-rel-diff-pct!))

(define (options-generator)
  (let ((options (gnc-new-optiondb)))

    ;; Allocation tab (default)
    (gnc-register-account-allocation-option options
      pagename-allocation optname-target-allocations
      "a"
      (N_ "Direct target percentage per account. Parent effective totals roll up recursively from child accounts, and direct targets must sum to 100%.")
      "")

    (gnc:options-add-account-levels!
     options pagename-allocation optname-levels "b"
     (N_ "Maximum number of levels in the account tree displayed.")
     'all)

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

    ;; Display tab
    (gnc-register-simple-boolean-option options
      gnc:pagename-display optname-fullname
      "a" (N_ "Show the full account name?") #f)

    (gnc-register-simple-boolean-option options
      gnc:pagename-display optname-show-bar-chart
      "b" (N_ "Display a bar chart comparing target vs actual currency amounts?") #t)

    (gnc-register-simple-boolean-option options
      gnc:pagename-display optname-show-time-chart
      "c" (N_ "Display a line chart of actual vs target percentages over time?") #t)

    (gnc-register-simple-boolean-option options
      gnc:pagename-display optname-show-table
      "d" (N_ "Display a summary rebalancing table?") #t)

    (gnc:options-add-plot-size!
     options gnc:pagename-display
     optname-plot-width optname-plot-height "e"
     (cons 'percent 100.0) (cons 'pixels 450.0))

    (gnc-register-number-range-option options
      gnc:pagename-display optname-line-width
      "f" opthelp-line-width
      2.0 0.5 5.0 0.5)

    (gnc-register-simple-boolean-option options
      gnc:pagename-display optname-y-grid
      "g" (N_ "Add grid lines.") #t)

    (gnc:options-set-default-section options pagename-allocation)

    options))

(define (dedupe-sorted-numbers lst)
  (let loop ((rest lst) (acc '()))
    (cond
     ((null? rest) (reverse acc))
     ((and (pair? acc) (= (car rest) (car acc)))
      (loop (cdr rest) acc))
     (else
      (loop (cdr rest) (cons (car rest) acc))))))

(define (time64-midday t64)
  (let ((bdt (gnc-localtime t64)))
    (set-tm:sec bdt 0)
    (set-tm:min bdt 0)
    (set-tm:hour bdt 12)
    (set-tm:isdst bdt -1)
    (gnc-mktime bdt)))

(define (parse-percent-number str)
  (let ((normalized
         (string-map (lambda (c) (if (char=? c #\,) #\. c))
                     (string-trim-both str))))
    (and (not (string-null? normalized))
         (let ((num (string->number normalized)))
           (and (number? num) (exact->inexact num))))))

(define (build-guid-account-table root)
  (let ((tbl (make-hash-table)))
    (when root
      (for-each
       (lambda (acct)
         (hash-set! tbl (gncAccountGetGUID acct) acct))
       (gnc-account-get-descendants root)))
    tbl))

(define (parse-allocations-string raw guid->acct)
  (let ((acct->direct (make-hash-table))
        (total 0.0)
        (missing-count 0))
    (when (string? raw)
      (for-each
       (lambda (token)
         (let ((trimmed (string-trim-both token)))
           (unless (string-null? trimmed)
             (let ((eq-idx (string-index trimmed #\=)))
               (when (and eq-idx (> eq-idx 0))
                 (let* ((guid (string-trim-both (substring trimmed 0 eq-idx)))
                        (val (parse-percent-number
                              (substring trimmed (+ eq-idx 1))))
                        (acct (hash-ref guid->acct guid)))
                   (when (and val (> val 1e-6))
                     (set! total (+ total val))
                     (if acct
                         (hash-set! acct->direct acct
                                    (+ (or (hash-ref acct->direct acct) 0.0) val))
                         (set! missing-count (+ missing-count 1))))))))))
       (string-split raw #\;)))
    (values acct->direct total missing-count)))

(define (build-allocation-tree root acct->direct)
  (define nodes '())

  (define (walk acct depth ancestor-targeted?)
    (let* ((direct (or (hash-ref acct->direct acct) 0.0))
           (in-universe? (or (> direct 1e-6) ancestor-targeted?))
           (child-results
            (map (cut walk <> (+ depth 1) in-universe?)
                 (gnc-account-get-children-sorted acct)))
           (effective
            (+ direct (fold + 0.0 (map node-effective child-results))))
           (universe-accts
            (append (if in-universe? (list acct) '())
                    (append-map node-universe-accts child-results)))
           (node (make-alloc-node acct depth direct effective universe-accts
                                  '() '() 0.0 0.0 0.0 0.0 0.0 0.0)))
      (when (> effective 1e-6)
        (set! nodes (cons node nodes)))
      node))

  (when root
    (for-each (cut walk <> 1 #f)
              (gnc-account-get-children-sorted root)))
  (reverse nodes))

(define (sum-number-lists lists count)
  (if (null? lists)
      (make-list count 0.0)
      (apply map + lists)))

(define (float->monetary currency val)
  (let ((scu (max 1 (gnc-commodity-get-fraction currency))))
    (gnc:make-gnc-monetary currency
                           (/ (inexact->exact (round (* val scu))) scu))))

(define (format-signed-pct val decimals)
  (let* ((eps (/ 0.5 (expt 10 decimals)))
         (clean-val (if (< (abs val) eps) 0.0 val))
         (sign (if (> clean-val 0.0) "+" "")))
    (if (= decimals 1)
        (format #f "~a~,1f%" sign clean-val)
        (format #f "~a~,2f%" sign clean-val))))

(define (format-signed-currency currency val)
  (let ((mon-str (gnc:monetary->string (float->monetary currency val))))
    (if (> val 0.004)
        (string-append "+" mon-str)
        mon-str)))

(define (asset-allocation-renderer report-obj export-type)

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

         (alloc-str (get-option pagename-allocation optname-target-allocations))
         (account-levels (get-option pagename-allocation optname-levels))

         (show-fullname? (get-option gnc:pagename-display optname-fullname))
         (show-bar-chart? (get-option gnc:pagename-display optname-show-bar-chart))
         (show-time-chart? (get-option gnc:pagename-display optname-show-time-chart))
         (show-table? (get-option gnc:pagename-display optname-show-table))
         (height (get-option gnc:pagename-display optname-plot-height))
         (width (get-option gnc:pagename-display optname-plot-width))
         (line-width (get-option gnc:pagename-display optname-line-width))
         (y-grid (get-option gnc:pagename-display optname-y-grid))

         (root (gnc-get-current-root-account))
         (guid->acct (build-guid-account-table root))
         (document (gnc:make-html-document)))

    (define tree-depth
      (if (eq? account-levels 'all)
          (max 1 (gnc:get-current-account-tree-depth))
          account-levels))

    (define acct->name
      (if show-fullname? gnc-account-get-full-name xaccAccountGetName))

    (call-with-values
        (lambda () (parse-allocations-string alloc-str guid->acct))
      (lambda (acct->direct total-direct missing-count)
        (cond
         ((<= total-direct 1e-6)
          (gnc:html-document-add-object!
           document
           (gnc:html-make-generic-warning
            report-title (gnc:report-id report-obj)
            (G_ "No target allocations configured")
            (G_ "Open Report Options (Allocation tab) to assign target percentages to your accounts so they sum to 100%."))))

         ((> (abs (- total-direct 100.0)) 0.01)
          (let ((diff (- total-direct 100.0)))
            (gnc:html-document-add-object!
             document
             (gnc:html-make-generic-warning
              report-title (gnc:report-id report-obj)
              (G_ "Target allocations must sum to 100%")
              (format #f
                      (G_ "The sum of direct target allocations is ~,2f% (difference: ~a). Please adjust the percentages in Report Options so they equal 100.00%.")
                      total-direct
                      (format-signed-pct diff 2))))))

         ((> missing-count 0)
          (gnc:html-document-add-object!
           document
           (gnc:html-make-generic-warning
            report-title (gnc:report-id report-obj)
            (G_ "Missing target accounts")
            (G_ "One or more configured target accounts no longer exist in this book. Please update Report Options."))))

         ((<= to-date-t64 from-date-t64)
          (gnc:html-document-add-object!
           document
           (gnc:html-make-generic-warning
            report-title (gnc:report-id report-obj)
            (G_ "Invalid dates")
            (G_ "Start date must be earlier than End date."))))

         (else
          (let* ((all-nodes (build-allocation-tree root acct->direct))
                 (table-nodes (filter (lambda (n) (<= (node-depth n) tree-depth))
                                      all-nodes))
                 (chart-nodes-filtered
                  (filter (lambda (n)
                            (not (and (<= (node-direct n) 1e-6)
                                      (>= (node-effective n) 99.99))))
                          table-nodes))
                 (chart-nodes (if (null? chart-nodes-filtered)
                                  table-nodes
                                  chart-nodes-filtered))
                 (all-universe-accts
                  (delete-duplicates
                   (append-map node-universe-accts all-nodes)
                   eq?))
                 (raw-dates (dedupe-sorted-numbers
                             (map gnc:time64-end-day-time
                                  (gnc:make-date-list
                                   (time64-midday from-date-t64)
                                   (time64-midday to-date-t64)
                                   (gnc:deltasym-to-delta interval)))))
                 (dates-list
                  (cond
                   ((null? raw-dates) (list from-date-t64 to-date-t64))
                   ((< (last raw-dates) to-date-t64) (append raw-dates (list to-date-t64)))
                   (else raw-dates)))
                 (date-string-list (map qof-print-date dates-list))
                 (commodity-list (gnc:accounts-get-commodities
                                  all-universe-accts report-currency))
                 (exchange-fn (gnc:case-exchange-time-fn
                               price-source report-currency
                               commodity-list to-date-t64
                               5 15))
                 (acct->val-series (make-hash-table))
                 (num-dates (length dates-list)))

            (gnc:report-percent-done 20)
            (for-each
             (lambda (acct)
               (let* ((bals (gnc:account-get-balances-at-dates acct dates-list))
                      (vals (map (lambda (bal d)
                                   (exact->inexact
                                    (gnc:gnc-monetary-amount
                                     (exchange-fn bal report-currency d))))
                                 bals dates-list)))
                 (hash-set! acct->val-series acct vals)))
             all-universe-accts)

            (gnc:report-percent-done 60)
            (let* ((total-val-series
                    (sum-number-lists
                     (map (cut hash-ref acct->val-series <>) all-universe-accts)
                     num-dates))
                   (total-end-val (last total-val-series)))

              (for-each
               (lambda (node)
                 (let* ((vals (sum-number-lists
                               (map (cut hash-ref acct->val-series <>)
                                    (node-universe-accts node))
                               num-dates))
                        (pcts (map (lambda (v tot)
                                     (if (> (abs tot) 1e-9) (/ v tot) 0.0))
                                   vals total-val-series))
                        (actual-val (last vals))
                        (effective (node-effective node))
                        (target-val (* total-end-val (/ effective 100.0)))
                        (diff-val (- actual-val target-val))
                        (actual-pct (* 100.0 (last pcts)))
                        (diff-pct-pts (- actual-pct effective))
                        (rel-diff-pct (if (> (abs target-val) 1e-9)
                                          (* 100.0 (/ diff-val target-val))
                                          0.0)))
                   (set-node-val-series! node vals)
                   (set-node-actual-pct-series! node pcts)
                   (set-node-target-val! node target-val)
                   (set-node-actual-val! node actual-val)
                   (set-node-diff-val! node diff-val)
                   (set-node-actual-pct! node actual-pct)
                   (set-node-diff-pct-pts! node diff-pct-pts)
                   (set-node-rel-diff-pct! node rel-diff-pct)))
               table-nodes)

              (gnc:report-percent-done 80)

              (cond
               ((every (lambda (v) (< (abs v) 1e-6)) total-val-series)
                (gnc:html-document-add-object!
                 document
                 (gnc:html-make-empty-data-warning
                  report-title (gnc:report-id report-obj))))

               (else
                ;; 1. Bar chart: Target vs Actual & Difference in report currency
                (when show-bar-chart?
                  (let ((bar-chart (gnc:make-html-chart)))
                    (gnc:html-chart-set-type! bar-chart 'bar)
                    (gnc:html-chart-set-stacking?! bar-chart #f)
                    (gnc:html-chart-set-title!
                     bar-chart
                     (list (format #f (G_ "~a — Target vs Actual (~a)")
                                   report-title (qof-print-date (last dates-list)))))
                    (gnc:html-chart-set-width! bar-chart width)
                    (gnc:html-chart-set-height! bar-chart height)
                    (gnc:html-chart-set-format-style! bar-chart "currency")
                    (gnc:html-chart-set-currency-iso!
                     bar-chart (gnc-commodity-get-mnemonic report-currency))
                    (gnc:html-chart-set-currency-symbol!
                     bar-chart (gnc-commodity-get-nice-symbol report-currency))
                    (gnc:html-chart-set-y-axis-label!
                     bar-chart (gnc-commodity-get-mnemonic report-currency))
                    (gnc:html-chart-set!
                     bar-chart '(options scales yAxes (0) ticks beginAtZero) #t)
                    (gnc:html-chart-set-grid?! bar-chart y-grid)
                    (gnc:html-chart-set-tooltip-mode! bar-chart 'index)
                    (gnc:html-chart-set-tooltip-non-zero-only! bar-chart #f)
                    (gnc:html-chart-set-data-labels!
                     bar-chart
                     (map (lambda (n)
                            (format #f "~a (~,1f% vs ~,1f%, ~a)"
                                    (acct->name (node-acct n))
                                    (node-actual-pct n)
                                    (node-effective n)
                                    (format-signed-pct (node-diff-pct-pts n) 1)))
                          chart-nodes))
                    (gnc:html-chart-add-data-series!
                     bar-chart (G_ "Target")
                     (map node-target-val chart-nodes)
                     "#0074D9"
                     'fill #t
                     'borderWidth 1)
                    (gnc:html-chart-add-data-series!
                     bar-chart (G_ "Actual")
                     (map node-actual-val chart-nodes)
                     "#2E8B57"
                     'fill #t
                     'borderWidth 1)
                    (gnc:html-chart-add-data-series!
                     bar-chart (G_ "Difference (Actual - Target)")
                     (map node-diff-val chart-nodes)
                     (map (lambda (n)
                            (if (>= (node-diff-val n) 0.0) "#2ECC40" "#FF4136"))
                          chart-nodes)
                     'fill #t
                     'borderWidth 1)
                    (gnc:html-document-add-object! document bar-chart)))

                ;; 2. Detailed rebalancing table
                (when show-table?
                  (let* ((table (gnc:make-html-table))
                         (min-depth (apply min (map node-depth table-nodes))))
                    (gnc:html-table-set-style!
                     table "table"
                     'attribute (list "border" 0)
                     'attribute (list "cellspacing" 0)
                     'attribute (list "cellpadding" 4))
                    (gnc:html-table-set-col-headers!
                     table
                     (list (G_ "Account")
                           (G_ "Direct Target")
                           (G_ "Effective Target")
                           (G_ "Target Value")
                           (G_ "Actual Value")
                           (G_ "Actual %")
                           (G_ "Difference")
                           (G_ "Diff (% pts)")
                           (G_ "Diff vs Target")))
                    (for-each
                     (lambda (n)
                       (let* ((indent-levels (max 0 (- (node-depth n) min-depth)))
                              (indent-str (string-concatenate
                                           (make-list (* 3 indent-levels) "&#160;")))
                              (name-cell (string-append indent-str
                                                        (acct->name (node-acct n))))
                              (direct-str (if (> (node-direct n) 1e-6)
                                              (format #f "~,2f%" (node-direct n))
                                              "—"))
                              (eff-str (format #f "~,2f%" (node-effective n)))
                              (diff-val (node-diff-val n))
                              (diff-cell (gnc:make-html-table-cell/markup
                                          "number-cell"
                                          (format-signed-currency
                                           report-currency diff-val))))
                         (gnc:html-table-append-row!
                          table
                          (list name-cell
                                (gnc:make-html-table-cell/markup "number-cell" direct-str)
                                (gnc:make-html-table-cell/markup "number-cell" eff-str)
                                (gnc:make-html-table-cell/markup
                                 "number-cell"
                                 (float->monetary report-currency (node-target-val n)))
                                (gnc:make-html-table-cell/markup
                                 "number-cell"
                                 (float->monetary report-currency (node-actual-val n)))
                                (gnc:make-html-table-cell/markup
                                 "number-cell"
                                 (format #f "~,2f%" (node-actual-pct n)))
                                diff-cell
                                (gnc:make-html-table-cell/markup
                                 "number-cell"
                                 (format-signed-pct (node-diff-pct-pts n) 2))
                                (gnc:make-html-table-cell/markup
                                 "number-cell"
                                 (format-signed-pct (node-rel-diff-pct n) 2))))))
                     table-nodes)
                    (gnc:html-table-append-row/markup!
                     table "grand-total"
                     (list (gnc:make-html-table-cell/markup "total-label-cell"
                                                            (G_ "Total Portfolio"))
                           (gnc:make-html-table-cell/markup "total-number-cell"
                                                            (format #f "~,2f%" total-direct))
                           (gnc:make-html-table-cell/markup "total-number-cell" "100.00%")
                           (gnc:make-html-table-cell/markup
                            "total-number-cell"
                            (float->monetary report-currency total-end-val))
                           (gnc:make-html-table-cell/markup
                            "total-number-cell"
                            (float->monetary report-currency total-end-val))
                           (gnc:make-html-table-cell/markup "total-number-cell" "100.00%")
                           (gnc:make-html-table-cell/markup
                            "total-number-cell"
                            (float->monetary report-currency 0.0))
                           (gnc:make-html-table-cell/markup "total-number-cell" "0.00%")
                           (gnc:make-html-table-cell/markup "total-number-cell" "0.00%")))
                    (gnc:html-document-add-object! document table)))

                ;; 3. Over-time line chart: solid Actual % vs dashed Target % with matching color
                (when show-time-chart?
                  (let ((line-chart (gnc:make-html-chart))
                        (colors (gnc:assign-colors (length chart-nodes))))
                    (gnc:html-chart-set-type! line-chart 'line)
                    (gnc:html-chart-set-title!
                     line-chart
                     (list (format #f (G_ "~a — Allocation Over Time") report-title)
                           (gnc-date-interval-format from-date-t64 to-date-t64)))
                    (gnc:html-chart-set-width! line-chart width)
                    (gnc:html-chart-set-height! line-chart height)
                    (gnc:html-chart-set-format-style! line-chart "percent")
                    (gnc:html-chart-set-currency-iso!
                     line-chart (gnc-commodity-get-mnemonic report-currency))
                    (gnc:html-chart-set-currency-symbol!
                     line-chart (gnc-commodity-get-nice-symbol report-currency))
                    (gnc:html-chart-set-y-axis-label! line-chart (G_ "Allocation (%)"))
                    (gnc:html-chart-set!
                     line-chart '(options scales yAxes (0) ticks beginAtZero) #t)
                    (gnc:html-chart-set-data-labels! line-chart date-string-list)
                    (gnc:html-chart-set-grid?! line-chart y-grid)
                    (gnc:html-chart-set-tooltip-mode! line-chart 'index)
                    (gnc:html-chart-set-tooltip-non-zero-only! line-chart #f)

                    (for-each
                     (lambda (n color)
                       (let ((label (acct->name (node-acct n)))
                             (target-frac (/ (node-effective n) 100.0)))
                         (gnc:html-chart-add-data-series!
                          line-chart
                          label
                          (node-actual-pct-series n)
                          color
                          'fill #f
                          'borderWidth line-width)
                         (gnc:html-chart-add-data-series!
                          line-chart
                          (format #f (G_ "~a (Target)") label)
                          (make-list num-dates target-frac)
                          color
                          'fill #f
                          'borderWidth line-width
                          'borderDash '(6 4)
                          'pointRadius 0)))
                     chart-nodes
                     colors)

                    (gnc:html-document-add-object! document line-chart)))

                ;; 4. CSV export
                (when (eq? export-type 'csv)
                  (gnc:html-document-set-export-string
                   document
                   (gnc:lists->csv
                    (cons (list (G_ "Account")
                                (G_ "Direct Target (%)")
                                (G_ "Effective Target (%)")
                                (G_ "Target Value")
                                (G_ "Actual Value")
                                (G_ "Actual (%)")
                                (G_ "Difference")
                                (G_ "Diff (% pts)")
                                (G_ "Diff vs Target (%)"))
                          (append
                           (map (lambda (n)
                                  (list (gnc-account-get-full-name (node-acct n))
                                        (node-direct n)
                                        (node-effective n)
                                        (node-target-val n)
                                        (node-actual-val n)
                                        (node-actual-pct n)
                                        (node-diff-val n)
                                        (node-diff-pct-pts n)
                                        (node-rel-diff-pct n)))
                                table-nodes)
                           (list (list (G_ "Total Portfolio")
                                       total-direct
                                       100.0
                                       total-end-val
                                       total-end-val
                                       100.0
                                       0.0
                                       0.0
                                       0.0)))))))))))))))

    (unless (gnc:html-document-export-string document)
      (gnc:html-document-set-export-error document (G_ "No exportable data")))

    (gnc:report-finished)
    document))

(gnc:define-report
 'version 1
 'name reportname
 'report-guid asset-allocation-uuid
 'menu-path (list gnc:menuname-investments)
 'menu-name menuname
 'menu-tip menutip
 'options-generator options-generator
 'export-types '(("CSV" . csv))
 'export-thunk (lambda (report-obj export-type)
                 (asset-allocation-renderer report-obj export-type))
 'renderer (lambda (report-obj)
             (asset-allocation-renderer report-obj #f)))
