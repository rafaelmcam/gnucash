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

(define pagename-macro-classes (N_ "Macro Classes"))
(define optname-macro-classes (N_ "Macro Classes"))

(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))
(define optname-stepsize (N_ "Step Size"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))

(define optname-fullname (N_ "Show long account names"))
(define optname-show-bar-chart (N_ "Show current allocation bar chart"))
(define optname-show-time-chart (N_ "Show allocation over time chart"))
(define optname-show-table (N_ "Show table"))
(define optname-chart-filter-mode (N_ "Chart Account Filter"))
(define optname-chart-top-n (N_ "Maximum Accounts in Charts"))
(define optname-chart-min-pct (N_ "Minimum Allocation in Charts (%)"))
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

(define-record-type <macro-node>
  (make-macro-node name explicit-target accounts-sum effective
                   assigned-accts universe-accts
                   val-series actual-pct-series
                   target-val actual-val diff-val
                   actual-pct diff-pct-pts rel-diff-pct)
  macro-node?
  (name macro-name)
  (explicit-target macro-explicit-target)
  (accounts-sum macro-accounts-sum)
  (effective macro-effective)
  (assigned-accts macro-assigned-accts)
  (universe-accts macro-universe-accts)
  (val-series macro-val-series set-macro-val-series!)
  (actual-pct-series macro-actual-pct-series set-macro-actual-pct-series!)
  (target-val macro-target-val set-macro-target-val!)
  (actual-val macro-actual-val set-macro-actual-val!)
  (diff-val macro-diff-val set-macro-diff-val!)
  (actual-pct macro-actual-pct set-macro-actual-pct!)
  (diff-pct-pts macro-diff-pct-pts set-macro-diff-pct-pts!)
  (rel-diff-pct macro-rel-diff-pct set-macro-rel-diff-pct!))

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
     (N_ "Maximum number of levels in the account tree displayed in charts (the table always shows all accounts).")
     'all)

    ;; Macro Classes tab
    (gnc-register-macro-classes-option options
      pagename-macro-classes optname-macro-classes
      "a"
      (N_ "Define cross-cutting macro classes and optional target percentages (leave 0 to inherit sum of assigned accounts).")
      "")

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

    (gnc-register-multichoice-callback-option options
      gnc:pagename-display optname-chart-filter-mode
      "d1" (N_ "Choose which target accounts are shown in both the bar chart and the over-time chart (the table always shows all accounts).")
      "top-n"
      (list
       (vector 'top-n (G_ "Top N largest allocations"))
       (vector 'min-pct (G_ "Allocations >= minimum %"))
       (vector 'all (G_ "All target accounts")))
      (lambda (x)
        (gnc-optiondb-set-option-selectable-by-name
         options gnc:pagename-display optname-chart-top-n (eq? x 'top-n))
        (gnc-optiondb-set-option-selectable-by-name
         options gnc:pagename-display optname-chart-min-pct (eq? x 'min-pct))))

    (gnc-register-number-range-option options
      gnc:pagename-display optname-chart-top-n
      "d2" (N_ "Maximum number of largest-allocation accounts to display in charts.")
      6 1 50 1)

    (gnc-register-number-range-option options
      gnc:pagename-display optname-chart-min-pct
      "d3" (N_ "Minimum target or actual allocation percentage required for an account to appear in charts.")
      5.0 0.0 100.0 0.5)

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

(define (unescape-alloc-token str)
  (let ((len (string-length str)))
    (let loop ((i 0) (acc '()))
      (cond
       ((>= i len) (list->string (reverse acc)))
       ((and (char=? (string-ref str i) #\%) (<= (+ i 3) len))
        (let ((code (string-downcase (substring str (+ i 1) (+ i 3)))))
          (cond
           ((string=? code "25") (loop (+ i 3) (cons #\% acc)))
           ((string=? code "3b") (loop (+ i 3) (cons #\; acc)))
           ((string=? code "3d") (loop (+ i 3) (cons #\= acc)))
           ((string=? code "7c") (loop (+ i 3) (cons #\| acc)))
           (else (loop (+ i 1) (cons (string-ref str i) acc))))))
       (else
        (loop (+ i 1) (cons (string-ref str i) acc)))))))

(define (parse-allocations-string raw guid->acct)
  (let ((acct->direct (make-hash-table))
        (acct->macro (make-hash-table))
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
                        (rhs (substring trimmed (+ eq-idx 1)))
                        (pipe-idx (string-index rhs #\|))
                        (val-str (if pipe-idx
                                     (substring rhs 0 pipe-idx)
                                     rhs))
                        (macro-name (if pipe-idx
                                        (string-trim-both
                                         (unescape-alloc-token
                                          (substring rhs (+ pipe-idx 1))))
                                        ""))
                        (val (parse-percent-number val-str))
                        (acct (hash-ref guid->acct guid)))
                   (when (or (and val (> val 1e-6))
                             (not (string-null? macro-name)))
                     (if acct
                         (begin
                           (when (and val (> val 1e-6))
                             (set! total (+ total val))
                             (hash-set! acct->direct acct
                                        (+ (or (hash-ref acct->direct acct) 0.0) val)))
                           (unless (string-null? macro-name)
                             (hash-set! acct->macro acct macro-name)))
                         (begin
                           (when (and val (> val 1e-6))
                             (set! total (+ total val)))
                           (set! missing-count (+ missing-count 1)))))))))))
       (string-split raw #\;)))
    (values acct->direct acct->macro total missing-count)))

(define (parse-macro-classes-string raw)
  (let ((seen (make-hash-table))
        (entries '()))
    (when (string? raw)
      (for-each
       (lambda (token)
         (let ((trimmed (string-trim-both token)))
           (unless (string-null? trimmed)
             (let* ((eq-idx (string-index trimmed #\=))
                    (raw-name (if eq-idx (substring trimmed 0 eq-idx) trimmed))
                    (name (string-trim-both (unescape-alloc-token raw-name)))
                    (val (if eq-idx
                             (or (parse-percent-number
                                  (substring trimmed (+ eq-idx 1)))
                                 0.0)
                             0.0)))
               (unless (or (string-null? name) (hash-ref seen name))
                 (hash-set! seen name #t)
                 (set! entries
                   (cons (cons name (if (> val 1e-6) val 0.0))
                         entries)))))))
       (string-split raw #\;)))
    (reverse entries)))

(define (build-macro-nodes root macro-specs acct->direct acct->macro)
  (let ((macro-table (make-hash-table))
        (ordered-names '())
        (outside-macros-direct 0.0))
    (for-each
     (lambda (spec)
       (let ((name (car spec))
             (explicit (cdr spec)))
         (unless (hash-ref macro-table name)
           ;; entry: (list explicit accounts-sum assigned-accts-rev covered-accts-rev)
           (hash-set! macro-table name (list explicit 0.0 '() '()))
           (set! ordered-names (cons name ordered-names)))))
     macro-specs)

    ;; Walk tree top-down tracking nearest macro class per account (no double-counting).
    (define (walk acct inherited-macro)
      (let* ((own-macro-raw (hash-ref acct->macro acct))
             (own-macro (and own-macro-raw
                             (not (string-null? own-macro-raw))
                             own-macro-raw))
             (eff-macro (or own-macro inherited-macro))
             (direct (or (hash-ref acct->direct acct) 0.0)))
        (if (not eff-macro)
            (set! outside-macros-direct (+ outside-macros-direct direct))
            (begin
              (unless (hash-ref macro-table eff-macro)
                (hash-set! macro-table eff-macro (list 0.0 0.0 '() '()))
                (set! ordered-names (cons eff-macro ordered-names)))
              (let* ((entry (hash-ref macro-table eff-macro))
                     (explicit (list-ref entry 0))
                     (cur-sum (list-ref entry 1))
                     (assigned-rev (list-ref entry 2))
                     (covered-rev (list-ref entry 3))
                     (new-assigned
                      (if (and own-macro (not (equal? own-macro inherited-macro)))
                          (cons acct assigned-rev)
                          assigned-rev)))
                (hash-set! macro-table eff-macro
                           (list explicit
                                 (+ cur-sum direct)
                                 new-assigned
                                 (cons acct covered-rev))))))
        (for-each (cut walk <> eff-macro)
                  (gnc-account-get-children-sorted acct))))

    (when root
      (for-each (cut walk <> #f)
                (gnc-account-get-children-sorted root)))

    (values
     (map
      (lambda (name)
        (let* ((entry (hash-ref macro-table name))
               (explicit (list-ref entry 0))
               (accounts-sum (list-ref entry 1))
               (assigned-accts (reverse (list-ref entry 2)))
               (covered-accts (reverse (list-ref entry 3)))
               (effective (if (> explicit 1e-6) explicit accounts-sum)))
          (make-macro-node name explicit accounts-sum effective
                           assigned-accts covered-accts
                           '() '() 0.0 0.0 0.0 0.0 0.0 0.0)))
      (reverse ordered-names))
     outside-macros-direct)))

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
         (macro-classes-str (get-option pagename-macro-classes optname-macro-classes))

         (show-fullname? (get-option gnc:pagename-display optname-fullname))
         (show-bar-chart? (get-option gnc:pagename-display optname-show-bar-chart))
         (show-time-chart? (get-option gnc:pagename-display optname-show-time-chart))
         (show-table? (get-option gnc:pagename-display optname-show-table))
         (chart-filter-mode (get-option gnc:pagename-display optname-chart-filter-mode))
         (chart-top-n (get-option gnc:pagename-display optname-chart-top-n))
         (chart-min-pct (get-option gnc:pagename-display optname-chart-min-pct))
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
      (lambda (acct->direct acct->macro total-direct missing-count)
        (let ((all-nodes (build-allocation-tree root acct->direct))
              (macro-specs (parse-macro-classes-string macro-classes-str)))
          (call-with-values
              (lambda () (build-macro-nodes root macro-specs acct->direct acct->macro))
            (lambda (all-macro-nodes outside-macros-direct)
              (let* ((total-allocation
                      (+ outside-macros-direct
                         (fold + 0.0 (map macro-effective all-macro-nodes))))
                     (exceeded-macros
                      (filter (lambda (m)
                                (and (> (macro-explicit-target m) 1e-6)
                                     (> (macro-accounts-sum m)
                                        (+ (macro-explicit-target m) 0.01))))
                              all-macro-nodes)))
                (cond
                 ((> missing-count 0)
                  (gnc:html-document-add-object!
                   document
                   (gnc:html-make-generic-warning
                    report-title (gnc:report-id report-obj)
                    (G_ "Missing target accounts")
                    (G_ "One or more configured target accounts no longer exist in this book. Please update Report Options."))))

                 ((<= total-allocation 1e-6)
                  (gnc:html-document-add-object!
                   document
                   (gnc:html-make-generic-warning
                    report-title (gnc:report-id report-obj)
                    (G_ "No target allocations configured")
                    (G_ "Open Report Options (Allocation tab) to assign target percentages to your accounts and/or macro classes so they sum to 100%."))))

                 ((pair? exceeded-macros)
                  (let ((details
                         (string-join
                          (map (lambda (m)
                                 (format #f (G_ "~a: accounts sum ~,2f% exceeds target ~,2f%")
                                         (macro-name m)
                                         (macro-accounts-sum m)
                                         (macro-explicit-target m)))
                               exceeded-macros)
                          "; ")))
                    (gnc:html-document-add-object!
                     document
                     (gnc:html-make-generic-warning
                      report-title (gnc:report-id report-obj)
                      (G_ "Macro class target exceeded")
                      (format #f
                              (G_ "The sum of direct account targets inside a macro class must not exceed the macro class target (~a). Please adjust the percentages in Report Options.")
                              details)))))

                 ((> (abs (- total-allocation 100.0)) 0.01)
                  (let ((diff (- total-allocation 100.0)))
                    (gnc:html-document-add-object!
                     document
                     (gnc:html-make-generic-warning
                      report-title (gnc:report-id report-obj)
                      (G_ "Target allocations must sum to 100%")
                      (format #f
                              (G_ "The total target allocation (accounts outside macro classes + macro classes) is ~,2f% (difference: ~a). Please adjust the percentages in Report Options so they equal 100.00%.")
                              total-allocation
                              (format-signed-pct diff 2))))))

                 ((<= to-date-t64 from-date-t64)
                  (gnc:html-document-add-object!
                   document
                   (gnc:html-make-generic-warning
                    report-title (gnc:report-id report-obj)
                    (G_ "Invalid dates")
                    (G_ "Start date must be earlier than End date."))))

                 (else
                  (let* ((table-nodes all-nodes)
                         (active-macro-nodes
                          (filter (lambda (m)
                                    (or (> (macro-effective m) 1e-6)
                                        (pair? (macro-assigned-accts m))))
                                  all-macro-nodes))
                         (any-macro-assigned?
                          (any (lambda (n) (hash-ref acct->macro (node-acct n)))
                               table-nodes))
                         (all-universe-accts
                          (delete-duplicates
                           (append
                            (append-map node-universe-accts all-nodes)
                            (append-map macro-universe-accts
                                        (filter (lambda (m) (> (macro-effective m) 1e-6))
                                                active-macro-nodes)))
                           (lambda (a b)
                             (string=? (gncAccountGetGUID a) (gncAccountGetGUID b)))))
                         (all-valuation-accts
                          (delete-duplicates
                           (append all-universe-accts
                                   (append-map macro-universe-accts active-macro-nodes))
                           (lambda (a b)
                             (string=? (gncAccountGetGUID a) (gncAccountGetGUID b)))))
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
                                  all-valuation-accts report-currency))
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
             all-valuation-accts)

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

              (for-each
               (lambda (m)
                 (let* ((vals (sum-number-lists
                               (map (cut hash-ref acct->val-series <>)
                                    (macro-universe-accts m))
                               num-dates))
                        (pcts (map (lambda (v tot)
                                     (if (> (abs tot) 1e-9) (/ v tot) 0.0))
                                   vals total-val-series))
                        (actual-val (last vals))
                        (effective (macro-effective m))
                        (target-val (* total-end-val (/ effective 100.0)))
                        (diff-val (- actual-val target-val))
                        (actual-pct (* 100.0 (last pcts)))
                        (diff-pct-pts (- actual-pct effective))
                        (rel-diff-pct (if (> (abs target-val) 1e-9)
                                          (* 100.0 (/ diff-val target-val))
                                          0.0)))
                   (set-macro-val-series! m vals)
                   (set-macro-actual-pct-series! m pcts)
                   (set-macro-target-val! m target-val)
                   (set-macro-actual-val! m actual-val)
                   (set-macro-diff-val! m diff-val)
                   (set-macro-actual-pct! m actual-pct)
                   (set-macro-diff-pct-pts! m diff-pct-pts)
                   (set-macro-rel-diff-pct! m rel-diff-pct)))
               active-macro-nodes)

              (gnc:report-percent-done 80)

              (cond
               ((every (lambda (v) (< (abs v) 1e-6)) total-val-series)
                (gnc:html-document-add-object!
                 document
                 (gnc:html-make-empty-data-warning
                  report-title (gnc:report-id report-obj))))

               (else
                (let* ((depth-nodes
                        (filter (lambda (n) (<= (node-depth n) tree-depth))
                                all-nodes))
                       (chart-candidates-raw
                        (filter (lambda (n)
                                  (not (and (<= (node-direct n) 1e-6)
                                            (>= (node-effective n) 99.99))))
                                depth-nodes))
                       (chart-candidates
                        (if (null? chart-candidates-raw)
                            depth-nodes
                            chart-candidates-raw))
                       (node-chart-score
                        (lambda (n)
                          (max (node-effective n) (node-actual-pct n))))
                       (sorted-candidates
                        (stable-sort
                         chart-candidates
                         (lambda (a b)
                           (let ((sa (node-chart-score a))
                                 (sb (node-chart-score b)))
                             (if (> (abs (- sa sb)) 1e-6)
                                 (> sa sb)
                                 (> (node-effective a) (node-effective b)))))))
                       (chart-nodes
                        (case chart-filter-mode
                          ((top-n)
                           (let ((n-lim (max 1 (inexact->exact (round chart-top-n)))))
                             (take sorted-candidates
                                   (min n-lim (length sorted-candidates)))))
                          ((min-pct)
                           (filter (lambda (n)
                                     (>= (node-chart-score n)
                                         (- chart-min-pct 1e-6)))
                                   sorted-candidates))
                          (else sorted-candidates)))
                       (macro-chart-score
                        (lambda (m)
                          (max (macro-effective m) (macro-actual-pct m))))
                       (chart-macro-nodes
                        (stable-sort
                         (filter (lambda (m) (> (macro-chart-score m) 10.0))
                                 active-macro-nodes)
                         (lambda (a b)
                           (> (macro-chart-score a) (macro-chart-score b)))))
                       ;; Unified chart plot entries: (label target-val actual-val diff-val
                       ;;                              effective actual-pct diff-pct-pts pct-series)
                       (chart-plot-entries
                        (append
                         (map (lambda (n)
                                (list (acct->name (node-acct n))
                                      (node-target-val n)
                                      (node-actual-val n)
                                      (node-diff-val n)
                                      (node-effective n)
                                      (node-actual-pct n)
                                      (node-diff-pct-pts n)
                                      (node-actual-pct-series n)))
                              chart-nodes)
                         (map (lambda (m)
                                (list (string-append (G_ "[Macro] ") (macro-name m))
                                      (macro-target-val m)
                                      (macro-actual-val m)
                                      (macro-diff-val m)
                                      (macro-effective m)
                                      (macro-actual-pct m)
                                      (macro-diff-pct-pts m)
                                      (macro-actual-pct-series m)))
                              chart-macro-nodes)))
                       (filter-subtitle
                        (and (< (length chart-nodes) (length chart-candidates))
                             (case chart-filter-mode
                               ((top-n)
                                (format #f
                                        (G_ "Showing top ~a of ~a allocations")
                                        (length chart-nodes)
                                        (length chart-candidates)))
                               ((min-pct)
                                (format #f
                                        (G_ "Showing ~a of ~a allocations (>= ~,1f%)")
                                        (length chart-nodes)
                                        (length chart-candidates)
                                        chart-min-pct))
                               (else #f)))))

                  ;; Empty chart selection notice when min-% threshold excludes all accounts and no macro > 10%
                  (when (and (null? chart-plot-entries)
                             (or show-bar-chart? show-time-chart?))
                    (gnc:html-document-add-object!
                     document
                     (gnc:make-html-text
                      (gnc:html-markup/attr
                       "p"
                       "style='margin:8px 0;padding:8px 12px;background:#fff8e1;border:1px solid #ffe082;border-radius:4px;'"
                       (format #f
                               (G_ "No accounts meet the chart filter threshold (>= ~,1f%). All target accounts are listed in the table below.")
                               chart-min-pct)))))

                  ;; 1. Bar chart: Target vs Actual & Difference in report currency
                  (when (and show-bar-chart? (pair? chart-plot-entries))
                    (let ((bar-chart (gnc:make-html-chart)))
                      (gnc:html-chart-set-type! bar-chart 'bar)
                      (gnc:html-chart-set-stacking?! bar-chart #f)
                      (gnc:html-chart-set-title!
                       bar-chart
                       (cons (format #f (G_ "~a — Target vs Actual (~a)")
                                     report-title (qof-print-date (last dates-list)))
                             (if filter-subtitle
                                 (list filter-subtitle)
                                 '())))
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
                       (map (lambda (e)
                              (format #f "~a (~,1f% vs ~,1f%, ~a)"
                                      (list-ref e 0)
                                      (list-ref e 5)
                                      (list-ref e 4)
                                      (format-signed-pct (list-ref e 6) 1)))
                            chart-plot-entries))
                      (gnc:html-chart-add-data-series!
                       bar-chart (G_ "Target")
                       (map (cut list-ref <> 1) chart-plot-entries)
                       "#0074D9"
                       'fill #t
                       'borderWidth 1)
                      (gnc:html-chart-add-data-series!
                       bar-chart (G_ "Actual")
                       (map (cut list-ref <> 2) chart-plot-entries)
                       "#2E8B57"
                       'fill #t
                       'borderWidth 1)
                      (gnc:html-chart-add-data-series!
                       bar-chart (G_ "Difference (Actual - Target)")
                       (map (cut list-ref <> 3) chart-plot-entries)
                       (map (lambda (e)
                              (if (>= (list-ref e 3) 0.0) "#2ECC40" "#FF4136"))
                            chart-plot-entries)
                       'fill #t
                       'borderWidth 1)
                      (gnc:html-document-add-object! document bar-chart)))

                  ;; 2. Detailed rebalancing table (always lists all target accounts)
                  (when show-table?
                    (when (pair? table-nodes)
                      (let* ((table (gnc:make-html-table))
                             (min-depth (apply min (map node-depth table-nodes))))
                        (gnc:html-table-set-style!
                         table "table"
                         'attribute (list "border" 0)
                         'attribute (list "cellspacing" 0)
                         'attribute (list "cellpadding" 4))
                        (gnc:html-table-set-col-headers!
                         table
                         (append
                          (list (G_ "Account"))
                          (if any-macro-assigned? (list (G_ "Macro Class")) '())
                          (list (G_ "Direct Target")
                                (G_ "Effective Target")
                                (G_ "Target Value")
                                (G_ "Actual Value")
                                (G_ "Actual %")
                                (G_ "Difference")
                                (G_ "Diff (% pts)")
                                (G_ "Diff vs Target"))))
                        (for-each
                         (lambda (n)
                           (let* ((indent-levels (max 0 (- (node-depth n) min-depth)))
                                  (indent-str (string-concatenate
                                               (make-list (* 3 indent-levels) "&#160;")))
                                  (name-cell (string-append indent-str
                                                            (acct->name (node-acct n))))
                                  (macro-name (or (hash-ref acct->macro (node-acct n)) "—"))
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
                              (append
                               (list name-cell)
                               (if any-macro-assigned? (list macro-name) '())
                               (list
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
                                 (format-signed-pct (node-rel-diff-pct n) 2)))))))
                         table-nodes)
                        (gnc:html-table-append-row/markup!
                         table "grand-total"
                         (append
                          (list (gnc:make-html-table-cell/markup "total-label-cell"
                                                                 (G_ "Total Portfolio")))
                          (if any-macro-assigned?
                              (list (gnc:make-html-table-cell/markup "total-label-cell" ""))
                              '())
                          (list
                           (gnc:make-html-table-cell/markup "total-number-cell"
                                                            (format #f "~,2f%" total-direct))
                           (gnc:make-html-table-cell/markup "total-number-cell"
                                                            (format #f "~,2f%" total-allocation))
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
                           (gnc:make-html-table-cell/markup "total-number-cell" "0.00%"))))
                        (gnc:html-document-add-object! document table)))

                    ;; 2b. Dedicated Macro Class Allocations section
                    (when (pair? active-macro-nodes)
                      (gnc:html-document-add-object!
                       document
                       (gnc:make-html-text
                        (gnc:html-markup "h3" (G_ "Macro Class Allocations"))))
                      (let ((mtable (gnc:make-html-table)))
                        (gnc:html-table-set-style!
                         mtable "table"
                         'attribute (list "border" 0)
                         'attribute (list "cellspacing" 0)
                         'attribute (list "cellpadding" 4))
                        (gnc:html-table-set-col-headers!
                         mtable
                         (list (G_ "Macro Class")
                               (G_ "Accounts")
                               (G_ "Explicit Target")
                               (G_ "Accounts Sum")
                               (G_ "Effective Target")
                               (G_ "Target Value")
                               (G_ "Actual Value")
                               (G_ "Actual %")
                               (G_ "Difference")
                               (G_ "Diff (% pts)")
                               (G_ "Diff vs Target")))
                        (for-each
                         (lambda (m)
                           (let* ((acct-names (string-join
                                               (map acct->name (macro-assigned-accts m))
                                               ", "))
                                  (exp-str (if (> (macro-explicit-target m) 1e-6)
                                               (format #f "~,2f%" (macro-explicit-target m))
                                               "—"))
                                  (sum-str (format #f "~,2f%" (macro-accounts-sum m)))
                                  (eff-str (format #f "~,2f%" (macro-effective m))))
                             (gnc:html-table-append-row!
                              mtable
                              (list (macro-name m)
                                    (if (string-null? acct-names) "—" acct-names)
                                    (gnc:make-html-table-cell/markup "number-cell" exp-str)
                                    (gnc:make-html-table-cell/markup "number-cell" sum-str)
                                    (gnc:make-html-table-cell/markup "number-cell" eff-str)
                                    (gnc:make-html-table-cell/markup
                                     "number-cell"
                                     (float->monetary report-currency (macro-target-val m)))
                                    (gnc:make-html-table-cell/markup
                                     "number-cell"
                                     (float->monetary report-currency (macro-actual-val m)))
                                    (gnc:make-html-table-cell/markup
                                     "number-cell"
                                     (format #f "~,2f%" (macro-actual-pct m)))
                                    (gnc:make-html-table-cell/markup
                                     "number-cell"
                                     (format-signed-currency report-currency (macro-diff-val m)))
                                    (gnc:make-html-table-cell/markup
                                     "number-cell"
                                     (format-signed-pct (macro-diff-pct-pts m) 2))
                                    (gnc:make-html-table-cell/markup
                                     "number-cell"
                                     (format-signed-pct (macro-rel-diff-pct m) 2))))))
                         active-macro-nodes)
                        (when (null? table-nodes)
                          (gnc:html-table-append-row/markup!
                           mtable "grand-total"
                           (list (gnc:make-html-table-cell/markup "total-label-cell"
                                                                  (G_ "Total Portfolio"))
                                 (gnc:make-html-table-cell/markup "total-label-cell" "")
                                 (gnc:make-html-table-cell/markup "total-number-cell" "—")
                                 (gnc:make-html-table-cell/markup "total-number-cell" "0.00%")
                                 (gnc:make-html-table-cell/markup
                                  "total-number-cell"
                                  (format #f "~,2f%" total-allocation))
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
                                 (gnc:make-html-table-cell/markup "total-number-cell" "0.00%"))))
                        (gnc:html-document-add-object! document mtable))))

                  ;; 3. Over-time line chart: solid Actual % vs dashed Target % with matching color
                  (when (and show-time-chart? (pair? chart-plot-entries))
                    (let ((line-chart (gnc:make-html-chart))
                          (colors (gnc:assign-colors (length chart-plot-entries))))
                      (gnc:html-chart-set-type! line-chart 'line)
                      (gnc:html-chart-set-title!
                       line-chart
                       (cons (format #f (G_ "~a — Allocation Over Time") report-title)
                             (cons (gnc-date-interval-format from-date-t64 to-date-t64)
                                   (if filter-subtitle
                                       (list filter-subtitle)
                                       '()))))
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
                       (lambda (e color)
                         (let ((label (list-ref e 0))
                               (effective (list-ref e 4))
                               (pct-series (list-ref e 7)))
                           (gnc:html-chart-add-data-series!
                            line-chart
                            label
                            pct-series
                            color
                            'fill #f
                            'borderWidth line-width)
                           (gnc:html-chart-add-data-series!
                            line-chart
                            (format #f (G_ "~a (Target)") label)
                            (make-list num-dates (/ effective 100.0))
                            color
                            'fill #f
                            'borderWidth line-width
                            'borderDash '(6 4)
                            'pointRadius 0)))
                       chart-plot-entries
                       colors)

                      (gnc:html-document-add-object! document line-chart)))

                  ;; 4. CSV export
                  (when (eq? export-type 'csv)
                    (let* ((acct-csv-rows
                            (if (null? table-nodes)
                                '()
                                (cons
                                 (append
                                  (list (G_ "Account"))
                                  (if any-macro-assigned? (list (G_ "Macro Class")) '())
                                  (list (G_ "Direct Target (%)")
                                        (G_ "Effective Target (%)")
                                        (G_ "Target Value")
                                        (G_ "Actual Value")
                                        (G_ "Actual (%)")
                                        (G_ "Difference")
                                        (G_ "Diff (% pts)")
                                        (G_ "Diff vs Target (%)")))
                                 (append
                                  (map (lambda (n)
                                         (append
                                          (list (gnc-account-get-full-name (node-acct n)))
                                          (if any-macro-assigned?
                                              (list (or (hash-ref acct->macro (node-acct n)) ""))
                                              '())
                                          (list (node-direct n)
                                                (node-effective n)
                                                (node-target-val n)
                                                (node-actual-val n)
                                                (node-actual-pct n)
                                                (node-diff-val n)
                                                (node-diff-pct-pts n)
                                                (node-rel-diff-pct n))))
                                       table-nodes)
                                  (list
                                   (append
                                    (list (G_ "Total Portfolio"))
                                    (if any-macro-assigned? (list "") '())
                                    (list total-direct
                                          total-allocation
                                          total-end-val
                                          total-end-val
                                          100.0
                                          0.0
                                          0.0
                                          0.0)))))))
                           (macro-csv-rows
                            (if (null? active-macro-nodes)
                                '()
                                (append
                                 (if (null? table-nodes) '() (list '()))
                                 (cons (list (G_ "Macro Class")
                                             (G_ "Accounts")
                                             (G_ "Explicit Target (%)")
                                             (G_ "Accounts Sum (%)")
                                             (G_ "Effective Target (%)")
                                             (G_ "Target Value")
                                             (G_ "Actual Value")
                                             (G_ "Actual (%)")
                                             (G_ "Difference")
                                             (G_ "Diff (% pts)")
                                             (G_ "Diff vs Target (%)"))
                                       (map (lambda (m)
                                              (list (macro-name m)
                                                    (string-join
                                                     (map gnc-account-get-full-name
                                                          (macro-assigned-accts m))
                                                     ", ")
                                                    (macro-explicit-target m)
                                                    (macro-accounts-sum m)
                                                    (macro-effective m)
                                                    (macro-target-val m)
                                                    (macro-actual-val m)
                                                    (macro-actual-pct m)
                                                    (macro-diff-val m)
                                                    (macro-diff-pct-pts m)
                                                    (macro-rel-diff-pct m)))
                                            active-macro-nodes))))))
                      (gnc:html-document-set-export-string
                       document
                       (gnc:lists->csv (append acct-csv-rows macro-csv-rows))))))))))))))))))

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
