;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test-asset-allocation.scm: Tests for Asset Allocation report.
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
(use-modules (gnucash json parser))
(use-modules (gnucash report))
(use-modules (gnucash report stylesheets plain))
(use-modules (gnucash reports standard asset-allocation))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-64))
(use-modules (ice-9 format))
(use-modules (sxml xpath))
(use-modules (tests test-engine-extras))
(use-modules (tests srfi64-extras))
(use-modules (tests test-report-extras))

(setlocale LC_ALL "C")

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-asset-allocation")
  (qof-date-format-set QOF-DATE-FORMAT-ISO)
  (null-and-validation-test)
  (recursive-parent-and-children-test)
  (parent-zero-children-targeted-test)
  (parent-targeted-children-zero-satisfied-test)
  (over-time-chart-colors-and-dashed-target-test)
  (chart-filtering-test)
  (csv-export-test)
  (test-end "test-asset-allocation"))

(define (set-option! options section name value)
  (if (gnc-lookup-option (gnc:optiondb options) section name)
      (gnc-set-option (gnc:optiondb options) section name value)
      (test-assert (format #f "wrong-option ~a ~a" section name) #f)))

(define (teardown)
  (gnc-clear-current-session))

(define (alloc-pair acct pct)
  (format #f "~a=~,2f" (gncAccountGetGUID acct) pct))

(define (alloc-spec . pairs)
  (string-join pairs ";"))

(define (extract-chartjs-json-list html-str)
  (let ((marker "var chartjsoptions = "))
    (let loop ((start 0) (acc '()))
      (let ((idx (string-contains html-str marker start)))
        (if (not idx)
            (reverse acc)
            (let* ((json-start (+ idx (string-length marker)))
                   (json-end (string-contains html-str "\n// The following" json-start)))
              (if (not json-end)
                  (reverse acc)
                  (let* ((raw-json (substring html-str json-start json-end))
                         (trimmed-json (string-trim-right
                                        raw-json
                                        (lambda (c)
                                          (or (char-whitespace? c) (char=? c #\;)))))
                         (parsed (json-string->scm trimmed-json)))
                    (loop json-end (cons parsed acc))))))))))

(define (find-table-row sxml row-name)
  (find
   (lambda (row)
     (and (pair? row)
          (eq? (car row) 'tr)
          (let ((cells ((sxpath '(td)) row)))
            (and (pair? cells)
                 (string-contains (sxml->text (car cells)) row-name)))))
   ((sxpath '(// table tbody tr)) sxml)))

(define (sxml->text node)
  (cond
   ((string? node) node)
   ((and (pair? node) (eq? (car node) '@)) "")
   ((pair? node) (string-concatenate (map sxml->text (cdr node))))
   (else "")))

(define (null-and-validation-test)
  (test-group-with-cleanup "null-and-validation"
    (let* ((env (create-test-env))
           (book (gnc-get-current-book))
           (accounts (env-create-account-structure-alist
                      env
                      (list "Root"
                            (list (cons 'type ACCT-TYPE-ASSET))
                            (list "Bank" (list (cons 'type ACCT-TYPE-BANK)))
                            (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY))))))
           (bank (cdr (assoc "Bank" accounts)))
           (equity (cdr (assoc "Equity" accounts))))
      (env-transfer env 10 01 2025 equity bank 1000)

      ;; Default empty allocations -> warning without crashing
      (let* ((options (gnc:make-report-options asset-allocation-uuid))
             (sxml (gnc:options->sxml asset-allocation-uuid options
                                      "test-asset-allocation" "empty-alloc"
                                      #:strip-tag "script")))
        (test-assert "empty allocation shows warning"
          (string-contains (sxml->text sxml) "No target allocations configured")))

      ;; Sum != 100% -> validation error message
      (let ((options (gnc:make-report-options asset-allocation-uuid)))
        (set-option! options "Allocation" "Target Allocations"
                     (alloc-spec (alloc-pair bank 80.0)))
        (let ((sxml (gnc:options->sxml asset-allocation-uuid options
                                       "test-asset-allocation" "bad-sum"
                                       #:strip-tag "script")))
          (test-assert "non-100 sum shows validation error"
            (and (string-contains (sxml->text sxml) "must sum to 100%")
                 (string-contains (sxml->text sxml) "80.00%"))))))
    (teardown)))

;; User Example 1: Parent P (5%), Child 1 (10%), Child 2 (2%) -> Parent effective target = 17%
(define (recursive-parent-and-children-test)
  (test-group-with-cleanup "recursive-parent-and-children"
    (let* ((env (create-test-env))
           (book (gnc-get-current-book))
           (accounts (env-create-account-structure-alist
                      env
                      (list "Root"
                            (list (cons 'type ACCT-TYPE-ASSET))
                            (list "Assets"
                                  (list "ParentP"
                                        (list "Child1")
                                        (list "Child2"))
                                  (list "Bonds"))
                            (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY))))))
           (parent-p (cdr (assoc "ParentP" accounts)))
           (child1 (cdr (assoc "Child1" accounts)))
           (child2 (cdr (assoc "Child2" accounts)))
           (bonds (cdr (assoc "Bonds" accounts)))
           (equity (cdr (assoc "Equity" accounts))))
      (env-transfer env 05 01 2025 equity parent-p 100)
      (env-transfer env 05 01 2025 equity child1 100)
      (env-transfer env 05 01 2025 equity child2 20)
      (env-transfer env 05 01 2025 equity bonds 780)

      (let ((options (gnc:make-report-options asset-allocation-uuid)))
        (set-option! options "General" "Start Date"
                     (cons 'absolute (gnc-dmy2time64 01 01 2025)))
        (set-option! options "General" "End Date"
                     (cons 'absolute (gnc-dmy2time64 31 03 2025)))
        (set-option! options "Allocation" "Target Allocations"
                     (alloc-spec (alloc-pair parent-p 5.0)
                                 (alloc-pair child1 10.0)
                                 (alloc-pair child2 2.0)
                                 (alloc-pair bonds 83.0)))
        (let* ((sxml (gnc:options->sxml asset-allocation-uuid options
                                        "test-asset-allocation" "ex1"
                                        #:strip-tag "script"))
               (parent-row (find-table-row sxml "ParentP"))
               (parent-cells (map sxml->text ((sxpath '(td)) parent-row)))
               (child1-row (find-table-row sxml "Child1"))
               (child1-cells (map sxml->text ((sxpath '(td)) child1-row))))
          (test-equal "ParentP direct target is 5.00%" "5.00%" (list-ref parent-cells 1))
          (test-equal "ParentP effective target is 17.00%" "17.00%" (list-ref parent-cells 2))
          (test-assert "ParentP target value is 170.00"
            (string-contains (list-ref parent-cells 3) "170.00"))
          (test-assert "ParentP actual value is 220.00"
            (string-contains (list-ref parent-cells 4) "220.00"))
          (test-equal "ParentP actual % is 22.00%" "22.00%" (list-ref parent-cells 5))
          (test-equal "ParentP diff % pts is +5.00%" "+5.00%" (list-ref parent-cells 7))
          (test-equal "Child1 effective target is 10.00%" "10.00%" (list-ref child1-cells 2))
          (test-equal "Child1 actual % is 10.00%" "10.00%" (list-ref child1-cells 5)))))
    (teardown)))

;; User Example 2: Parent Stocks (0%), Stock1 (5%), Stock2 (2%) -> Stocks effective target = 7%,
;; untargeted Stock3 & Checking are excluded from allocated universe.
(define (parent-zero-children-targeted-test)
  (test-group-with-cleanup "parent-zero-children-targeted"
    (let* ((env (create-test-env))
           (book (gnc-get-current-book))
           (accounts (env-create-account-structure-alist
                      env
                      (list "Root"
                            (list (cons 'type ACCT-TYPE-ASSET))
                            (list "Assets"
                                  (list "Stocks"
                                        (list "Stock1")
                                        (list "Stock2")
                                        (list "UntargetedStock"))
                                  (list "Bonds")
                                  (list "Checking"))
                            (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY))))))
           (stocks (cdr (assoc "Stocks" accounts)))
           (stock1 (cdr (assoc "Stock1" accounts)))
           (stock2 (cdr (assoc "Stock2" accounts)))
           (untargeted-stock (cdr (assoc "UntargetedStock" accounts)))
           (bonds (cdr (assoc "Bonds" accounts)))
           (checking (cdr (assoc "Checking" accounts)))
           (equity (cdr (assoc "Equity" accounts))))
      (env-transfer env 05 01 2025 equity stock1 50)
      (env-transfer env 05 01 2025 equity stock2 20)
      (env-transfer env 05 01 2025 equity untargeted-stock 500)
      (env-transfer env 05 01 2025 equity bonds 930)
      (env-transfer env 05 01 2025 equity checking 500)

      (let ((options (gnc:make-report-options asset-allocation-uuid)))
        (set-option! options "General" "Start Date"
                     (cons 'absolute (gnc-dmy2time64 01 01 2025)))
        (set-option! options "General" "End Date"
                     (cons 'absolute (gnc-dmy2time64 31 03 2025)))
        (set-option! options "Allocation" "Target Allocations"
                     (alloc-spec (alloc-pair stock1 5.0)
                                 (alloc-pair stock2 2.0)
                                 (alloc-pair bonds 93.0)))
        (let* ((sxml (gnc:options->sxml asset-allocation-uuid options
                                        "test-asset-allocation" "ex2"
                                        #:strip-tag "script"))
               (stocks-row (find-table-row sxml "Stocks"))
               (stocks-cells (map sxml->text ((sxpath '(td)) stocks-row))))
          (test-equal "Stocks effective target is 7.00%" "7.00%" (list-ref stocks-cells 2))
          (test-assert "Stocks actual value excludes UntargetedStock (70.00)"
            (string-contains (list-ref stocks-cells 4) "70.00"))
          (test-equal "Stocks actual % is 7.00%" "7.00%" (list-ref stocks-cells 5))
          (test-equal "Stocks diff % pts is 0.00%" "0.00%" (list-ref stocks-cells 7))
          (test-assert "Untargeted Checking excluded from table"
            (not (find-table-row sxml "Checking")))
          (test-assert "Total excludes UntargetedStock & Checking ($1,000.00)"
            (string-contains
             (sxml->text (find-table-row sxml "Total Portfolio"))
             "1,000.00")))))
    (teardown)))

;; User Example 3: Parent Stocks (10%), Child1 (0%) holds 5%, Child2 (0%) holds 5% -> satisfied!
(define (parent-targeted-children-zero-satisfied-test)
  (test-group-with-cleanup "parent-targeted-children-zero-satisfied"
    (let* ((env (create-test-env))
           (book (gnc-get-current-book))
           (accounts (env-create-account-structure-alist
                      env
                      (list "Root"
                            (list (cons 'type ACCT-TYPE-ASSET))
                            (list "Assets"
                                  (list "Stocks"
                                        (list "SubStock1")
                                        (list "SubStock2"))
                                  (list "Bonds"))
                            (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY))))))
           (stocks (cdr (assoc "Stocks" accounts)))
           (sub1 (cdr (assoc "SubStock1" accounts)))
           (sub2 (cdr (assoc "SubStock2" accounts)))
           (bonds (cdr (assoc "Bonds" accounts)))
           (equity (cdr (assoc "Equity" accounts))))
      (env-transfer env 05 01 2025 equity sub1 50)
      (env-transfer env 05 01 2025 equity sub2 50)
      (env-transfer env 05 01 2025 equity bonds 900)

      (let ((options (gnc:make-report-options asset-allocation-uuid)))
        (set-option! options "General" "Start Date"
                     (cons 'absolute (gnc-dmy2time64 01 01 2025)))
        (set-option! options "General" "End Date"
                     (cons 'absolute (gnc-dmy2time64 31 03 2025)))
        (set-option! options "Allocation" "Target Allocations"
                     (alloc-spec (alloc-pair stocks 10.0)
                                 (alloc-pair bonds 90.0)))
        (let* ((sxml (gnc:options->sxml asset-allocation-uuid options
                                        "test-asset-allocation" "ex3"
                                        #:strip-tag "script"))
               (stocks-row (find-table-row sxml "Stocks"))
               (stocks-cells (map sxml->text ((sxpath '(td)) stocks-row))))
          (test-equal "Stocks effective target is 10.00%" "10.00%" (list-ref stocks-cells 2))
          (test-assert "Stocks target value is 100.00"
            (string-contains (list-ref stocks-cells 3) "100.00"))
          (test-assert "Stocks actual value sums children (100.00)"
            (string-contains (list-ref stocks-cells 4) "100.00"))
          (test-equal "Stocks actual % is 10.00%" "10.00%" (list-ref stocks-cells 5))
          (test-equal "Stocks diff % pts is 0.00%" "0.00%" (list-ref stocks-cells 7)))))
    (teardown)))

;; Verify Chart 1 (bar) & Chart 2 (over-time line with solid actual & dashed target sharing color)
(define (over-time-chart-colors-and-dashed-target-test)
  (test-group-with-cleanup "over-time-chart-colors-and-dashed-target"
    (let* ((env (create-test-env))
           (book (gnc-get-current-book))
           (accounts (env-create-account-structure-alist
                      env
                      (list "Root"
                            (list (cons 'type ACCT-TYPE-ASSET))
                            (list "Equities")
                            (list "FixedIncome")
                            (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY))))))
           (eq-acct (cdr (assoc "Equities" accounts)))
           (fi-acct (cdr (assoc "FixedIncome" accounts)))
           (equity (cdr (assoc "Equity" accounts))))
      ;; Jan 2025: 50% / 50%
      (env-transfer env 05 01 2025 equity eq-acct 500)
      (env-transfer env 05 01 2025 equity fi-acct 500)
      ;; Mar 2025: Equities grows to 600, Total = 1100
      (env-transfer env 05 03 2025 equity eq-acct 100)

      (let ((options (gnc:make-report-options asset-allocation-uuid)))
        (set-option! options "General" "Start Date"
                     (cons 'absolute (gnc-dmy2time64 31 01 2025)))
        (set-option! options "General" "End Date"
                     (cons 'absolute (gnc-dmy2time64 31 03 2025)))
        (set-option! options "General" "Step Size" 'MonthDelta)
        (set-option! options "Allocation" "Target Allocations"
                     (alloc-spec (alloc-pair eq-acct 40.0)
                                 (alloc-pair fi-acct 60.0)))
        (let* ((html (gnc:options->render asset-allocation-uuid options
                                          "test-asset-allocation" "charts"))
               (charts (extract-chartjs-json-list html)))
          (test-equal "renders 2 Chart.js charts (bar + line)" 2 (length charts))
          (let* ((bar-chart (car charts))
                 (line-chart (cadr charts))
                 (line-datasets (vector->list
                                 (assoc-ref (assoc-ref line-chart "data") "datasets")))
                 (find-ds (lambda (lbl)
                            (find (lambda (d) (equal? (assoc-ref d "label") lbl))
                                  line-datasets)))
                 (eq-actual (find-ds "Equities"))
                 (eq-target (find-ds "Equities (Target)"))
                 (fi-actual (find-ds "FixedIncome"))
                 (fi-target (find-ds "FixedIncome (Target)")))
            (test-equal "first chart is bar" "bar" (assoc-ref bar-chart "type"))
            (test-equal "second chart is line" "line" (assoc-ref line-chart "type"))
            (test-assert "Equities solid & target datasets exist" (and eq-actual eq-target))
            (test-assert "FixedIncome solid & target datasets exist" (and fi-actual fi-target))
            (test-equal "Equities actual & target share exact same color"
              (assoc-ref eq-actual "borderColor")
              (assoc-ref eq-target "borderColor"))
            (test-equal "FixedIncome actual & target share exact same color"
              (assoc-ref fi-actual "borderColor")
              (assoc-ref fi-target "borderColor"))
            (test-assert "Actual line is solid (no borderDash)"
              (not (assoc-ref eq-actual "borderDash")))
            (test-equal "Target line is dashed [6, 4]"
              #(6 4)
              (assoc-ref eq-target "borderDash"))
            ;; Verify over-time progression for Equities: Jan=0.5, end>=0.54
            (let ((eq-vals (vector->list (assoc-ref eq-actual "data"))))
              (test-assert "Jan allocation is 50%" (< (abs (- (car eq-vals) 0.5)) 1e-6))
              (test-assert "Mar allocation > 54%" (> (last eq-vals) 0.54)))))))
    (teardown)))

(define (csv-export-test)
  (test-group-with-cleanup "csv-export"
    (let* ((env (create-test-env))
           (book (gnc-get-current-book))
           (accounts (env-create-account-structure-alist
                      env
                      (list "Root"
                            (list (cons 'type ACCT-TYPE-ASSET))
                            (list "Equities")
                            (list "Bonds")
                            (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY))))))
           (eq-acct (cdr (assoc "Equities" accounts)))
           (bonds (cdr (assoc "Bonds" accounts)))
           (equity (cdr (assoc "Equity" accounts))))
      (env-transfer env 10 01 2025 equity eq-acct 700)
      (env-transfer env 10 01 2025 equity bonds 300)

      (let ((options (gnc:make-report-options asset-allocation-uuid)))
        (set-option! options "General" "Start Date"
                     (cons 'absolute (gnc-dmy2time64 01 01 2025)))
        (set-option! options "General" "End Date"
                     (cons 'absolute (gnc-dmy2time64 31 03 2025)))
        (set-option! options "Allocation" "Target Allocations"
                     (alloc-spec (alloc-pair eq-acct 60.0)
                                 (alloc-pair bonds 40.0)))
        (let* ((template (gnc:find-report-template asset-allocation-uuid))
               (report-id (gnc:make-report asset-allocation-uuid options))
               (report-obj (gnc-report-find report-id))
               (doc ((gnc:report-template-export-thunk template) report-obj 'csv))
               (csv (gnc:html-document-export-string doc)))
          (test-assert "CSV export contains headers and accounts"
            (and (string? csv)
                 (string-contains csv "Direct Target (%)")
                 (string-contains csv "Effective Target (%)")
                 (string-contains csv "Equities")
                 (string-contains csv "Total Portfolio"))))))
    (teardown)))

(define (chart-filtering-test)
  (test-group-with-cleanup "chart-filtering"
    (let* ((env (create-test-env))
           (book (gnc-get-current-book))
           (accounts (env-create-account-structure-alist
                      env
                      (list "Root"
                            (list (cons 'type ACCT-TYPE-ASSET))
                            (list "A1")
                            (list "A2")
                            (list "A3")
                            (list "A4")
                            (list "A5")
                            (list "A6")
                            (list "A7")
                            (list "A8")
                            (list "Equity" (list (cons 'type ACCT-TYPE-EQUITY))))))
           (names '("A1" "A2" "A3" "A4" "A5" "A6" "A7" "A8"))
           (pcts  '(25.0 20.0 15.0 12.0 10.0 8.0 6.0 4.0))
           (equity (cdr (assoc "Equity" accounts))))
      (for-each
       (lambda (name pct)
         (env-transfer env 10 01 2025 equity (cdr (assoc name accounts))
                       (inexact->exact (round (* pct 10)))))
       names pcts)

      (let ((alloc-val (apply alloc-spec
                              (map (lambda (name pct)
                                     (alloc-pair (cdr (assoc name accounts)) pct))
                                   names pcts))))
        ;; 1. Default ('top-n = 6): charts show 6 largest, table shows all 8
        (let ((options (gnc:make-report-options asset-allocation-uuid)))
          (set-option! options "General" "Start Date"
                       (cons 'absolute (gnc-dmy2time64 01 01 2025)))
          (set-option! options "General" "End Date"
                       (cons 'absolute (gnc-dmy2time64 31 03 2025)))
          (set-option! options "Allocation" "Target Allocations" alloc-val)
          (let* ((html (gnc:options->render asset-allocation-uuid options
                                            "test-asset-allocation" "filter-default"))
                 (charts (extract-chartjs-json-list html))
                 (sxml (gnc:options->sxml asset-allocation-uuid options
                                          "test-asset-allocation" "filter-default"
                                          #:strip-tag "script"))
                 (bar-labels (vector->list
                              (assoc-ref (assoc-ref (car charts) "data") "labels")))
                 (line-datasets (vector->list
                                 (assoc-ref (assoc-ref (cadr charts) "data") "datasets"))))
            (test-equal "default filter shows 6 bars" 6 (length bar-labels))
            (test-equal "default filter shows 12 line datasets (6 actual + 6 target)"
              12 (length line-datasets))
            (test-assert "table still includes small accounts A7 and A8"
              (and (find-table-row sxml "A7")
                   (find-table-row sxml "A8")))))

        ;; 2. Minimum % filter (Y = 10.0% -> A1..A5 = 5 accounts in charts, all 8 in table)
        (let ((options (gnc:make-report-options asset-allocation-uuid)))
          (set-option! options "General" "Start Date"
                       (cons 'absolute (gnc-dmy2time64 01 01 2025)))
          (set-option! options "General" "End Date"
                       (cons 'absolute (gnc-dmy2time64 31 03 2025)))
          (set-option! options "Allocation" "Target Allocations" alloc-val)
          (set-option! options "Display" "Chart Account Filter" 'min-pct)
          (set-option! options "Display" "Minimum Allocation in Charts (%)" 10.0)
          (let* ((html (gnc:options->render asset-allocation-uuid options
                                            "test-asset-allocation" "filter-min"))
                 (charts (extract-chartjs-json-list html))
                 (sxml (gnc:options->sxml asset-allocation-uuid options
                                          "test-asset-allocation" "filter-min"
                                          #:strip-tag "script"))
                 (bar-labels (vector->list
                              (assoc-ref (assoc-ref (car charts) "data") "labels")))
                 (line-datasets (vector->list
                                 (assoc-ref (assoc-ref (cadr charts) "data") "datasets"))))
            (test-equal "min-pct=10% shows 5 bars" 5 (length bar-labels))
            (test-equal "min-pct=10% shows 10 line datasets" 10 (length line-datasets))
            (test-assert "table still includes A6, A7, A8"
              (and (find-table-row sxml "A6")
                   (find-table-row sxml "A7")
                   (find-table-row sxml "A8")))))

        ;; 3. 'all mode -> all 8 accounts in charts
        (let ((options (gnc:make-report-options asset-allocation-uuid)))
          (set-option! options "General" "Start Date"
                       (cons 'absolute (gnc-dmy2time64 01 01 2025)))
          (set-option! options "General" "End Date"
                       (cons 'absolute (gnc-dmy2time64 31 03 2025)))
          (set-option! options "Allocation" "Target Allocations" alloc-val)
          (set-option! options "Display" "Chart Account Filter" 'all)
          (let* ((html (gnc:options->render asset-allocation-uuid options
                                            "test-asset-allocation" "filter-all"))
                 (charts (extract-chartjs-json-list html))
                 (bar-labels (vector->list
                              (assoc-ref (assoc-ref (car charts) "data") "labels")))
                 (line-datasets (vector->list
                                 (assoc-ref (assoc-ref (cadr charts) "data") "datasets"))))
            (test-equal "all mode shows 8 bars" 8 (length bar-labels))
            (test-equal "all mode shows 16 line datasets" 16 (length line-datasets))))

        ;; 4. 'min-pct = 90.0% -> 0 charts + notice + full table intact
        (let ((options (gnc:make-report-options asset-allocation-uuid)))
          (set-option! options "General" "Start Date"
                       (cons 'absolute (gnc-dmy2time64 01 01 2025)))
          (set-option! options "General" "End Date"
                       (cons 'absolute (gnc-dmy2time64 31 03 2025)))
          (set-option! options "Allocation" "Target Allocations" alloc-val)
          (set-option! options "Display" "Chart Account Filter" 'min-pct)
          (set-option! options "Display" "Minimum Allocation in Charts (%)" 90.0)
          (let* ((html (gnc:options->render asset-allocation-uuid options
                                            "test-asset-allocation" "filter-none"))
                 (charts (extract-chartjs-json-list html))
                 (sxml (gnc:options->sxml asset-allocation-uuid options
                                          "test-asset-allocation" "filter-none"
                                          #:strip-tag "script")))
            (test-equal "min-pct=90% renders 0 charts" 0 (length charts))
            (test-assert "shows threshold notice"
              (string-contains (sxml->text sxml) "No accounts meet the chart filter threshold"))
            (test-assert "full table still shows A1 and A8"
              (and (find-table-row sxml "A1")
                   (find-table-row sxml "A8")))))))
    (teardown)))
