(in-package :polyforms)

;; STOCK OPTION PLANS
(defun exercisable-p (date-record date)
  (let ((exercised 0))
    (if (fifth date-record)
        (setf exercised (fifth date-record)))
    (floor (min (- (second date-record) exercised)
                (- (* (floor (/ (- date (first date-record)) 30.5))
                      (second date-record) ; number of options
                      0.02778) ; amount per month
                   exercised)))))

(defun outstanding-grants (&optional (date nil))
  (setf date (get-absolute-date date))
  (reduce #'+ (remove-if-not #'(lambda (date-in-list)
                                 (> date date-in-list))
                             *option-grant-dates*
                             :key #'first)
          :key #'(lambda (date-record)
                   (if (fifth date-record)
                       (- (second date-record) (fifth date-record))
                       (second date-record)))))

(defun exercisable-grants (&optional (date nil))
  (setf date (get-absolute-date date))
  (reduce #'+ (remove-if-not #'(lambda (date-in-list)
                                 (> date (+ date-in-list 366)))
                             *option-grant-dates*
                             :key #'first)
          :key #'(lambda (date-record)
                   (exercisable-p date-record date))))

(defun gross-proceeds (&key (stock-price nil) (date nil))
  (setf date (get-absolute-date date))
  (unless stock-price
    (if (> date (today))
        (setf stock-price (get-stock-quote *my-stock* (today) nil))
        (setf stock-price (get-stock-quote *my-stock* date nil))))
  (reduce #'+ (remove-if-not #'(lambda (date-in-list)
                                 (> date (+ date-in-list 366)))
                             *option-grant-dates*
                             :key #'first)
          :key #'(lambda (date-record)
                   (* (exercisable-p date-record date)
                      (if (> stock-price (third date-record))
                          stock-price 0)))))

(defun exercisable-value (&key (stock-price nil) (date nil))
  (setf date (get-absolute-date date))
  (unless stock-price
    (if (> date (today))
        (setf stock-price (get-stock-quote *my-stock* (today) nil))
        (setf stock-price (get-stock-quote *my-stock* date nil))))
  (reduce #'+ (remove-if-not #'(lambda (date-in-list)
                                 (> date (+ date-in-list 366)))
                             *option-grant-dates*
                             :key #'first)
          :key #'(lambda (date-record)
                   (* (exercisable-p date-record date)
                      (max (- stock-price (third date-record)) 0)))))

(defun estimated-proceeds (&key (stock-price nil) (date nil))
  (setf date (get-absolute-date date))
  (unless stock-price
    (if (> date (today))
        (setf stock-price (get-stock-quote *my-stock* (today) nil))
        (setf stock-price (get-stock-quote *my-stock* date nil))))
  (let* ((total-value (exercisable-value :stock-price stock-price :date date))
         (gross-proceeds (gross-proceeds :stock-price stock-price :date date))
         (total-taxes (* total-value 0.1600)) ; federal income tax = 16.00%
         (other-fee (* gross-proceeds 0.00001317))) ; fee $25.70 per million?
    (- total-value total-taxes other-fee *options-commission-and-fee*))) ; $25

;; PURCHASE PLAN
(defun espp-available-for-sale ()
  *purchased-units*)

(defun espp-market-value (&key (stock-price nil))
  (if stock-price
      (* *purchased-units* stock-price)
      (* *purchased-units* (get-stock-quote *my-stock*))))

;; RESTRICTED STOCK UNITS
(defun units-unvested (&optional (date nil))
  (setf date (get-absolute-date date))
  (reduce #'+ (remove-if-not #'(lambda (date-in-list)
                                 (< date date-in-list))
                             *unit-vesting-dates*
                             :key #'first)
          :key #'second))

(defun units-available-for-sale (&optional (date nil))
  (setf date (get-absolute-date date))
  (max (- (reduce #'+ (remove-if-not #'(lambda (date-in-list)
                                         (>= date date-in-list))
                                     *unit-vesting-dates*
                                     :key #'first)
                  :key #'second) *sold-units*) 0))

(defun units-market-value (&key (stock-price nil) (date nil) (taxes t))
  (setf date (get-absolute-date date))
  (unless stock-price
    (if (> date (today))
        (setf stock-price (get-stock-quote *my-stock* (today) nil))
        (setf stock-price (get-stock-quote *my-stock* date nil))))
  (* (units-available-for-sale date) stock-price))

;; POTENTIAL TOTAL
(defun potential-total-options (&key (stock-price nil) (date 999999))
  (setf date (get-absolute-date date))
  (unless stock-price
    (if (> date (today))
        (setf stock-price (get-stock-quote *my-stock* (today) nil))))
  (exercisable-value :date date :stock-price stock-price))

(defun potential-total-units (&key (stock-price nil) (date 999999))
  (setf date (get-absolute-date date))
  (unless stock-price
    (if (> date (today))
        (setf stock-price (get-stock-quote *my-stock* (today) nil))))
  (units-market-value :date date :stock-price stock-price))

(defun potential-total-purchased (&key (stock-price nil))
  (unless stock-price
    (if (> date (today))
        (setf stock-price (get-stock-quote *my-stock* (today) nil))))
  (espp-market-value :stock-price stock-price))

(defun potential-total (&key (stock-price nil) (date 999999))
  (setf date (get-absolute-date date))
  (unless stock-price
    (if (> date (today))
        (setf stock-price (get-stock-quote *my-stock* (today) nil))
        (setf stock-price (get-stock-quote *my-stock* date nil))))
  (+ (exercisable-value :date date :stock-price stock-price)
     (units-market-value :date date :stock-price stock-price)
     (espp-market-value :stock-price stock-price)))


;; Getting stock quotes from Yahoo Finance
;; (defun get-stock-quote (&optional (symbol "INTU") (date nil) (fmt? t))
;;   (setf date (get-absolute-date date))
;;   (when (null symbol)
;;     (error "You must specify a symbol."))
;;   (if fmt? (format t "~&Sending query to Yahoo Finance... "))
;;   (let* ((url-base "http://ichart.finance.yahoo.com/table.csv")
;;          (date1 (get-gregorian-date (find-prev-work-day date)))
;;          (query (query-to-form-urlencoded
;;                  `(("s" . ,symbol)
;;                    ("a" . ,(format nil "~a" (1- (extract-month date1))))
;;                    ("b" . ,(format nil "~a" (extract-day date1)))
;;                    ("c" . ,(format nil "~a" (extract-year date1)))
;;                    ("d" . ,(format nil "~a" (1- (extract-month date1))))
;;                    ("e" . ,(format nil "~a" (extract-day date1)))
;;                    ("f" . ,(format nil "~a" (extract-year date1))))))
;;          (result (values (s-http-client:do-http-request
;;                           (format nil "~a?~a" url-base query)))))
;;     (when (and result (stringp result))
;;       (multiple-value-bind (last)
;;           (match-re ",([0-9.]+)$" result)
;;         (when (and last (stringp last))
;;           (let ((new-last (read-from-string last)))
;;             (max new-last 0)))))))
;; (memoize 'get-stock-quote :key #'(lambda (x) (list (first x) (second x))) :test #'equalp)


;; SAMPLE PLOTS
(defun plot-stock-quotes (from to &key (stock "INTU") (min 28.75) (scale 25))
  (asterisk-plot (lambda (x) (get-stock-quote stock x nil))
                 (get-absolute-date from)
                 (get-absolute-date to)
                 #'find-next-work-day
                 :fmt-x-axis #'fmt-short-date
                 :y-axis-min min :y-axis-scale scale))

(defun plot-exercisable-value (from to &key (min 0) (scale 0.01) (stock-price nil))
  (asterisk-plot (lambda (x) (exercisable-value :date x :stock-price stock-price))
                 (get-absolute-date from)
                 (get-absolute-date to)
                 #'find-next-pay-date
                 :fmt-x-axis #'fmt-short-date
                 :y-axis-min min :y-axis-scale scale))

(defun plot-market-value-units (from to &key (min 0) (scale 0.01) (stock-price nil))
  (asterisk-plot (lambda (x) (market-value-units :date x :stock-price stock-price))
                 (get-absolute-date from)
                 (get-absolute-date to)
                 #'find-next-pay-date
                 :fmt-x-axis #'fmt-short-date
                 :y-axis-min min :y-axis-scale scale))

(defun plot-potential-total (from to &key (min 0) (scale 0.01) (stock-price nil))
  (asterisk-plot (lambda (x) (potential-total :date x :stock-price stock-price))
                 (get-absolute-date from)
                 (get-absolute-date to)
                 #'find-next-pay-date
                 :fmt-x-axis #'fmt-short-date
                 :y-axis-min min :y-axis-scale scale))
