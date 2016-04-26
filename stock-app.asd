(defpackage :polyforms
  (:use :common-lisp)

  (:export
   ;; stock option plans
   :outstanding-grants
   :exercisable-grants
   :exercisable-value
   ;; purchase plan
   :espp-available-for-sale
   :espp-market-value
   ;; restricted stock units
   :units-unvested
   :units-available-for-sale
   :units-market-value
   ;; potential total
   :potential-total-purchased
   :potential-total-options
   :potential-total-units
   :potential-total
   ))

(defsystem stock-app
  :components
  ((:file "constants")
   (:file "utilities"
          :depends-on ("constants"))
   (:file "stock-details"
          :depends-on ("constants" "utilities"))
   (:file "stock-calculations"
          :depends-on ("constants" "utilities" "stock-details"))))
