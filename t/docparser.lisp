(in-package :cl-user)
(defpackage docparser-test
  (:use :cl :fiveam))
(in-package :docparser-test)

(def-suite tests
  :description "docparser tests.")
(in-suite tests)

(test parsing
  (let ((out))
    (finishes
     (setf out (docparser:parse :docparser-test-system)))
    (print out)))

(run! 'tests)
