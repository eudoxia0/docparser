(in-package :cl-user)
(defpackage docparser-test
  (:use :cl :fiveam))
(in-package :docparser-test)

(def-suite tests
  :description "docparser tests.")
(in-suite tests)

(test parsing
  (let ((index))
    (finishes
     (setf index (docparser:parse :docparser-test-system)))
    ;; Test the package
    (is
     (equal (length (docparser::index-packages index))
            1))
    (let ((package-index (elt (docparser::index-packages index) 0)))
      (is
       (equal (docparser::package-index-name package-index)
              "DOCPARSER-TEST-SYSTEM"))
      (is
       (equal (length (docparser::package-index-nodes package-index))
              29)))))

(run! 'tests)
