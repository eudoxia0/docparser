(in-package :cl-user)
(defpackage docparser-test
  (:use :cl :fiveam))
(in-package :docparser-test)

;;; Utilities

(defmacro with-test-node ((node number type name) &body body)
  `(let ((,node (elt nodes ,number)))
     (is
      (typep ,node ',type))
     (is
      (equal (docparser:symbol-node-name (docparser:node-name ,node))
             ,name))
     (is (equal (docparser:node-docstring ,node)
                "docstring"))
     ,@body))

;;; Tests

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
              29)))
    ;; Test that individual nodes were parsed properly
    (let ((nodes (docparser::package-index-nodes
                  (elt (docparser::index-packages index) 0))))
      ;; The `var` variable
      (with-test-node (node 0 docparser:variable-node "VAR")
        t)
      ;; The `func` function
      (with-test-node (node 1 docparser:function-node "FUNC")
        (is
         (equal (length (docparser:operator-lambda-list node))
                5)))
      ;; The `mac` macro
      (with-test-node (node 2 docparser:macro-node "MAC")
        (is
         (equal (length (docparser:operator-lambda-list node))
                3)))
      ;; The `rec1` struct
      (with-test-node (node 3 docparser:struct-node "REC1"))
      ;; The `rec2` struct
      (with-test-node (node 13 docparser:struct-node "REC2"))
      ;; The `custom-string` type
      (with-test-node (node 21 docparser:type-node "CUSTOM-STRING"))
      ;; The `test-class` class
      (with-test-node (node 22 docparser:class-node "TEST-CLASS")
        (is (equal (length (docparser:record-slots node))
                   3))
        (let ((first-slot (first (docparser:record-slots node))))
          (is
           (typep first-slot 'docparser:class-slot-node))
          (is
           (equal (docparser:symbol-node-name (docparser:node-name first-slot))
                  "FIRST-SLOT"))
          (is
           (equal (docparser:node-docstring first-slot)
                  "docstring"))))
      ;; The `test-method` defgeneric
      ;; The `test-method` method
      ;; The `indirectly-define-function` macro
      (with-test-node (node 25 docparser:macro-node "INDIRECTLY-DEFINE-FUNCTION")
        (is
         (equal (length (docparser:operator-lambda-list node))
                0)))
      ;; The `hidden-function` function
      (with-test-node (node 26 docparser:function-node "HIDDEN-FUNCTION")
        (is
         (equal (length (docparser:operator-lambda-list node))
                0)))
      ;; The `size-t` CFFI type
      (with-test-node (node 27 docparser:cffi-type "SIZE-T"))
      ;; The `nums` CFFI enum
      (with-test-node (node 28 docparser:cffi-enum "NUMS")
        (is (equal (docparser:cffi-enum-variants node)
                   (list :a :b :c))))
      )))

(run! 'tests)
