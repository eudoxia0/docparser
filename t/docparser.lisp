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
        t)
      )))

(run! 'tests)
