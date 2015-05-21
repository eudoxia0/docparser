(in-package :cl-user)
(defpackage docparser-test
  (:use :cl :fiveam))
(in-package :docparser-test)

;;; Utilities

(defmacro with-test-node ((node type name) &body body)
  `(let* ((,node (elt nodes current-node)))
     (is
      (typep ,node ',type))
     (is
      (equal (symbol-name (docparser:node-name ,node))
             ,name))
     (is (equal (docparser:node-docstring ,node)
                "docstring"))
     ,@body
     (incf current-node)))

;;; Tests

(def-suite tests
  :description "docparser tests.")
(in-suite tests)

(defvar *index* nil)

(test system-parsing
  (let ((*index* (docparser:parse :docparser-test-system)))
    ;; Test the package
    (is
     (equal (length (docparser::index-packages *index*))
            1))
    (let ((package-index (elt (docparser::index-packages *index*) 0)))
      (is
       (equal (docparser::package-index-name package-index)
              "DOCPARSER-TEST-SYSTEM"))
      (is
       (equal (length (docparser::package-index-nodes package-index))
              32)))))

(test nodes
  (let* ((*index* (docparser:parse :docparser-test-system))
         (nodes (docparser::package-index-nodes
                 (elt (docparser::index-packages *index*) 0))))
    ;; Test that individual nodes were parsed properly
    (let ((current-node 0))
      ;; Variables
      (with-test-node (node docparser:variable-node "VAR")
        t)
      (with-test-node (node docparser:variable-node "VAR2")
        t)
      (with-test-node (node docparser:variable-node "CONST")
        t)
      ;; The `func` function
      (with-test-node (node docparser:function-node "FUNC")
        (is
         (equal (length (docparser:operator-lambda-list node))
                5)))
      ;; The `mac` macro
      (with-test-node (node docparser:macro-node "MAC")
        (is
         (equal (length (docparser:operator-lambda-list node))
                3)))
      ;; The `rec1` struct
      (with-test-node (node docparser:struct-node "REC1"))
      ; Skip some defstruct-generated stuff
      (incf current-node 9)
      ;; The `rec2` struct
      (with-test-node (node docparser:struct-node "REC2"))
      ; Skip some defstruct-generated stuff
      (incf current-node 7)
      ;; The `custom-string` type
      (with-test-node (node docparser:type-node "CUSTOM-STRING"))
      ;; The `test-class` class
      (with-test-node (node docparser:class-node "TEST-CLASS")
        (is (equal (length (docparser:record-slots node))
                   3))
        (let ((first-slot (first (docparser:record-slots node))))
          (is
           (typep first-slot 'docparser:class-slot-node))
          (is
           (equal (symbol-name (docparser:node-name first-slot))
                  "FIRST-SLOT"))
          (is
           (equal (docparser:node-docstring first-slot)
                  "docstring"))))
      ;; The `test-method` defgeneric
      (incf current-node)
      ;; The `test-method` method
      (incf current-node)
      ;; The `indirectly-define-function` macro
      (with-test-node (node docparser:macro-node "INDIRECTLY-DEFINE-FUNCTION")
        (is
         (equal (length (docparser:operator-lambda-list node))
                0)))
      ;; The `hidden-function` function
      (with-test-node (node docparser:function-node "HIDDEN-FUNCTION")
        (is
         (equal (length (docparser:operator-lambda-list node))
                0)))
      ;; The `size-t` CFFI type
      (with-test-node (node docparser:cffi-type "SIZE-T"))
      ;; The `nums` CFFI enum
      (with-test-node (node docparser:cffi-enum "NUMS")
        (is (equal (docparser:cffi-enum-variants node)
                   (list :a :b :c))))
      ;; The `bits` CFFI bitfield
      (with-test-node (node docparser:cffi-bitfield "BITS")
        (is (equal (docparser:cffi-bitfield-masks node)
                   (list :a :b :c)))))))

(test queries
  (let ((*index* (docparser:parse :docparser-test-system)))
    (let ((result (docparser:query *index* :symbol-name "VAR")))
      (is (equal (length result)
                 1))
      (is (equal (docparser:node-docstring (elt result 0))
                 "docstring")))
    (let ((result (docparser:query *index* :package-name "DOCPARSER-TEST-SYSTEM"
                                           :symbol-name "VAR")))
      (is (equal (length result)
                 1))
      (is (equal (docparser:node-docstring (elt result 0))
                 "docstring")))))

(def-suite load-systems)
(in-suite load-systems)

(test load-all-systems
  (let* ((systems (list :1am
                        :able
                        :abnf
                        :access
                        :acl-compat
                        :alexandria
                        :anaphora
                        :antik
                        :arc-compat
                        :archive
                        :array-utils
                        :aserve
                        :babel
                        :bibtex
                        :binomial-heap
                        :bknr.web
                        :bordeaux-threads
                        :btrie
                        :caveman2
                        :cells
                        :chanl
                        :cffi
                        :checkl
                        :chipz
                        :chirp
                        :cl-6502
                        :cl-aa
                        :cl-annot
                        :cl-base64
                        :cl-ca
                        :cl-cairo2
                        :cl-conspack
                        :cl-csv
                        :cl-fad))
         (success-count 0)
         (system-count (length systems))
         (failures (list)))
    (loop for system in systems do
      (format t "~%Loading ~S~%" system)
      (finishes
        (let ((index (docparser:parse system))
              (node-count 0))
          (docparser:do-packages (package index)
            (docparser:do-nodes (node package)
              (incf node-count)))
          (if (> node-count 0)
            (progn
              (incf success-count)
              (is-true t))
            (push system failures)))))
    (format t "~&Succeeded ~A/~A systems. Failed: ~A"
            success-count
            system-count
            failures)))

(run! 'tests)
(run! 'load-systems)
