(in-package :cl-user)
(defpackage docparser-test
  (:use :cl :fiveam)
  (:export :run-tests)
  (:documentation "docparser tests."))
(in-package :docparser-test)

;;; Utilities

(defmacro with-test-node ((node type name) &body body)
  `(let* ((,node (elt nodes current-node)))
     (is
      (typep ,node ',type))
     (is
      (string= (symbol-name (docparser:node-name ,node))
               ,name))
     (is (string= (docparser:node-docstring ,node)
                  "docstring"))
     ,@body
     (incf current-node)))

(defmacro test-node-equality (nodes)
  (let ((node (gensym)))
    `(loop for ,node across ,nodes do
      (is-true
       (docparser:node= ,node ,node)))))

;;; Tests

(def-suite tests
  :description "docparser tests.")
(in-suite tests)

(defvar *index* nil)

(test system-parsing
  (let ((*index* (docparser:parse :docparser-test-system)))
    ;; Test the package
    (is
     (= (length (docparser::index-packages *index*))
        1))
    (let ((package-index (elt (docparser::index-packages *index*) 0)))
      (is
       (string= (docparser::package-index-name package-index)
                "DOCPARSER-TEST-SYSTEM")))))

(test variable-nodes
  (let* ((*index* (docparser:parse :docparser-test-system))
         (nodes (docparser::package-index-nodes
                 (elt (docparser::index-packages *index*) 0)))
         (current-node 0))
    (with-test-node (node docparser:variable-node "VAR")
      t)
    (with-test-node (node docparser:variable-node "VAR2")
      t)
    (with-test-node (node docparser:variable-node "CONST")
      t)
    (test-node-equality nodes)))

(test operator-nodes
  (let* ((*index* (docparser:parse :docparser-test-system))
         (nodes (docparser::package-index-nodes
                 (elt (docparser::index-packages *index*) 0)))
         (current-node 3))
    ;; The `func` function
    (with-test-node (node docparser:function-node "FUNC")
      (is
       (= (length (docparser:operator-lambda-list node))
          5)))
    ;; The `mac` macro
    (with-test-node (node docparser:macro-node "MAC")
      (is
       (= (length (docparser:operator-lambda-list node))
          3)))
    (test-node-equality nodes)))

(test type-nodes
  (let* ((*index* (docparser:parse :docparser-test-system))
         (nodes (docparser::package-index-nodes
                 (elt (docparser::index-packages *index*) 0)))
         (current-node 5))
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
      (is (= (length (docparser:record-slots node))
             3))
      (let ((first-slot (first (docparser:record-slots node)))
            (second-slot (second (docparser:record-slots node))))
        ;; First slot tests
        (is
         (typep first-slot 'docparser:class-slot-node))
        (is
         (string= (symbol-name (docparser:node-name first-slot))
                  "FIRST-SLOT"))
        (is
          (eq :first-slot
              (docparser:slot-initarg first-slot)))
        (is
          (equal (list nil nil)
                 (multiple-value-list (docparser:slot-initform first-slot))))
        (is
         (equal (docparser:node-docstring first-slot)
                "first docstring"))
        (is
         (eq (docparser:slot-allocation first-slot) :class))
        ;; Second slot
        (is
          (eq :second-slot
              (docparser:slot-initarg second-slot)))
        (is
          (equal (list "initform" t)
                 (multiple-value-list (docparser:slot-initform second-slot))))
        (is
         (string= (docparser:node-docstring second-slot)
                  "second docstring"))
        (is
         (eq (docparser:slot-allocation second-slot) :instance))))
    ;; The `test-condition` condition
    (with-test-node (node docparser:condition-node "TEST-CONDITION")
      (is (= (length (docparser:record-slots node))
             1))
      (let ((first-slot (first (docparser:record-slots node))))
        (is
         (typep first-slot 'docparser:class-slot-node))
        (is
         (string= (symbol-name (docparser:node-name first-slot))
                  "FIRST-SLOT"))
        (is
         (string= (docparser:node-docstring first-slot)
                  "docstring"))))
    (test-node-equality nodes)))

(test method-nodes
  (let* ((*index* (docparser:parse :docparser-test-system))
         (nodes (docparser::package-index-nodes
                 (elt (docparser::index-packages *index*) 0)))
         (current-node 26))
    ;; The `test-method` defgeneric
    (incf current-node)
    ;; The `test-method` method
    (incf current-node)
    ;; The `indirectly-define-function` macro
    (with-test-node (node docparser:macro-node "INDIRECTLY-DEFINE-FUNCTION")
      (is
       (= (length (docparser:operator-lambda-list node))
          0)))
    ;; The `hidden-function` function
    (with-test-node (node docparser:function-node "HIDDEN-FUNCTION")
      (is
       (= (length (docparser:operator-lambda-list node))
          0)))
    (test-node-equality nodes)))

(test cffi-nodes
  (let* ((*index* (docparser:parse :docparser-test-system))
         (nodes (docparser::package-index-nodes
                 (elt (docparser::index-packages *index*) 0)))
         (current-node 30))
    ;; The `printf` function
    (incf current-node 3)
    ;; The `size-t` CFFI type
    (with-test-node (node docparser:cffi-type "SIZE-T"))
    ;; The `cstruct` struct
    (incf current-node 2)
    ;; The `cunion` union
    (incf current-node 1)
    ;; The `nums` CFFI enum
    (with-test-node (node docparser:cffi-enum "NUMS")
      (is (equal (docparser:cffi-enum-variants node)
                 (list :a :b :c))))
    ;; The `bits` CFFI bitfield
    (with-test-node (node docparser:cffi-bitfield "BITS")
      (is (equal (docparser:cffi-bitfield-masks node)
                 (list :a :b :c))))
    (test-node-equality nodes)))

(test cffi-function-without-docstring
  (let* ((index (docparser:parse :docparser-test-system))
         (nodes (docparser:query index
                                 :package-name :docparser-test-system
                                 :symbol-name '#:printf-without-docstring))
         (func (find 'docparser:cffi-function nodes :key 'type-of)))
    (is (equal (docparser:operator-lambda-list func)
               '((docparser-test-system::control :string) &rest)))
    (is (equal (docparser:node-name func)
               'docparser-test-system::printf-without-docstring))
    (is (equal (docparser:cffi-function-return-type func)
               :int))
    (is (null (docparser:node-docstring func)))
    (is (not (docparser:operator-setf-p func)))))

(test queries
  (let ((*index* (docparser:parse :docparser-test-system)))
    (let ((result (docparser:query *index* :symbol-name "VAR")))
      (is (= (length result)
             1))
      (is (string= (docparser:node-docstring (elt result 0))
                   "docstring")))
    (let ((result (docparser:query *index* :package-name "DOCPARSER-TEST-SYSTEM"
                                           :symbol-name "VAR")))
      (is (= (length result)
             1))
      (is (string= (docparser:node-docstring (elt result 0))
                   "docstring")))))

(test printing
  (let ((*index* (docparser:parse :docparser-test-system)))
    (docparser:do-packages (package *index*)
      (docparser:do-nodes (node package)
        (print node))
      (docparser:dump *index*))))

(test utils
  (is-true (docparser:symbol-external-p 'docparser:render-humanize))
  (is (string= (docparser:render-humanize 'docparser:render-humanize)
               "render-humanize")))

(def-suite load-systems
  :description "Test docparser on real-world systems.")
(in-suite load-systems)

(test load-all-systems
  (let* ((systems (list :alexandria
                        :cl-conspack
                        :cl-csv))
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

(defun run-tests ()
  (run! 'tests)
  (run! 'load-systems))
