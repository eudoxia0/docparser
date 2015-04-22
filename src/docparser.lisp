(in-package :cl-user)
(defpackage docparser
  (:use :cl)
  (:import-from :trivial-types
                :proper-list)
  (:import-from :alexandria
                :destructuring-case)
  ;; Classes
  (:export :symbol-node
           :documentation-node
           :operator-node
           :function-node
           :macro-node
           :generic-function-node
           :method-node
           :variable-node
           :slot-node
           :record-node
           :struct-node
           :class-node
           :type-node)
  ;; Accessors
  (:export :symbol-node-package
           :symbol-node-name
           :symbol-external-p
           :symbol-setf-p)
  (:export :node-name
           :node-docstring)
  (:export :operator-lambda-list)
  (:export :slot-name
           :slot-accessors
           :slot-readers
           :slot-writers
           :slot-type
           :slot-allocation)
  (:export :record-slots)
  (:export :class-node-superclasses)
  ;; Interface
  (:export :parse)
  (:documentation "Parse documentation from ASDF systems."))
(in-package :docparser)

;;; Classes

(defclass symbol-node ()
  ((symbol-node-package :reader symbol-node-package
                        :initarg :package
                        :type string
                        :documentation "A symbol's package name.")
   (symbol-node-name :reader symbol-node-name
                     :initarg :name
                     :type string
                     :documentation "A symbol's name.")
   (externalp :reader symbol-external-p
              :initarg :externalp
              :type boolean
              :documentation "Whether the symbol is external to the package.")
   (setfp :reader symbol-setf-p
          :initarg :setfp
          :initform nil
          :type boolean
          :documentation "Whether the symbol is a setf method."))
  (:documentation "A symbol."))

(defclass documentation-node ()
  ((node-name :reader node-name
              :initarg :name
              :type symbol-node
              :documentation "The symbol name of the operator, variable, or class.")
   (node-docstring :reader node-docstring
                   :initarg :docstring
                   :type (or null string)
                   :documentation "The node's documentation."))
  (:documentation "Superclass for all documentation nodes."))

(defclass operator-node (documentation-node)
 ((lambda-list :reader operator-lambda-list
               :initarg :lambda-list
               :documentation "The operator's lambda list."))
  (:documentation "The base class of functions and macros."))

(defclass function-node (operator-node)
  ()
  (:documentation "A function."))

(defclass macro-node (operator-node)
  ()
  (:documentation "A macro."))

(defclass generic-function-node (operator-node)
  ()
  (:documentation "A generic function."))

(defclass method-node (operator-node)
  ()
  (:documentation "A method."))

(defclass variable-node (documentation-node)
  ()
  (:documentation "A variable."))

(defclass slot-node (documentation-node)
  ((name :reader slot-name
         :initarg :name
         :type symbol-node)
   (accessors :reader slot-accessors
              :initarg :accessors
              :initform nil
              :type (proper-list symbol-node))
   (readers :reader slot-readers
            :initarg :readers
            :initform nil
            :type (proper-list symbol-node))
   (writers :reader slot-writers
            :initarg :writers
            :initform nil
            :type (proper-list symbol-node))
   (type :reader slot-type
         :initarg :type
         :documentation "The slot's type.")
   (allocation :reader slot-allocation
               :initarg :allocation
               :initform :instance
               :type keyword
               :documentation "The slot's allocation type."))
  (:documentation "A class or structure slot."))

(defclass record-node (documentation-node)
  ((slots :reader record-slots
          :initarg :slots
          :type (proper-list slot-node)
          :documentation "A list of slots.")))

(defclass struct-node (record-node)
  ()
  (:documentation "A structure."))

(defclass class-node (record-node)
  ((superclasses :reader class-node-superclasses
                 :initarg :superclasses
                 :type (proper-list symbol)
                 :documentation "A list of the class's superclasses (symbols)."))
  (:documentation "A class."))

(defclass type-node (operator-node)
  ()
  (:documentation "A type."))

;;; Constructors

(defun cl-symbol-external-p (symbol)
  "Whether or not a symbol is external."
  (multiple-value-bind (sym status)
      (find-symbol (symbol-name symbol)
                   (symbol-package symbol))
    (declare (ignore sym))
    (eq status :external)))

(defun symbol-node-from-symbol (symbol &key setf)
  "Build a symbol node from a Common Lisp symbol."
  (make-instance 'symbol-node
                 :package (package-name (symbol-package symbol))
                 :name (symbol-name symbol)
                 :externalp (cl-symbol-external-p symbol)
                 :setfp setf))

;;; Methods

(defun render-full-symbol (symbol-node)
  "Render a symbol into a string."
  (concatenate 'string
               (symbol-node-package symbol-node)
               ":"
               (symbol-node-name symbol-node)))

(defun render-humanize (symbol-node)
  "Render a symbol into a string in a human-friendly way."
  (string-downcase (symbol-node-name symbol-node)))

;;; Printing

(defmethod print-object ((symbol symbol-node) stream)
  "Print a symbol node."
  (print-unreadable-object (symbol stream)
    (format stream "symbol ~A" (render-humanize symbol))))

(defmethod print-object ((operator operator-node) stream)
  "Print an operator node."
  (print-unreadable-object (operator stream)
    (format stream "~A ~A ~A"
            (typecase operator
              (function-node "function")
              (macro-node "macro")
              (generic-function-node "generic function")
              (method-node "method")
              (t "operator"))
            (let ((name (node-name operator)))
              (if (symbol-setf-p name)
                  (format nil "(setf ~A)" (render-humanize name))
                  (render-humanize name)))
            (operator-lambda-list operator))))

(defmethod print-object ((var variable-node) stream)
  "Print a variable node."
  (print-unreadable-object (var stream)
    (format stream "variable ~A" (render-humanize (node-name var)))))

(defmethod print-object ((type type-node) stream)
  "Print a type definition node."
  (print-unreadable-object (type stream)
    (format stream "type ~A" (render-humanize (node-name type)))))


;;; Parsing

(defun load-system (system-name)
  "Load an ASDF system by name."
  (uiop:with-muffled-loader-conditions ()
    (uiop:with-muffled-compiler-conditions ()
      (asdf:load-system system-name :verbose nil :force t))))

(defparameter *parsers* (list)
  "A list of symbols to the functions used to parse their corresponding forms.")

(defmacro define-parser (name (form) &body body)
  "Define a parser."
  `(push (cons ',name (lambda (,form)
                        ,@body))
         *parsers*))

(defun parse-form (form)
  "Parse a form into a node."
  (when (listp form)
    (let ((parser (rest (assoc (first form) *parsers*))))
      (when parser
        (funcall parser (rest form))))))

(defun parse (system-name)
  "Parse documentation from a system."
  (let* ((nodes (list))
         (old-macroexpander *macroexpand-hook*)
         (*macroexpand-hook*
           #'(lambda (function form environment)
               (let ((parsed (parse-form form)))
                 (if parsed
                     (progn
                       (push parsed nodes)
                       `(identity t))
                     (funcall old-macroexpander
                              function
                              form
                              environment))))))
    (load-system system-name)
    nodes))

;;; Parsers

(define-parser cl:defun (form)
  (destructuring-bind (name (&rest args) &rest body) form
    (let ((docstring (if (stringp (first body))
                         (first body)
                         nil)))
      (make-instance 'function-node
                     :name (if (listp name)
                               ;; SETF name
                               (symbol-node-from-symbol (second name)
                                                        :setf t)
                               ;; Regular name
                               (symbol-node-from-symbol name))
                     :docstring docstring
                     :lambda-list args))))

(define-parser cl:defmacro (form)
  (destructuring-bind (name (&rest args) &rest body) form
    (let ((docstring (if (stringp (first body))
                         (first body)
                         nil)))
      (make-instance 'macro-node
                     :name (symbol-node-from-symbol name)
                     :docstring docstring
                     :lambda-list args))))

(define-parser cl:defgeneric (form)
  (declare (ignore form))
  t)

(define-parser cl:defmethod (form)
  (destructuring-bind (name (&rest args) &rest body) form
    (let ((docstring (if (stringp (first body))
                         (first body)
                         nil)))
      (make-instance 'method-node
                     :name (if (listp name)
                               ;; SETF name
                               (symbol-node-from-symbol (second name)
                                                        :setf t)
                               ;; Regular name
                               (symbol-node-from-symbol name))
                     :docstring docstring
                     :lambda-list args))))

(defun parse-var (form)
  (destructuring-bind (name &optional initial-value docstring) form
    (declare (ignore initial-value))
    (make-instance 'variable-node
                   :name (symbol-node-from-symbol name)
                   :docstring docstring)))

(define-parser cl:defparameter (form) (parse-var form))
(define-parser cl:defvar (form) (parse-var form))
(define-parser cl:defconstant (form) (parse-var form))

(define-parser cl:deftype (form)
  (destructuring-bind (name lambda-list &rest body) form
    (let ((docstring (if (stringp (first body))
                         (first body)
                         nil)))
      (make-instance 'type-node
                     :name (symbol-node-from-symbol name)
                     :docstring docstring
                     :lambda-list lambda-list))))

(defun parse-slot (slot)
  (flet ((extract-all-and-delete (key)
           (let ((out (list)))
             (loop while (getf (rest slot) key) do
               (push (getf (rest slot) key) out)
               (remf (rest slot) ley))))
         (list->symbols (list)
           (loop for fn in list collecting
             (symbol-node-from-symbol fn))))
    (let ((accessors (extract-all-and-delete :accessor))
          (readers (extract-all-and-delete :reader))
          (writers (extract-all-and-delete :writer)))
      (destructuring-bind (name &key type allocation documentation)
          (make-instance 'slot-node
                         :name (symbol-node-from-symbol name)
                         :docstring docstring
                         :type type
                         :allocation allocation
                         :accessors (list->symbols accessors)
                         :readers (list->symbols readers)
                         :writers (list->symbols writers))))))


(define-parser cl:defclass (form)
  (destructuring-bind (name superclasses slots &rest options) form
    (let ((docstring (second (find :documentation options :key #'first))))
      (make-instance 'class-node
                     :name (symbol-node-from-symbol name)
                     :superclasses (loop for class in superclasses collecting
                                     (symbol-node-from-symbol name))
                     :slots (loop for slot in slots collecting
                              (parse-slot slot))
                     :docstring docstring))))
