(in-package :cl-user)
(defpackage docparser
  (:use :cl)
  (:import-from :trivial-types
                :proper-list)
  (:import-from :alexandria
                :destructuring-case)
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
   (externalp :reader symbol-externalp
              :initarg :externalp
              :type boolean
              :documentation "Whether the symbol is external to the package.")
   (setfp :reader symbol-setfp
          :initarg :setfp
          :initform nil
          :type boolean
          :documentation "Whether the symbol is a setf method."))
  (:documentation "A symbol."))

(defclass documentation-node ()
  ((documentation-symbol :reader doc-symbol
                         :initarg :symbol
                         :type symbol-node
                         :documentation "The symbol name of the operator, variable, or class.")
   (documentation-desc :reader doc-description
                       :initarg :doc
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
  ((accessors :reader slot-accessors
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
            :type (proper-list symbol-node)))
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
  ()
  (:documentation "A class."))

(defclass type-node (operator-node)
  ()
  (:documentation "A type."))

;;; Constructors

(defun symbol-external-p (symbol)
  "Whether or not a symbol is external."
  (multiple-value-bind (sym status)
      (find-symbol (symbol-name symbol)
                   (symbol-package symbol))
    (declare (ignore sym))
    (eq status :external)))

(defun symbol-node-from-symbol (symbol)
  "Build a symbol node from a Common Lisp symbol."
  (make-instance 'symbol-node
                 :package (package-name (symbol-package symbol))
                 :name (symbol-name symbol)
                 :externalp (symbol-external-p symbol)))

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
  (print-unreadable-object (symbol stream :type t)
    (write-string (render-full-symbol symbol) stream)))

;;; Parsing

(defun load-system (system-name)
  "Load an ASDF system by name."
  (asdf:load-system system-name :verbose nil :force t))

(defparameter *parsers* (list)
  "A list of symbols to the functions used to parse their corresponding forms.")

(defmacro define-parser (name (form) &body body)
  "Define a parser."
  `(assoc (cons ',name (lambda (,form)
                         ,@body))
          *parsers*))

(defun parse-form (form)
  "Parse a form into a node."
  (when (listp form)
    (let ((parser (rest (assoc (first form) *parsers*))))
      (when parser
        (apply parser (rest form))))))

(defun parse (system-name)
  (let* ((nodes (list))
         (old-macroexpander *macroexpand-hook*)
         (*macroexpand-hook* #'(lambda (function form environment)
                                 (let ((parsed (parse-form form)))
                                   (if parsed
                                       (push parsed nodes)
                                       (funcall old-macroexpander
                                                function
                                                form
                                                environment))))))
    (load-system system-name)
    nodes))

;;; Parsers

(define-parser cl:defun (form)
  (destructuring-bind (name (&rest args) docstring &body body) form
    t))
