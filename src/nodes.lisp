;;;; Classes that represent documentation nodes, and some methods
(in-package :docparser)

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

(defclass name-node ()
  ((node-name :reader node-name
              :initarg :name
              :type symbol-node
              :documentation "The symbol name of the operator, variable, or class."))
  (:documentation "The base class of nodes with symbol names."))

(defclass documentation-node (name-node)
  ((node-docstring :reader node-docstring
                   :initarg :docstring
                   :type (or null string)
                   :documentation "The node's documentation."))
  (:documentation "The base class of nodes with symbol names and docstrings."))

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

(defclass struct-slot-node (name-node)
  ()
  (:documentation "A structure's slot."))

(defclass class-slot-node (documentation-node)
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

;;; CFFI classes

(defclass cffi-node ()
  ()
  (:documentation "A CFFI construct."))

(defclass cffi-function (cffi-node function-node)
  ((return-type :reader cffi-function-return-type
                :initarg :return-type
                :documentation "The function's return type."))
  (:documentation "A C function."))

(defclass cffi-type (cffi-node documentation-node)
  ((base-type :reader cffi-type-base-type
              :initarg :base-type
              :documentation "The type's base type."))
  (:documentation "A C type."))

(defclass cffi-slot (name-node)
  ((type :reader cffi-slot-type
         :initarg :type
         :documentation "The slot's type."))
  (:documentation "A struct or union slot."))

(defclass cffi-struct (cffi-node documentation-node)
  ((slots :reader cffi-struct-slots
          :initarg :slots
          :type (proper-list cffi-slot)
          :documentation "A list of CFFI slots."))
  (:documentation "A C structure."))

(defclass cffi-union (cffi-node documentation-node)
  ((variants :reader cffi-union-variants
             :initarg :variants
             :type (proper-list cffi-slot)
             :documentation "A list of CFFI slots."))
  (:documentation "A C union."))

(defclass cffi-enum (cffi-node documentation-node)
  ((variants :reader cffi-enum-variants
             :initarg :variants
             :type (proper-list keyword)
             :documentation "A list of enum values (keywords)."))
  (:documentation "A C enum."))

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
