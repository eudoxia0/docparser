;;;; Classes that represent documentation nodes, and some methods
(in-package :docparser)

(defclass node ()
  ()
  (:documentation "The base class of all nodes."))

(defclass name-node (node)
  ((form :accessor node-form
         :initarg :form
         :documentation "The original form.")
   (node-name :reader node-name
              :initarg :name
              :type symbol
              :documentation "The symbol name of the operator, variable, or class."))
  (:documentation "The base class of nodes with symbol names."))

(defclass documentation-node (name-node)
  ((node-docstring :reader node-docstring
                   :initarg :docstring
                   :type (or null string)
                   :documentation "The node's documentation."))
  (:documentation "The base class of all documentable nodes."))

(defclass operator-node (documentation-node)
 ((lambda-list :reader operator-lambda-list
               :initarg :lambda-list
               :documentation "The operator's lambda list.")
  (setfp :reader operator-setf-p
         :initarg :setfp
         :initform nil
         :type boolean
         :documentation "Whether the operator is a setf operation."))
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
  ((qualifiers :reader method-qualifiers
               :initarg :qualifiers
               :initform nil))
  (:documentation "A method."))

(defclass variable-node (documentation-node)
  ((variable-initial-value :reader variable-initial-value
                           :initarg :initial-value))
  (:documentation "A variable."))

(defclass struct-slot-node (name-node)
  ()
  (:documentation "A structure's slot."))

(defclass class-slot-node (documentation-node)
  ((accessors :reader slot-accessors
              :initarg :accessors
              :initform nil
              :type (proper-list symbol))
   (readers :reader slot-readers
            :initarg :readers
            :initform nil
            :type (proper-list symbol))
   (writers :reader slot-writers
            :initarg :writers
            :initform nil
            :type (proper-list symbol))
   (type :reader slot-type
         :initarg :type
         :documentation "The slot's type.")
   (initarg :reader slot-initarg
            :initarg :initarg
            :documentation "The slot's initarg.")
   (initform :initarg :initform
             :documentation "The slot's initform.")
   (allocation :reader slot-allocation
               :initarg :allocation
               :initform :instance
               :type keyword
               :documentation "The slot's allocation type."))
  (:documentation "A class or structure slot."))

(defun slot-initform (class-slot-node)
  "Return the initform for the slot.
Also returns a second boolean value indicating whether the slot has an initform,
so an initform of NIL can be distinguished from not having an initform at all."
  (if (slot-boundp class-slot-node 'initform)
    (values (slot-value class-slot-node 'initform) t)
    (values nil nil)))

(defclass record-node (documentation-node)
  ()
  (:documentation "The base class of all nodes representing record-like data
  type definitions (i.e. structures, classes)."))

(defclass struct-node (record-node)
  ((slots :reader record-slots
          :initarg :slots
          :type (proper-list struct-slot-node)
          :documentation "A list of slots."))
  (:documentation "A structure."))

(defclass class-node (record-node)
  ((superclasses :reader class-node-superclasses
                 :initarg :superclasses
                 :type (proper-list symbol)
                 :documentation "A list of the class's superclasses (symbols).")
   (metaclass :reader class-node-metaclass
              :initarg :metaclass
              :type symbol
              :documentation "The class's metaclass (symbol).")
   (default-initargs :reader class-node-default-initargs
                     :initarg :default-initargs
                     :type (proper-list)
                     :documentation "The class's metaclass (symbol).")
   (slots :reader record-slots
          :initarg :slots
          :type (proper-list class-slot-node)
          :documentation "A list of slots."))
  (:documentation "A class."))

(defclass condition-node (class-node)
  ()
  (:documentation "A condition."))

(defclass type-node (operator-node)
  ()
  (:documentation "A type."))

;;; CFFI classes

(defclass cffi-node ()
  ()
  (:documentation "The base class of all CFFI documentation nodes. Does not
 inherit from documentation-node as not all sub-nodes have docstrings."))

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

(defclass cffi-bitfield (cffi-node documentation-node)
  ((masks :reader cffi-bitfield-masks
          :initarg :masks
          :type (proper-list keyword)
          :documentation "A list of masks (keywords)."))
  (:documentation "A C bitfield."))

;;; Methods

(defun symbol-external-p (symbol)
  "Whether or not a symbol is external."
  (multiple-value-bind (sym status)
      (find-symbol (symbol-name symbol)
                   (symbol-package symbol))
    (declare (ignore sym))
    (eq status :external)))

(defun symbol-package-name (symbol)
  "Return the name of a package's symbol."
  (let ((package (symbol-package symbol)))
    (when package
      (package-name package))))

(defun render-humanize (symbol)
  "Render a symbol into a string in a human-friendly way."
  (string-downcase (symbol-name symbol)))
