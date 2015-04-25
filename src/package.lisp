(in-package :cl-user)
(defpackage docparser
  (:use :cl)
  (:import-from :trivial-types
                :proper-list
                :association-list)
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
           :struct-slot-node
           :class-slot-node
           :record-node
           :struct-node
           :class-node
           :type-node)
  ;; CFFI classes
  (:export :cffi-node
           :cffi-function
           :cffi-type
           :cffi-slot
           :cffi-struct
           :cffi-union
           :cffi-enum)
  ;; Accessors
  (:export :symbol-node-package
           :symbol-node-name
           :symbol-external-p
           :symbol-setf-p)
  (:export :node-name
           :node-docstring)
  (:export :operator-lambda-list)
  (:export :slot-accessors
           :slot-readers
           :slot-writers
           :slot-type
           :slot-allocation)
  (:export :record-slots)
  (:export :class-node-superclasses)
  ;; Interface
  (:export :parse)
  (:documentation "Parse documentation from ASDF systems."))
