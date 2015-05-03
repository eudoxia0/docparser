(in-package :cl-user)
(defpackage docparser
  (:use :cl)
  (:import-from :trivial-types
                :proper-list)
  ;; Classes
  (:export :documentation-node
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
  (:export ;; names and docstrings
           :node-name
           :node-docstring
           ;; Operators
           :operator-lambda-list
           :operator-setf-p
           ;; Slots
           :slot-accessors
           :slot-readers
           :slot-writers
           :slot-type
           :slot-allocation
           ;; Records and classes
           :record-slots
           :class-node-superclasses)
  ;; CFFI accessors
  (:export :cffi-enum-variants)
  ;; Methods
  (:export :symbol-external-p
           :symbol-package-name
           :render-full-symbol
           :render-humanize)
  ;; Indices
  (:export :package-index
           :index
           :package-index-name
           :package-index-docstring)
  ;; Interface
  (:export :parse
           :do-packages
           :do-nodes
           :query)
  (:documentation "Parse documentation from ASDF systems."))
