(in-package :cl-user)
(defpackage docparser
  (:use :cl)
  (:import-from :trivial-types
                :proper-list)
  (:import-from :anaphora
                :awhen
                :it)
  ;; Classes
  (:export :name-node
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
           :condition-node
           :type-node)
  ;; CFFI classes
  (:export :cffi-node
           :cffi-function
           :cffi-type
           :cffi-slot
           :cffi-struct
           :cffi-union
           :cffi-enum
           :cffi-bitfield)
  ;; Accessors
  (:export ;; names and docstrings
           :node-form
           :node-name
           :node-docstring
           ;; Operators
           :operator-lambda-list
           :operator-setf-p
           ;; Variables
           :variable-initial-value
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
  (:export :cffi-function-return-type
           :cffi-type-base-type
           :cffi-slot-type
           :cffi-struct-slots
           :cffi-union-variants
           :cffi-enum-variants
           :cffi-bitfield-masks)
  ;; Methods
  (:export :symbol-external-p
           :symbol-package-name
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
           :query
           :dump
           :*store-form*)
  (:documentation "Parse documentation from ASDF systems."))
