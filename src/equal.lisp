;;;; Node equality
(in-package :docparser)

(defgeneric node= (a b)
  (:documentation "Test whether a and b are equal."))

(defmethod node= ((a t) (b t))
  "The default method."
  nil)

(defmacro define-equality ((a b class) &body body)
  `(defmethod node= ((,a ,class) (,b ,class))
     ,@body))

(defun slot-or-nil (object slot)
  (if (slot-boundp object slot)
      (slot-value object slot)))

(define-equality (a b name-node)
  (and (equal (slot-or-nil a 'form) (slot-or-nil b 'form))
       (eq (node-name a) (node-name b))))

(define-equality (a b documentation-node)
  (and (equal (slot-or-nil a 'node-docstring) (slot-or-nil b 'node-docstring))
       (call-next-method)))

(define-equality (a b operator-node)
  (and (equal (operator-lambda-list a) (operator-lambda-list b))
       (call-next-method)))

(define-equality (a b function-node)
  (and (eql (operator-setf-p a) (operator-setf-p b))
       (call-next-method)))

(define-equality (a b generic-function-node)
  (and (eql (operator-setf-p a) (operator-setf-p b))
       (call-next-method)))

(define-equality (a b method-node)
  (and (eql (operator-setf-p a) (operator-setf-p b))
       (equal (method-qualifiers a) (method-qualifiers b))
       (call-next-method)))

(define-equality (a b class-slot-node)
  (and (equal (slot-accessors a) (slot-accessors b))
       (equal (slot-readers a) (slot-readers b))
       (equal (slot-writers a) (slot-writers b))
       (equal (slot-or-nil a 'type) (slot-or-nil b 'type))
       (eq (slot-allocation a) (slot-allocation b))
       (equal (slot-or-nil a 'initarg) (slot-or-nil b 'initarg))
       (equal (multiple-value-list (slot-initform a))
              (multiple-value-list (slot-initform b)))
       (call-next-method)))

(define-equality (a b struct-node)
  (and (every #'node= (record-slots a) (record-slots b))
       (call-next-method)))

(define-equality (a b class-node)
  (and (equal (class-node-superclasses a) (class-node-superclasses b))
       (equal (class-node-metaclass a) (class-node-metaclass b))
       (equal (class-node-default-initargs a) (class-node-default-initargs b))
       (every #'node= (record-slots a) (record-slots b))
       (call-next-method)))

(define-equality (a b cffi-function)
  (and (equal (cffi-function-return-type a) (cffi-function-return-type b))
       (call-next-method)))

(define-equality (a b cffi-type)
  (and (equal (cffi-type-base-type a) (cffi-type-base-type b))
       (call-next-method)))

(define-equality (a b cffi-slot)
  (and (equal (cffi-slot-type a) (cffi-slot-type b))
       (call-next-method)))

(define-equality (a b cffi-struct)
  (and (every #'node= (cffi-struct-slots a) (cffi-struct-slots b))
       (call-next-method)))

(define-equality (a b cffi-union)
  (and (equal (cffi-union-variants a) (cffi-union-variants b))
       (call-next-method)))

(define-equality (a b cffi-enum)
  (and (equal (cffi-enum-variants a) (cffi-enum-variants b))
       (call-next-method)))

(define-equality (a b cffi-bitfield)
  (and (equal (cffi-bitfield-masks a) (cffi-bitfield-masks b))
       (call-next-method)))
