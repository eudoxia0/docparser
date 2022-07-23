(in-package :cl-user)
(defpackage docparser/corrector/test-system
  (:use :cl)
  (:documentation "docstring"))
(in-package :docparser/corrector/test-system)

(defmacro find-slot (slot-name class)
  `(find ',slot-name
        (sb-mop:class-direct-slots  (find-class ',class))
        :key #'sb-mop:slot-definition-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter var t
  "docstring")

(setf (documentation 'var 'variable)
      "coorrected docstring: defparameter var t")

(defvar var2 t
  "docstring")

(setf (documentation 'var2 'variable)
      "coorrected docstring: defvar var2 t")

(defconstant const t
  "docstring")

(setf (documentation 'const 'variable)
      "coorrected docstring: defconstant const t")

(defun func (a b &optional (c "") (l 1))
  "docstring"
  (declare (ignore a b c l))
  t)

(setf (documentation 'func 'function)
      "coorrected docstring: defun func (a b &optional (c \"\") (l 1))")

(defmacro mac (a b c)
  "docstring"
  (declare (ignore a b c))
  t)

(setf (documentation 'mac 'function)
      "coorrected docstring: defmacro mac (a b c)")

(defstruct rec1
  "docstring"
  a
  (b "test" :type string)
  (c 1))

(setf (documentation 'rec1 'type)
      "coorrected docstring: defstruct rec1")

(defstruct rec2
  "docstring"
  struct-slot-a
  struct-slot-b)

(setf (documentation 'rec2 'type)
      "coorrected docstring: defstruct rec2")

#+nil(setf (documentation 'rec2 'structure)
           "coorrected docstring: defstruct rec2")

(deftype custom-string (val)
  "docstring"
  (string= val "my-string"))

(setf (documentation 'custom-string 'type)
      "coorrected docstring: deftype custom-string (val)")

(defclass test-class ()
  ((first-slot :accessor first-slot
               :initarg :first-slot
               :documentation "first docstring"
               :allocation :class)
   (second-slot :reader second-slot
                :reader s-slot
                :initarg :second-slot
                :initform "initform"
                :documentation "second docstring")
   third-slot)
  (:documentation "docstring"))

(setf (documentation 'test-class 'type)
      "coorrected docstring: defclass test-class ()")

(setf
 (documentation (find-slot first-slot test-class) t)
 "coorrected first docstring: first-slot")

(setf
 (documentation (find-slot second-slot test-class) t)
 "coorrected first docstring: second-slot")

(define-condition test-condition ()
  ((first-slot :accessor first-slot
               :initarg :first-slot
               :documentation "docstring"))
  (:documentation "docstring"))

(setf
 (documentation 'test-condition 'type)
 "coorrected docstring: define-condition test-condition ()")

(setf
 (slot-value (find-slot first-slot test-condition) 'sb-pcl::%documentation)
 "coorrected docstring: define-condition first-slot")


(defgeneric test-method (obj a)
  (:documentation "docstring"))

(setf
 (documentation 'test-method 'function)
 "coorrected docstring: defgeneric test-method (obj a)")

(defmethod test-method ((tc test-class) a)
  "docstring"
  (declare (ignore tc a))
  t)

(setf
 (documentation (find-method #'test-method nil '(test-class t)) t)
 "coorrected docstring: defmethod test-method ((tc test-class) a)")


(defmacro indirectly-define-function ()
  "docstring"
  `(defun hidden-function ()
     "docstring"
     t))

(indirectly-define-function)

(setf
 (documentation 'indirectly-define-function 'function)
 "coorrected docstring: defmacro indirectly-define-function ()")

(setf
 (documentation 'hidden-function 'function)
 "coorrected docstring: defun hidden-function ()")

(defun local-symbol-macro ()
  (flet (((setf thing) (new-value) new-value))
    (symbol-macrolet ((thing (thing)))
      (setf thing 1))))

;; Too cool to understand local-symbol-macro

;;; CFFI stuff

(cffi:defcfun printf :int
  "docstring"
  (control :string) &rest)

(cffi:defctype size-t :unsigned-long "docstring")

(cffi:defcstruct cstruct
  "docstring"
  (a :int)
  (b :double))

(cffi:defcunion cunion
  "docstring"
  (a :int)
  (b :double))

(cffi:defcenum nums "docstring" :a :b (:c 3))

(cffi:defbitfield bits "docstring" :a :b (:c #x0200))
