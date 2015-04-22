(in-package :cl-user)
(defpackage docparser-test-system
  (:use :cl)
  (:export :var
           :func
           :mac
           :rec1
           :rec2
           :struct-slot-a
           :struct-slot-b
           :test-class
           :first-slot
           :second-slot
           :test-method)
  (:documentation "docstring"))
(in-package :docparser-test-system)

(defparameter var t
  "docstring")

(defun func (a b &optional (c "") (l 1))
  "docstring"
  (declare (ignore a b c l))
  t)

(defmacro mac (a b c)
  "docstring"
  (declare (ignore a b c))
  t)

(defstruct rec1
  "docstring"
  a
  (b "test" :type string)
  (c 1))

(defstruct rec2
  "docstring"
  struct-slot-a
  struct-slot-b)

(defclass test-class ()
  ((first-slot :accessor first-slot
               :initarg :first-slot
               :documentation "docstring")
   (second-slot :reader second-slot
                :reader s-slot
                :initarg :second-slot
                :documentation "docstring")
   (unexported-slot :reader unexported-slot
                    :initarg :unexported-slot
                    :documentation "docstring"))
  (:documentation "docstring"))

(defmethod test-method ((tc test-class) a)
  "docstring"
  (declare (ignore tc a))
  t)
