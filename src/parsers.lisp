(in-package :docparser)

(define-parser cl:defun (name (&rest args) &rest body)
  (let ((docstring (if (stringp (first body))
                       (first body)
                       nil)))
    (make-instance 'function-node
                   :name (symbol-node-from-symbol
                          (if (listp name)
                              ;; SETF name
                              (second name)
                              ;; Regular name
                              name))
                   :docstring docstring
                   :setfp (listp name)
                   :lambda-list args)))

(define-parser cl:defmacro (name (&rest args) &rest body)
  (let ((docstring (if (stringp (first body))
                       (first body)
                       nil)))
    (make-instance 'macro-node
                   :name (symbol-node-from-symbol name)
                   :docstring docstring
                   :lambda-list args)))

(define-parser cl:defgeneric (name (&rest args) &rest options)
  (let ((docstring (second (find :documentation options :key #'first))))
    (make-instance 'generic-function-node
                   :name (symbol-node-from-symbol name)
                   :docstring docstring
                   :lambda-list args)))

(define-parser cl:defmethod (name (&rest args) &rest body)
  (let ((docstring (if (stringp (first body))
                       (first body)
                       nil)))
    (make-instance 'method-node
                   :name (symbol-node-from-symbol
                          (if (listp name)
                              ;; SETF name
                              (second name)
                              ;; Regular name
                              name))
                   :docstring docstring
                   :setfp (listp name)
                   :lambda-list args)))

(defun parse-var (form)
  (destructuring-bind (name &optional initial-value docstring) form
    (declare (ignore initial-value))
    (make-instance 'variable-node
                   :name (symbol-node-from-symbol name)
                   :docstring docstring)))

(define-parser cl:defparameter (&rest form)
  (parse-var form))

(define-parser cl:defvar (&rest form)
  (parse-var form))

(define-parser cl:defconstant (&rest form)
  (parse-var form))

(define-parser cl:deftype (name lambda-list &rest body)
  (let ((docstring (if (stringp (first body))
                       (first body)
                       nil)))
    (make-instance 'type-node
                   :name (symbol-node-from-symbol name)
                   :docstring docstring
                   :lambda-list lambda-list)))

(defun parse-slot (slot)
  (let ((slot (copy-list slot)))
    (flet ((extract-all-and-delete (key)
             (let ((out (list)))
               (loop while (getf (rest slot) key) do
                 (push (getf (rest slot) key) out)
                 (remf (rest slot) key))))
           (list->symbols (list)
             (loop for fn in list collecting
                                  (symbol-node-from-symbol fn))))
      (let ((accessors (extract-all-and-delete :accessor))
            (readers (extract-all-and-delete :reader))
            (writers (extract-all-and-delete :writer)))
        (destructuring-bind (name &key initarg initform type
                                    (allocation :instance)
                                    documentation)
            slot
          (declare (ignore initarg initform))
          (make-instance 'class-slot-node
                         :name (symbol-node-from-symbol name)
                         :docstring documentation
                         :type type
                         :allocation allocation
                         :accessors (list->symbols accessors)
                         :readers (list->symbols readers)
                         :writers (list->symbols writers)))))))

(define-parser cl:defclass (name superclasses slots &rest options)
  (let ((docstring (second (find :documentation options :key #'first))))
    (make-instance 'class-node
                   :name (symbol-node-from-symbol name)
                   :superclasses (loop for class in superclasses collecting
                                   (symbol-node-from-symbol name))
                   :slots (loop for slot in slots collecting
                            (parse-slot slot))
                   :docstring docstring)))

(defun parse-struct-slot (slot)
  (let ((name (if (listp slot)
                  (first slot)
                  slot)))
    (make-instance 'struct-slot-node
                   :name (symbol-node-from-symbol name))))

(define-parser cl:defstruct (name-and-options &rest slots)
  (let ((name (if (listp name-and-options)
                  (first name-and-options)
                  name-and-options))
        (docstring (if (stringp (first slots))
                       (first slots)
                       nil))
        (slots (if (stringp (first slots))
                   (rest slots)
                   slots)))
    (make-instance 'struct-node
                   :name (symbol-node-from-symbol name)
                   :docstring docstring
                   :slots (loop for slot in slots collecting
                            (parse-struct-slot slot)))))

;;; CFFI parsers

(define-parser cffi:defcfun (name-and-options return-type &rest args)
  (let ((name (if (listp name-and-options)
                  (first (remove-if-not #'symbolp name-and-options))
                  name-and-options))
        (docstring (if (stringp (first args))
                       (first args)
                       nil))
        (args (if (stringp (first args))
                  (rest args)
                  nil)))
    (make-instance 'cffi-function
                   :name (symbol-node-from-symbol name)
                   :docstring docstring
                   :return-type return-type
                   :lambda-list args)))

(define-parser cffi:defctype (name base-type &optional docstring)
  (make-instance 'cffi-type
                 :name (symbol-node-from-symbol name)
                 :docstring docstring
                 :base-type base-type))

(defun parse-cffi-slot (form)
  (destructuring-bind (name type &rest rest) form
    (declare (ignore rest))
    (make-instance 'cffi-slot
                   :name (symbol-node-from-symbol name)
                   :type type)))

(define-parser cffi:defcstruct (name-and-options &rest doc-and-slots)
  (let ((name (if (listp name-and-options)
                  (first name-and-options)
                  name-and-options))
        (docstring (if (stringp (first doc-and-slots))
                       (first doc-and-slots)
                       nil))
        (slots (if (stringp (first doc-and-slots))
                   (rest doc-and-slots)
                   nil)))
    (make-instance 'cffi-struct
                   :name (symbol-node-from-symbol name)
                   :docstring docstring
                   :variants (loop for slot in slots collecting
                               (parse-cffi-slot slot)))))

(define-parser cffi:defcunion (name &rest doc-and-slots)
  (let ((docstring (if (stringp (first doc-and-slots))
                       (first doc-and-slots)
                       nil))
        (slots (if (stringp (first doc-and-slots))
                   (rest doc-and-slots)
                   nil)))
    (make-instance 'cffi-union
                   :name (symbol-node-from-symbol name)
                   :docstring docstring
                   :variants (loop for slot in slots collecting
                               (parse-cffi-slot slot)))))

(define-parser cffi:defcenum (name-and-options &rest enum-list)
  (let ((name (if (listp name-and-options)
                  (first name-and-options)
                  name-and-options))
        (docstring (if (stringp (first enum-list))
                       (first enum-list)
                       nil))
        (enum-list (if (stringp (first enum-list))
                       (rest enum-list)
                       nil)))
    (make-instance 'cffi-enum
                   :name (symbol-node-from-symbol name)
                   :docstring docstring
                   :variants (loop for variant in enum-list collecting
                               (if (listp variant)
                                   (first variant)
                                   variant)))))
