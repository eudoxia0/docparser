(in-package :docparser)

(defun extract-docstring (body)
  "Extract the docstring from the body of a form.
Correctly handles bodies where the first form is a declaration."
  (loop for exp in body
     if (and (not (stringp exp))
             (not (and (listp exp)
                       (eq (car exp) 'cl:declare))))
     do (return)
     else if (stringp exp)
     do (return exp)))

(define-parser cl:defun (name (&rest args) &rest body)
  (let ((docstring (extract-docstring body)))
    (make-instance 'function-node
                   :name (if (listp name)
                             ;; SETF name
                             (second name)
                             ;; Regular name
                             name)
                   :docstring docstring
                   :setfp (listp name)
                   :lambda-list args)))

(define-parser cl:defmacro (name (&rest args) &rest body)
  (let ((docstring (extract-docstring body)))
    (make-instance 'macro-node
                   :name name
                   :docstring docstring
                   :lambda-list args)))

(define-parser cl:define-setf-expander (name (&rest args) &rest body)
  (let ((docstring (extract-docstring body)))
    (make-instance 'macro-node
                   :name name
                   :docstring docstring
                   :lambda-list args
                   :setfp t)))

(define-parser cl:defgeneric (name (&rest args) &rest options)
  (let ((docstring (second (find :documentation options :key #'first))))
    (make-instance 'generic-function-node
                   :name (if (listp name)
                             ;; SETF name
                             (second name)
                             ;; Regular name
                             name)
                   :docstring docstring
                   :setfp (listp name)
                   :lambda-list args)))

(define-parser cl:defmethod (name &rest args)
  (let* ((lambda-list-pos (position-if #'listp args))
         (qualifiers (subseq args 0 lambda-list-pos))
         (lambda-list (nth lambda-list-pos args))
         (body (subseq args (1+ lambda-list-pos)))
         (docstring (extract-docstring body)))
    (make-instance 'method-node
                   :name (if (listp name)
                             ;; SETF name
                             (second name)
                             ;; Regular name
                             name)
                   :docstring docstring
                   :qualifiers qualifiers
                   :setfp (listp name)
                   :lambda-list lambda-list)))

(defun parse-var (form)
  (destructuring-bind (name &optional (initial-value nil initial-value-specified-p) docstring) form
    (apply #'make-instance 'variable-node
           :name name
           :docstring docstring
           (if initial-value-specified-p
               (list :initial-value initial-value)
               '()))))

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
                   :name name
                   :docstring docstring
                   :lambda-list lambda-list)))

(defun parse-slot (slot)
  (if (listp slot)
      (let ((slot (copy-list slot)))
        (flet ((extract-all-and-delete (key)
                 (let ((out (list)))
                   (loop while (getf (rest slot) key) do
                     (push (getf (rest slot) key) out)
                     (remf (rest slot) key))
                   out)))
          (let ((accessors (extract-all-and-delete :accessor))
                (readers (extract-all-and-delete :reader))
                (writers (extract-all-and-delete :writer)))
            (destructuring-bind (name &key initarg initform type
                                        (allocation :instance)
                                        documentation
                                        ;; For metaclass stuff
                                        &allow-other-keys)
                slot
              (let ((node (make-instance 'class-slot-node
                                         :name name
                                         :docstring documentation
                                         :type type
                                         :allocation allocation
                                         :accessors accessors
                                         :readers readers
                                         :writers writers
                                         :initarg initarg)))
                (when (member :initform slot)
                  (setf (slot-value node 'initform) initform))
                node)))))
      (make-instance 'class-slot-node
                     :name slot)))

(define-parser cl:defclass (name superclasses slots &rest options)
  (let ((docstring (second (find :documentation options :key #'first)))
        (metaclass (second (find :metaclass options :key #'first)))
        (default-initargs (cdr (find :default-initargs options :key #'first))))
    (make-instance 'class-node
                   :name name
                   :superclasses superclasses
                   :metaclass (if metaclass metaclass 'cl:standard-class)
                   :default-initargs default-initargs
                   :slots (loop for slot in slots collecting
                            (parse-slot slot))
                   :docstring docstring)))

(define-parser cl:define-condition (name superclasses slots &rest options)
  (let ((docstring (second (find :documentation options :key #'first))))
    (make-instance 'condition-node
                   :name name
                   :superclasses superclasses
                   :slots (loop for slot in slots collecting
                            (parse-slot slot))
                   :docstring docstring)))

(defun parse-struct-slot (slot)
  (cond ((not (listp slot))
         (make-instance 'struct-slot-node
                        :name slot))
        ((evenp (list-length slot))
         (destructuring-bind (name initform &key (type t) read-only) slot
           (let ((node (make-instance 'struct-slot-node
                                      :name name
                                      :type type
                                      :read-only read-only)))
             (setf (slot-value node 'initform) initform)
             node)))
        (t
         (destructuring-bind (name &key (type t) read-only) slot
           (make-instance 'struct-slot-node
                          :name name
                          :type type
                          :read-only read-only)))))

(define-parser cl:defstruct (name-and-options &rest slots)
  ;; Struct options can be bare keywords or be a list with the keyword being
  ;; the first element. The following function searches for the keyword in
  ;; either form and if it finds it, returns it in list form with the
  ;; keyword as the first item (that way one can tell between it and nil).
  (flet ((extract-struct-option (options key)
           (let ((entry (find key options
                              :key
                              #'(lambda (el) (if (and (listp el) el)
                                                 (first el)
                                                 el)))))
             (if (listp entry)
                 entry
                 (list entry)))))
    (let* ((name (if (listp name-and-options)
                     (first name-and-options)
                     name-and-options))
           (options (when (listp name-and-options)
                      (rest name-and-options)))
           (conc-name (destructuring-bind (&optional key conc-name)
                          (extract-struct-option options :conc-name)
                        (cond ((and key conc-name) (symbol-name conc-name))
                              ((and key (not conc-name)) "")
                              (t (concatenate 'string (symbol-name name) "-")))))
           (copier (destructuring-bind (&optional key (copier nil has-copier-p))
                       (extract-struct-option options :copier)
                     (if has-copier-p
                         copier
                         (intern (concatenate 'string "COPY-" (symbol-name name))
                                 (symbol-package name)))))
           (type (destructuring-bind (&optional key type)
                     (extract-struct-option options :type)
                   (declare (ignore key))
                   type))
           ;; initial-offset must default to 0 if :type was given
           (initial-offset (destructuring-bind (&optional key initial-offset)
                               (extract-struct-option options :initial-offset)
                             (declare (ignore key))
                             (cond (initial-offset initial-offset)
                                   (type 0))))
           (named (or (not type) (when (extract-struct-option options :named) t)))
           (predicate (destructuring-bind (&optional key (predicate nil has-predicate-p))
                          (extract-struct-option options :predicate)
                        (if has-predicate-p
                            predicate
                            (intern (concatenate 'string (symbol-name name) "-P")
                                    (symbol-package name)))))
           (print-function (destructuring-bind (&optional key print-function)
                               (extract-struct-option options :print-function)
                             (declare (ignore key))
                             print-function))
           (print-object (destructuring-bind (&optional key print-object)
                             (extract-struct-option options :print-object)
                           (declare (ignore key))
                           print-object))
           (docstring (if (stringp (first slots))
                          (first slots)
                          nil))
           (slots (if (stringp (first slots))
                      (rest slots)
                      slots)))
      (destructuring-bind (&optional key include-name &rest include-slots)
          (extract-struct-option options :include)
        (declare (ignore key))
        (make-instance 'struct-node
                       :name name
                       :docstring docstring
                       :conc-name conc-name
                       :copier copier
                       :initial-offset initial-offset
                       :type type
                       :named named
                       :predicate predicate
                       :print-function print-function
                       :print-object print-object
                       :include-name include-name
                       :include-slots (loop for slot in include-slots collecting
                                           (parse-struct-slot slot))
                       :slots (loop for slot in slots collecting
                                   (parse-struct-slot slot)))))))

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
                   :name name
                   :docstring docstring
                   :return-type return-type
                   :lambda-list args)))

(define-parser cffi:defctype (name base-type &optional docstring)
  (make-instance 'cffi-type
                 :name name
                 :docstring docstring
                 :base-type base-type))

(defun parse-cffi-slot (form)
  (destructuring-bind (name type &rest rest) form
    (declare (ignore rest))
    (make-instance 'cffi-slot
                   :name name
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
                   :name name
                   :docstring docstring
                   :slots (loop for slot in slots collecting
                            (parse-cffi-slot slot)))))

(define-parser cffi:defcunion (name &rest doc-and-slots)
  (let ((docstring (if (stringp (first doc-and-slots))
                       (first doc-and-slots)
                       nil))
        (slots (if (stringp (first doc-and-slots))
                   (rest doc-and-slots)
                   nil)))
    (make-instance 'cffi-union
                   :name name
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
                   :name name
                   :docstring docstring
                   :variants (loop for variant in enum-list collecting
                               (if (listp variant)
                                   (first variant)
                                   variant)))))

(define-parser cffi:defbitfield (name-and-options &rest masks)
  (let ((name (if (listp name-and-options)
                  (first name-and-options)
                  name-and-options))
        (docstring (if (stringp (first masks))
                       (first masks)
                       nil))
        (masks (if (stringp (first masks))
                   (rest masks)
                   nil)))
    (make-instance 'cffi-bitfield
                   :name name
                   :docstring docstring
                   :masks (loop for mask in masks collecting
                            (if (listp mask)
                                (first mask)
                                mask)))))
