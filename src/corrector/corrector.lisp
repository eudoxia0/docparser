(in-package :cl-user)
(defpackage docparser/corrector
  (:use :cl)
  (:export parse)
)

(in-package :docparser/corrector)

(defparameter *index* nil)

(defun make-index (system)
  (setf *index* (docparser:parse system)))

(defun access (package-no oject-no)
  (aref
   (docparser::package-index-nodes
    (aref (docparser::index-packages *index*) package-no))
   oject-no))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; correct
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric correct (node))

(defmethod correct ((node t))
  node)

(defmethod correct ((node DOCPARSER:VARIABLE-NODE))
  (let ((name (docparser:node-name node)))
    (setf (slot-value node 'docparser:node-docstring)
          (documentation name 'variable))))

(defmethod correct ((node DOCPARSER:FUNCTION-NODE))
  (let ((name (docparser:node-name node))
        (setfp (slot-value node 'docparser::setfp)))
    (cond
      ((null setfp)
       (setf (slot-value node 'docparser:node-docstring)
        (documentation name 'function)))
      (setfp
       (setf
        (slot-value node 'docparser:node-docstring)
        (documentation `(setf ,name) 'function))))))

(defmethod correct ((node DOCPARSER:MACRO-NODE))
  (let ((name (docparser:node-name node))
        (setfp (slot-value node 'docparser::setfp)))
    (cond
      ((null setfp)
       (setf (slot-value node 'docparser:node-docstring)
        (documentation name 'function)))
      (setfp
       (setf
        (slot-value node 'docparser:node-docstring)
        (documentation `(setf ,name) 'function))))))

(defmethod correct ((node DOCPARSER:STRUCT-NODE))
  (let ((name (docparser:node-name node)))
    (setf (slot-value node 'docparser:node-docstring)
          (documentation name 'type))))

(defmethod correct ((node DOCPARSER:TYPE-NODE))
  (let ((name (docparser:node-name node))
        (setfp (slot-value node 'docparser::setfp)))
    (cond
      ((null setfp)
       (setf (slot-value node 'docparser:node-docstring)
             (documentation name 'type)))
      (setfp ;; Does it have sense? How to determine setf type?
       (setf
        (slot-value node 'docparser:node-docstring)
        (documentation `(setf ,name) 'type))))))

(defmethod correct ((node DOCPARSER:CLASS-NODE))
  (let ((name (docparser:node-name node)))
    (setf (slot-value node 'docparser:node-docstring)
          (documentation name 'type))))

(defmethod correct ((node DOCPARSER:CONDITION-NODE))
  (let ((name (docparser:node-name node)))
    (setf (slot-value node 'docparser:node-docstring)
          (documentation name 'type))))

(defmethod correct ((node DOCPARSER:GENERIC-FUNCTION-NODE))
  (let ((name (docparser:node-name node))
        (setfp (slot-value node 'docparser::setfp)))
    (cond
      ((null setfp)
       (setf
        (slot-value node 'docparser:node-docstring)
        (documentation name 'function)))
      (setfp
       (setf
        (slot-value node 'docparser:node-docstring)
        (documentation `(setf ,name) 'function))))))

(defmethod correct ((node DOCPARSER:METHOD-NODE))
  (let* ((name (docparser:node-name node))
         (setfp (slot-value node 'docparser::setfp))
         (qualifiers (slot-value node 'docparser::qualifiers))
         (specializers
           (mapcar #'(lambda(el)
                       (if (consp el) (second el) t))
                   (slot-value node 'docparser::lambda-list))))
    (cond
      ((null setfp)
       (setf
        (slot-value node 'docparser:node-docstring)
        (documentation
         (find-method
          (eval (read-from-string (format nil "#'~S" name)))
          qualifiers
          specializers)
         t)))
      (setfp
       (setf
        (slot-value node 'docparser:node-docstring)
        (documentation
         (find-method
          (eval (read-from-string (format nil "#'(setf ~S)" name)))
          qualifiers
          specializers)
         t))))))

(defun correct-all (index)
  (loop :for p :from 0
          :below (array-dimension
                  (docparser::index-packages index)
                  0)
        :do
           (let ((package
                   (aref (docparser::index-packages index) p)))
             (loop :for n :from 0
                     :below
                     (array-dimension
                      (docparser::package-index-nodes
                       package)
                      0)
                   :do
                      (let ((node
                              (aref
                               (docparser::package-index-nodes
                                package)
                               n)))
                        #+nil(break "~S ~S~%" package node)  
                        (format t "~S ~S~%"
                                package
                                node)
                        (correct node)))))
  index)


(defun parse (system-or-list)
  (correct-all
   (docparser:parse system-or-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+nil
(
 (make-index :docparser/corrector/test)
 (docparser:dump docparser/corrector::*index*)

 (docparser/corrector::correct (docparser/corrector::access 0 13))
 (docparser/corrector::correct-all)
 )

(parse :docparser/corrector/test)