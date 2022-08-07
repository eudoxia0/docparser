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
#+nil
(
 (make-index :docparser/corrector/test)
 (docparser:dump docparser/corrector::*index*)

 (docparser/corrector::correct (docparser/corrector::access 0 6))
 (docparser/corrector::correct-all)
 )

(type-of (slot-value  (docparser/corrector::access 0 6) 'docparser::slots))

(parse :docparser/corrector/test)

(find-method  #'math/matr:row nil '(INTEGER ARRAY))
