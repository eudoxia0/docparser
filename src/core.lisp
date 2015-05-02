(in-package :docparser)

;;; Loading systems

(defun load-system (system-name)
  "Load an ASDF system by name."
  (uiop:with-muffled-loader-conditions ()
    (uiop:with-muffled-compiler-conditions ()
      (asdf:load-system system-name :verbose nil :force t))))

;;; Indices

(defclass package-index ()
  ((name :reader package-index-name
         :initarg :name
         :type string
         :documentation "The package's name.")
   (docstring :reader package-index-docstring
              :initarg :docstring
              :type string
              :documentation "The package's docstring.")
   (nodes :accessor package-index-nodes
          :initform (make-array 0
                                :adjustable t
                                :element-type 'documentation-node
                                :fill-pointer 0)
          :type (vector documentation-node)
          :documentation "A vector of documentation objects."))
  (:documentation "Holds the documented objects in this package."))

(defclass index ()
  ((system-name :reader index-system-name
                :initarg :system-name
                :type string
                :documentation "The name of the parsed system.")
   (packages :accessor index-packages
             :initform (make-array 0
                                   :adjustable t
                                   :element-type 'package-index
                                   :fill-pointer 0)
             :type (vector package-index)
             :documentation "A vector of package indices."))
  (:documentation "Holds system documentation, and the internal package indices."))

(defun add-package-index (index package-index)
  (vector-push-extend package-index (index-packages index)))

(defun add-node (index node)
  "Add a node to an index, choosing the proper package."
  (let* ((symbol (node-name node))
         (symbol-package (docparser:symbol-node-package symbol))
         (package-index (find symbol-package
                              (index-packages index)
                              :key #'package-index-name)))
    (when package-index
      (vector-push-extend node (package-index-nodes package-index)))))

(defun query (index &key package-name symbol-name class)
  "Find all documentation nodes in the index matching the constraints and
returns them as a vector. If none are found, return an empty vector."
  (vector))

;;; Parsers

(defparameter *parsers* (list)
  "A list of symbols to the functions used to parse their corresponding forms.")

(defmacro define-parser (name (&rest lambda-list) &body body)
  "Define a parser."
  (let ((form (gensym)))
    `(push (cons ',name (lambda (,form)
                          (destructuring-bind ,lambda-list ,form
                            ,@body)))
           *parsers*)))

(defun parse-form (form)
  "Parse a form into a node."
  (let ((parser (rest (assoc (first form) *parsers*))))
    (when parser
      (funcall parser (rest form)))))

(defun parse-package-definition (form)
  (let ((name (symbol-name (first form)))
        (docstring (second (find :documentation (rest form) :key #'first))))
    (make-instance 'package-index
                   :name name
                   :docstring docstring)))

;;; Interface

(defun parse (system-name)
  "Parse documentation from a system."
  (let* ((system (asdf:find-system system-name))
         (index (make-instance 'index
                               :system-name (asdf:component-name system)))
         (old-macroexpander *macroexpand-hook*)
         (*macroexpand-hook*
           #'(lambda (function form environment)
               (when (listp form)
                 ;; Package nodes are special cases
                 (if (eql (first form) 'cl:defpackage)
                     ;; Parse the package definition, and add the new package
                     ;; index to the index
                     (let ((package-index (parse-package-definition (rest form))))
                       (add-package-index index package-index)
                       t)
                     ;; Regular node, parse it
                     (let ((node (parse-form form)))
                       (when node
                         (add-node index node))
                       t))
               ;; Always pass the form to the old macroexpander. This ensures
               ;; the system is loaded properly.
               (funcall old-macroexpander
                        function
                        form
                        environment)))))
    (load-system system-name)
    index))
