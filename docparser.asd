(defsystem docparser
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :depends-on (:trivial-types
               :alexandria
               :anaphora
               :cffi)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "nodes")
                 (:file "core")
                 (:file "parsers")
                 (:file "print"))))
  :description "Parse documentation from Common Lisp systems."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op docparser-test))))
