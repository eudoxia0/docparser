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
                 (:file "equal")
                 (:file "core")
                 (:file "parsers")
                 (:file "print"))))
  :description "Parse documentation from Common Lisp systems."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op docparser-test))))

(defsystem "docparser/corrector"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :maintainer "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :depends-on ("docparser")
  :components ((:module "src/corrector"
                :serial t
                :components
                ((:file "corrector"))))
  :description "Parses documentation from Common Lisp systems after
  they have been fully loaded into memory.")

(defsystem "docparser/corrector/test"
  :author "Mykola Matvyeyev <mnasoft@gmail.com>"
  :maintainer "Mykola Matvyeyev <mnasoft@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :depends-on (:trivial-types
               :alexandria
               :anaphora
               :cffi)
  :components ((:module "src/corrector/test"
                :serial t
                :components
                ((:file "test-system"))))
  :description "Test set for docparser/corrector.")
