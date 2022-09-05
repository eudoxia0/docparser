(defsystem docparser-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:docparser
               :fiveam
               ;; in order to refer sybmols from the
               ;; docparser-test-system package:
               :docparser-test-system)
  :components ((:module "t"
                :serial t
                :components
                ((:file "docparser")))))
