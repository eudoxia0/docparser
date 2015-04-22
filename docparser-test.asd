(defsystem docparser-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:docparser
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "docparser")))))
