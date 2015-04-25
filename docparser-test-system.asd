(defsystem docparser-test-system
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :description "Test system that's loaded and parsed."
  :depends-on (:cffi)
  :components ((:module "t"
                :components
                ((:file "test-system")))))
