(defsystem common-doc-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:common-doc
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "common-doc")
                 (:file "operations")
                 (:file "equality")
                 (:file "final")))))
