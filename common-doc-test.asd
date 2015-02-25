(defsystem common-doc-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :description "CommonDoc tests."
  :depends-on (:common-doc
               :common-doc-split-paragraphs
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "common-doc")
                 (:file "operations")
                 (:file "equality")
                 (:module "contrib"
                  :serial t
                  :components
                  ((:file "contrib")
                   (:file "split-paragraphs")))
                 (:file "final")))))
