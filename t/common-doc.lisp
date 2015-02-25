(in-package :cl-user)
(defpackage common-doc-test
  (:use :cl :fiveam :common-doc)
  (:import-from :common-doc.util
                :doc
                :make-text))
(in-package :common-doc-test)

(def-suite tests
  :description "common-doc tests.")
(in-suite tests)

(test simple-doc
  (let ((document
          (doc
           document
           (:title "My Document"
            :creator "me"
            :keywords (list "test" "test1"))
           (paragraph
            ()
            (text-node
             (:text "test"))))))
    (is
     (equal (keywords document) (list "test" "test1")))
    (is (equal (common-doc.ops:collect-all-text document)
               "test"))))
