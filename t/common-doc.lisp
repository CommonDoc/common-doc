(in-package :cl-user)
(defpackage common-doc-test
  (:use :cl :fiveam :common-doc)
  (:export :basic-tests))
(in-package :common-doc-test)

(def-suite basic-tests
  :description "common-doc tests.")
(in-suite basic-tests)

(test simple-doc
  (let ((document (make-document
                   "My Document"
                   :creator "me"
                   :keywords (list "test" "test1")
                   :children
                   (list
                    (make-paragraph
                     (list
                      (make-text "test")))))))
    (is
     (equal (keywords document)
            (list "test" "test1")))
    (is (equal (common-doc.ops:collect-all-text document)
               "test"))))
