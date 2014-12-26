(in-package :cl-user)
(defpackage common-doc-test
  (:use :cl :fiveam))
(in-package :common-doc-test)

(def-suite tests
  :description "common-doc tests.")
(in-suite tests)

(test simple-doc
  (let ((doc
          (make-instance 'common-doc:<document>
                         :title "My Document"
                         :creator "me"
                         :keywords (list "test" "test1")
                         :content
                         (list
                          (make-instance 'common-doc:<paragraph>
                                         :children
                                         (list
                                          (make-instance 'common-doc:<text-node>
                                                         :text "Hi!")))))))
    (is
     (equal (common-doc:keywords doc) (list "test" "test1")))))

(run! 'tests)
