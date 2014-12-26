(in-package :cl-user)
(defpackage common-doc-test
  (:use :cl :fiveam))
(in-package :common-doc-test)

(def-suite tests
  :description "common-doc tests.")
(in-suite tests)

(defun extract-doc-text (doc)
  (with-output-to-string (stream)
    (common-doc:traverse-document
     doc
     #'(lambda (node)
         (when (typep node 'common-doc:<text-node>)
           (write-string (common-doc:text node) stream))))))

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
                                                         :text "test")))))))
    (is
     (equal (common-doc:keywords doc) (list "test" "test1")))
    (is (equal (extract-doc-text doc)
               "test"))))

(run! 'tests)
