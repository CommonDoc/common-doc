(in-package :cl-user)
(defpackage common-doc-test
  (:use :cl :fiveam :common-doc)
  (:export :basic-tests))
(in-package :common-doc-test)

(def-suite basic-tests
  :description "common-doc tests.")
(in-suite basic-tests)

(test constructors
  (is-true
   (typep (make-strikethrough nil) 'strikethrough))
  (is-true
   (typep (make-superscript nil) 'superscript))
  (is-true
   (typep (make-subscript nil) 'subscript))
  (let ((node (make-code-block "lisp" nil)))
    (typep node 'code-block)
    (is (equal (language node) "lisp")))
  (is-true
   (typep (make-inline-quote nil) 'inline-quote))
  (is-true
   (typep (make-block-quote nil) 'block-quote))
  (is-true
   (typep (make-document-link "doc" "sec" nil)
          'document-link))
  (is-true
   (typep (make-web-link "http://www.example.com" nil)
          'web-link))
  (is-true
   (typep (make-list-item nil) 'list-item))
  (is-true
   (typep (make-unordered-list nil) 'unordered-list))
  (is-true
   (typep (make-ordered-list nil) 'ordered-list))
  (let ((def (make-definition nil nil)))
    (is-true
     (typep def 'definition))
    (is-true (make-definition-list (list def))
             'definition-list)))


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
