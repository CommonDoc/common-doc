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

(test nodes
  (is
   (eql (find-node "b")
        (find-class 'bold)))
  (is
   (equal (find-tag (find-class 'bold))
          "b"))
  (is
   (equal (find-special-slots (find-class 'code-block))
          (list (cons "lang" 'language)))))


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

(test file
  (let ((common-doc.file:*base-directory* (user-homedir-pathname)))
    (is
     (equal
      (common-doc.file:absolute-path #p"file.txt")
      (merge-pathnames #p"file.txt"
                       (user-homedir-pathname))))
    (is
     (equal
      (common-doc.file:relativize-pathname
       (merge-pathnames #p"file.txt"
                        (user-homedir-pathname)))
      #p"file.txt"))))

(test macros
  (signals common-doc.error:no-macro-expander
    (common-doc.macro:expand-macros (make-instance 'common-doc.macro:macro-node))))
