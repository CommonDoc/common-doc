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
   (typep (make-code nil) 'code))
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
            (list "test" "test1")))))

(test file
  (let ((common-doc.file:*base-directory* (user-homedir-pathname)))
    (is
     (equal
      (common-doc.file:absolute-path #p"file.txt")
      (merge-pathnames #p"file.txt"
                       (user-homedir-pathname))))
    (is
     (equal
      (common-doc.file:absolute-path "file.txt")
      (merge-pathnames #p"file.txt"
                       (user-homedir-pathname))))
    (is
     (equal
      (common-doc.file:relativize-pathname
       (merge-pathnames #p"file.txt"
                        (user-homedir-pathname)))
      #p"file.txt"))))

(define-node custom-macro (common-doc.macro:macro-node)
  ())

(defmethod common-doc.macro:expand-macro ((node custom-macro))
  (make-text "test"))

(test macros
  (is
   (typep (common-doc.macro:expand-macros (make-paragraph nil))
          'paragraph))
  (is
   (typep (common-doc.macro:expand-macros (make-document "title"))
          'document))
  (signals common-doc.error:no-macro-expander
    (common-doc.macro:expand-macros (make-instance 'common-doc.macro:macro-node)))
  (let ((doc
          (make-document
           "test"
           :children
           (list
            (make-unordered-list
             (list
              (make-list-item (list (make-instance 'custom-macro)))))
            (make-definition-list
             (list
              (make-definition (list (make-text "term"))
                               (list (make-instance 'custom-macro)))))))))
    (finishes
      (setf doc (common-doc.macro:expand-macros doc)))))

(test print
  (is
   (equal (prin1-to-string (make-text "abc"))
          "#<TEXT-NODE text: abc>"))
  (is
   (equal (prin1-to-string (make-text "The quick brown fox jumps over the lazy dog"))
          "#<TEXT-NODE text: The quick brown fox jumps over...>"))
  (is
   (equal (prin1-to-string (make-paragraph
                            (list (make-text "test")
                                  (make-text "abc"))))
          "#<PARAGRAPH children: TEXT-NODE, TEXT-NODE>"))
  (finishes
   (prin1-to-string (make-document-link "doc" "sec" nil))
   (prin1-to-string (make-unordered-list nil))
   (prin1-to-string (make-section (list (make-text "title"))))
   (prin1-to-string (make-document "title")))
  (is
   (equal (dump-to-string (make-text "test"))
"text-node
  \"test\"
"))
  (is
   (equal (dump-to-string (make-paragraph
                            (list (make-text "a")
                                  (make-text "b"))))
"paragraph
  text-node
    \"a\"
  text-node
    \"b\"
"))
  (is
   (equal (dump-to-string (make-text "test"
                                     :metadata (make-meta
                                                (list
                                                 (cons "a" 1)))))
"text-node [a=1]
  \"test\"
")))

;;; Formats

(defclass custom-format (common-doc.format:document-format)
  ())

(defmethod common-doc.format:parse-document ((document-format custom-format)
                                             input)
  (make-document "test document"))

(defmethod common-doc.format:emit-document ((document-format custom-format)
                                             (node document-node) stream)
  (write-string "test node" stream))

(defmethod common-doc.format:emit-document ((document-format custom-format)
                                             (document document) stream)
  (write-string "test document" stream))

(test formats
  (let ((doc (common-doc.format:parse-document (make-instance 'custom-format)
                                               "test")))
    (is
     (equal (title doc)
            "test document"))
    (is
     (equal (common-doc.format:emit-to-string (make-instance 'custom-format)
                                              doc)
            "test document"))
    (is
     (equal (common-doc.format:emit-to-string (make-instance 'custom-format)
                                              (make-text "test"))
            "test node"))))

(define-node macro-a (common-doc.macro:macro-node)
  ())

(define-node macro-b (common-doc.macro:macro-node)
  ())

(defmethod common-doc.macro:expand-macro ((node macro-a))
  (make-instance 'macro-b))

(defmethod common-doc.macro:expand-macro ((node macro-b))
  (make-text "test"))

(test recursive-macros
  (let ((doc
          (make-document
           "test"
           :children
           (list
            (make-instance 'macro-a)))))
    (finishes
      (setf doc (common-doc.macro:expand-macros doc)))
    (is
     (equal (length (children doc))
            1))
    (let ((child (first (children doc))))
      (is
       (typep child 'text-node))
      (is
       (equal (text child)
              "test")))))
