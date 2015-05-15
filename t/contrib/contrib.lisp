(in-package :cl-user)
(defpackage common-doc-test.contrib
  (:use :cl :fiveam :common-doc)
  (:export :contrib))
(in-package :common-doc-test.contrib)

(def-suite contrib)
(in-suite contrib)

(test gnuplot
  (let ((common-doc.file:*base-directory*
          (asdf:system-relative-pathname :common-doc-test
                                         #p"t/contrib/"))
        (doc (make-content
              (list
               (make-instance 'common-doc.gnuplot:gnuplot
                              :path "gnuplot.png"
                              :children
                              (list
                               (make-text "plot sin(x)/x"))))))
        (image-path (asdf:system-relative-pathname :common-doc-test
                                                   #p"t/contrib/gnuplot.png")))
    (finishes
      (setf doc (common-doc.macro:expand-macros doc)))
    (is-true
     (probe-file image-path))
    (delete-file image-path)))

(test include
  (let ((common-doc.file:*base-directory*
          (asdf:system-relative-pathname :common-doc-test
                                         #p"t/contrib/"))
        (node
          (make-content
           (list
            (make-instance 'common-doc.include:include
                           :path "contrib.lisp")))))
    (finishes
      (setf node (common-doc.macro:expand-macros node)))
    (is
     (typep (first (children node)) 'text-node))
    (is
     (equal (text (first (children node)))
            (uiop:read-file-string
             (asdf:system-relative-pathname :common-doc-test
                                            #p"t/contrib/contrib.lisp"))))))

(test split-paragraphs
  (let ((node
          (make-content
           (list
            (make-text
             (format nil "Paragraph 1.~%~%Paragraph with "))
            (make-bold
             (list (make-text "bold text")))
            (make-text (format nil ".~%~%Paragraph 3."))))))
    (finishes
      (common-doc.split-paragraphs:split-paragraphs node))
    (is-true
     (typep node 'content-node))
    (is
     (equal (length (children node))
            3))
    (is
     (every #'(lambda (node)
                (typep node 'paragraph))
            (children node)))))

(test tex
  (let ((tex-inline (make-content
                     (list
                      (make-instance 'common-doc.tex:tex
                                     :children
                                     (list
                                      (make-text "1+x"))))))
        (tex-block (make-content
                    (list
                     (make-instance 'common-doc.tex:tex-block
                                    :children
                                    (list
                                     (make-text "\\int \\log x")))))))
    (finishes
      (setf doc (common-doc.macro:expand-macros tex-inline)))
    (finishes
      (setf doc (common-doc.macro:expand-macros tex-block)))
    (is (equal (common-doc.ops:collect-all-text tex-inline)
               "$ 1+x $"))
    (is (equal (common-doc.ops:collect-all-text tex-block)
               "\\( \\int \\log x \\)"))))
