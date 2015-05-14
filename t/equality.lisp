(in-package :common-doc-test.ops)

(defun limit-list (list)
  (if (> (length list) 3)
      (subseq list 0 3)
      list))

(test node-equality
  (let* ((text (make-text "test"))
         (text-meta (make-text "test"
                               :metadata (make-meta (list (cons "a" 1)))))
         (code-block (make-code-block "lisp"
                                      (list (make-text "test"))))
         (doc-link (make-document-link "doc" "sec"
                                       (list (make-text "test"))))
         (web-link (make-web-link "http://www.example.com"
                                  (list (make-text "test"))))
         (image (make-image "fig1.jpg"))
         (paragraph (make-paragraph
                     (list (make-text "test"))))
         (section (make-section
                   (list (make-text "Section 1"))
                   :children
                   (list
                    (make-figure
                     image
                     (list paragraph))))))
    (macrolet ((tests (&rest nodes)
                 `(progn
                    ,@(loop for node in nodes collecting
                            `(is (node-equal ,node ,node)))
                    ,@(loop for node in nodes collecting
                            `(progn
                               ,@(loop for other-node in (limit-list (set-difference nodes (list node)))
                                       collecting
                                       `(is (not (node-equal ,node ,other-node)))))))))
      (tests text text-meta code-block doc-link web-link image paragraph section))))
