(defpackage html-example
  (:use :cl :common-doc))
(in-package :html-example)

(ql:quickload :common-html)

(defvar *document*
  (make-document
    "My Document"
    :children
    (list
     (make-section (list (make-text "Introduction"))
                   :children
                   (list
                    (make-paragraph
                     (list (make-text "This is an example of using CommonHTML.")))
                    (make-ordered-list
                     (list
                      (make-list-item
                       (list (make-text "Item one")))
                      (make-list-item
                       (list (make-text "Item two"))))
                     :metadata (make-meta (list (cons "class" "my-list"))))
                    (make-definition-list
                     (list
                      (make-definition
                       (list (make-text "Term"))
                       (list (make-text "Definition"))))))))))

(common-doc.format:emit-to-string (make-instance 'common-html:html)
                                  *document*)
"<!DOCTYPE html>
<html>
  <head>
    <title>My Document</title>
  </head>
  <body>
    <h1>Introduction</h1>

    <p>This is an example of using CommonHTML.</p>

    <ol class=\"my-list\">
      <li>Item one</li>
      <li>Item two</li>
    </ol>

    <dl>
      <dt>Term</dt
      <dd>Definition</dd>
    </dl>
  </body>
</html>"
