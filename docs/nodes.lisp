(in-package :common-doc)

(make-document "My Document"
  :children
  (list
    (make-section (list (make-text "Introduction"))
      :children
      (list
        (make-paragraph
          (list (make-text "...")))))))

COMMON-DOC> (dump my-doc)
document
  section
    paragraph
      text-node
        "..."

CL-USER> (in-package :common-doc)
#<PACKAGE "COMMON-DOC">

COMMON-DOC> (text (make-text "Hello, world!"))
"Hello, world!"

COMMON-DOC> (children (make-paragraph
                        (list (make-text "This is ")
                              (make-text "a test"))))
(#<TEXT-NODE text: This is > #<TEXT-NODE text: a test>)

COMMON-DOC> (children (make-paragraph (list (make-text "This is ") (make-text "a test"))))
(#<TEXT-NODE text: This is > #<TEXT-NODE text: a test>)

COMMON-DOC> (make-code-block "lisp" (list (make-text "(progn ...)")))
#<CODE-BLOCK children: TEXT-NODE>

COMMON-DOC> (language *)
"lisp"

COMMON-DOC> (children **)
(#<TEXT-NODE text: (progn ...)>)
