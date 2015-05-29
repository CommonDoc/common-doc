(defpackage traverse-example
  (:use :cl :common-doc)
  (:import-from :common-doc.ops
                :with-document-traversal))
(in-package :traverse-example)

(defvar *document*
  (make-document "test"
                 :children
                 (list
                  (make-bold
                   (list
                    (make-italic
                     (list
                      (make-underline
                       (list (make-text "Hello, world!"))))))))))

(with-document-traversal (*document* node)
  (print node))

;; #<DOCUMENT "test"> 
;; #<BOLD children: ITALIC> 
;; #<ITALIC children: UNDERLINE> 
;; #<UNDERLINE children: TEXT-NODE> 
;; #<TEXT-NODE text: Hello, world!> 
;; NIL
