(in-package :common-doc.error)

(define-condition common-doc-error (simple-error)
  ()
  (:documentation "The base class of all CommonDoc errors."))

(define-condition macro-error (common-doc-error)
  ()
  (:documentation "The base class of all macro-related errors."))

(define-condition no-macro-expander (macro-error)
  ((node :accessor node
         :initarg :node
         :type common-doc.macro:macro-node
         :documentation "The node that couldn't be expanded."))
  (:report
   (lambda (condition stream)
     (format stream "No expand-macro method for node ~S."
             (type-of (node condition)))))
  (:documentation "Signaled when a macro node has no `expand-macro` method."))

(define-condition bad-pathname (common-doc-error parse-error)
  ((path-string :accessor path-string
                :initarg :path-string
                :type string
                :documentation "The string that couldn't be parsed."))
  (:report
   (lambda (condition stream)
     (format stream "Could not parse '~A' into a pathname."
             (path-string condition))))
  (:documentation "An error when parsing a string into pathname."))
