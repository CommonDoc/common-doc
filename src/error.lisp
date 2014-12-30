(in-package :common-doc.error)

(define-condition <common-doc-error> (simple-error)
  ()
  (:documentation "The base class of all CommonDoc errors."))

(define-condition <macro-error> (<common-doc-error>)
  ()
  (:documentation "The base class of all macro-related errors."))

(define-condition <no-macro-expander> (<macro-error>)
  ((node :accessor node
         :initarg :node
         :type common-doc.macro:<macro-node>))
  (:report
   (lambda (condition stream)
     (format stream "No expand-macro method for node ~S."
             (type-of (node condition)))))
  (:documentation "Signaled when a macro node has no `expand-macro` method."))
