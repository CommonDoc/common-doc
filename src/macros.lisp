(in-package :common-doc.macro)

(defclass <macro-node> (<content-node>)
  ((name :accessor name
         :initarg :name
         :type string
         :documentation "The name of the macro."))
  (:documentation "A macro to be expanded."))

(defgeneric expand-macro (node)
  (:documentation "Replace a macro node with a primitive node."))

(defmethod expand-macro ((node <document-node>))
  "The default macroexpansion: Do nothing."
  t)

(defmethod expand-macro ((macro <macro-node>))
  (error 'common-doc.error:<no-macro-expander> :node macro))
