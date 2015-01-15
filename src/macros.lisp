(in-package :common-doc.macro)

(defclass macro-node (content-node)
  ((name :accessor name
         :initarg :name
         :type string
         :documentation "The name of the macro."))
  (:documentation "A macro to be expanded."))

(defgeneric expand-macro (node)
  (:documentation "Replace a macro node with a primitive node."))

(defmethod expand-macro ((node document-node))
  "The default macroexpansion: Do nothing."
  node)

(defmethod expand-macro ((macro macro-node))
  (error 'common-doc.error:no-macro-expander :node macro))

(defgeneric expand-macros (node)
  (:documentation "Recursively expand all macros in `node`."))

(defmethod expand-macros ((node content-node))
  "Expand the macros in a node with children."
  (let ((current-node (expand-macro node)))
    (if (children current-node)
        (progn
          (setf (children current-node)
                (loop for child in (children current-node) collecting
                  (expand-macros child)))
          current-node)
        current-node)))
