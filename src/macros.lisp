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

(defmethod expand-macros ((node document-node))
  "The default macroexpansion."
  node)

(defun %expand-macros (node)
  (let ((current-node (expand-macro node)))
    (assert (typep current-node '(or document-node document)))
    (if (and (slot-exists-p current-node 'children)
             (children current-node))
        (progn
          (setf (children current-node)
                (loop for child in (children current-node) collecting
                  (expand-macros child)))
          current-node)
        (if (subtypep (type-of current-node) 'macro-node)
            (expand-macros current-node)
            current-node))))

(defmethod expand-macros ((node base-list))
  "Expand the macros in a base list."
  (%expand-macros node))

(defmethod expand-macros ((node definition))
  "Expand the macros in a definition."
  (setf (term node)
        (loop for child in (term node) collecting
          (expand-macros child)))
  (setf (definition node)
        (loop for child in (definition node) collecting
          (expand-macros child)))
  node)

(defmethod expand-macros ((node content-node))
  "Expand the macros in a node with children."
  (%expand-macros node))

(defmethod expand-macros ((doc document))
  "Expand the macros in a document."
  (setf (children doc)
        (loop for child in (children doc) collecting
          (expand-macros child)))
  doc)

;;; Define metadata macros

(defvar *meta-macros* (make-hash-table :test #'equal)
  "A hash table of metadata macros to their definition.")

(define-node define-meta-macro (macro-node)
  ((name :reader meta-macro-name
         :initarg :name
         :type string
         :attribute-name "name"
         :documentation "The macro's name."))
  (:tag-name "defmetamacro")
  (:documentation "A metadata macro."))

(defmethod expand-macro ((node define-meta-macro))
  "Define the metadata macro, and return an empty text node."
  (setf (gethash (meta-macro-name node) *meta-macros*)
        (common-doc.ops:collect-all-text (children node)))
  (common-doc:make-text ""))
