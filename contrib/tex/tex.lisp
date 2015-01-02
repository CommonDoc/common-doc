(in-package :cl-user)
(defpackage common-doc.tex
  (:use :cl)
  (:import-from :common-doc.macro
                :<macro-node>
                :expand-macro)
  (:import-from :common-doc
                :<text-node>
                :<content-node>
                :children
                :define-node)
  (:documentation "TeX package."))
(in-package :common-doc.tex)

;;; Classes

(define-node <tex> (<macro-node>)
  ()
  (:tag-name "tex")
  (:documentation "Inline TeX code."))

(define-node <tex-block> (<macro-node>)
  ()
  (:tag-name "texb")
  (:documentation "Block of TeX code."))

;;; Macroexpansions

(defmethod expand-macro ((tex <tex>))
  "Wrap the children in dollar signs."
  (make-instance '<content-node>
                 :children
                 (append (list (make-instance '<text-node> :text "$"))
                         (common-doc:children tex)
                         (list (make-instance '<text-node> :text "$")))))

(defmethod expand-macro ((texb <tex-block>))
  "Wrap the children in TeX block tags."
  (make-instance '<content-node>
                 :children
                 (append (list (make-instance '<text-node> :text "\\("))
                         (common-doc:children texb)
                         (list (make-instance '<text-node> :text "\\)")))))
