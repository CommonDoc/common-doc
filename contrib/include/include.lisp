(in-package :cl-user)
(defpackage common-doc.include
  (:use :cl)
  (:import-from :common-doc.macro
                :<macro-node>
                :expand-macro)
  (:import-from :common-doc
                :define-node)
  (:import-from :common-doc.util
                :make-text)
  (:documentation "Includex package."))
(in-package :common-doc.include)

;;; Classes

(define-node <include> (<macro-node>)
  ((path :reader include-path
         :initarg :path
         :type string
         :attribute-name "path"
         :documentation "Path to the local file to include.")
   (range :reader include-range
          :initarg :range
          :type string
          :attribute-name "range"
          :documentation "The range of lines to include."))
  (:tag-name "include")
  (:documentation "Include an external file."))

;;; Macroexpansions

(defmethod expand-macro ((include <include>))
  "Expand the include file into a text node with its contents."
  (let ((text ""))
    (make-text text)))
