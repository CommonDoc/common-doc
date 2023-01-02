(in-package :cl-user)
(defpackage common-doc.graphviz
  (:use :cl)
  (:import-from
   :common-doc
   :text
   :children
   :make-image
   :make-text
   :define-node)
  (:import-from
   :common-doc.macro
   :macro-node
   :expand-macro)
  (:import-from
   :common-doc.file
   :absolute-path
   :relativize-pathname)
  (:export
   :graphviz
   :image-path
   :*dot-command*)
  (:documentation "Graphviz package."))
(in-package :common-doc.graphviz)

;;; Global Configuration

(defvar *dot-command* "dot"
  "The path/executable name used for the @c(dot) command.  The default is @c(\"dot\").

It is either a string, which is taken as the name of an executable.
This can also be a full pathname to the executable.

Or it is a list of strings, where the first taken as the executable and
the rest are extra command line arguments.")

;;; Classes

(define-node graphviz (macro-node)
	     ((path :reader image-path
		    :initarg :path
		    :type string
		    :attribute-name "path"
		    :documentation "Path to file where the image will be stored.")
	      (output-format :reader output-format
			     :initarg :output-format
			     :type string
			     :attribute-name "format"
			     :documentation "Output format for @c(dot), used as argument @c(-T<format>)"))
  (:tag-name "graphviz")
  (:documentation "Graphviz diagram."))


;;; Macroexpansion

(defmethod expand-macro ((plot graphviz))
  "Take the graphviz zource code from the children and render it using dot into an image."
  (let* ((pathname (absolute-path (image-path plot)))
	 (text (text (first (children plot))))
	 (command (list *dot-command* "-T" (or (output-format plot) "png") "-o" (namestring pathname))))
    (handler-case
	(progn
	  (with-input-from-string (stream text)
	    (uiop:run-program command :input stream))
	  (make-image (namestring (relativize-pathname pathname))))
      (t (e)
	(make-text (format nil "graphviz dot error: ~A~%" e))))))
