(in-package :cl-user)
(defpackage common-doc.plantuml
  (:use :cl)
  (:import-from
   :common-doc
   :text
   :children
   :make-text
   :make-image
   :define-node)
  (:import-from
   :common-doc.macro
   :macro-node
   :expand-macro)
  (:import-from
   :common-doc.file
   :absolute-path
   :relativize-pathname)
  (:export :plantuml
   :image-path
   :*plantuml-command*)
  (:documentation "PlantUML contrib package."))
(in-package :common-doc.plantuml)

;;; Configuration

(defvar *plantuml-command* '("java" "-Djava.awt.headless=true" "-jar" "plantuml.jar")
  "The command used to execute the PlantUML program.

This is either a string, naming an executable which will be looked up
in the standard search path.   (Or a full path to the executable.)

Or a list of strings, where the first names the executable as above, and the rest
are command line arguments.

For PlantUML it should most likely resemble this:

@c('(\"java\" \"-Djava.awt.headless=true\" \"-jar\" \"plantuml.jar\")).

This is the default.  But most likely this needs to be modified by replacing @c(plantuml.jar) with
the correct jar file and with a full path so @c(java) can find it.

The option @c(-Djava.awt.headless=true) is optional, but without it a temporary foreground process
will steal focus.")

;;; Classes

(define-node plantuml (macro-node)
	     ((path :reader image-path
		    :initarg :path
		    :type string
		    :attribute-name "path"
		    :documentation "Path to file where the image will be stored.")
	      (output-format :reader output-format
			     :initarg :output-format
			     :type string
			     :attribute-name "format"
			     :documentation "Output format for @c(PlantUML), used as argument @c(-t <format>)."))
  (:tag-name "plantuml")
  (:documentation "Macro to include PlantUML diagrams."))

;;; Macroexpansion

(defmethod expand-macro ((plot plantuml))
  (let ((pathname (absolute-path (image-path plot)))
	(text (text (first (children plot))))
	(command (concatenate 'list *plantuml-command*
			      (list (concatenate 'string "-t" (or (output-format plot) "png"))
				    "-p"))))
    (handler-case
	(progn
	  (with-input-from-string (stream text)
	    (uiop:run-program command :input stream :output pathname))
	  (make-image (namestring (relativize-pathname pathname))))
      (t (e)
	(make-text (format nil "PlantUML error: ~A" e))))))
