(in-package :cl-user)
(defpackage common-doc.gnuplot
  (:use :cl)
  (:import-from :common-doc
                :text
                :children
                :make-text
                :make-image
                :define-node)
  (:import-from :common-doc.macro
                :macro-node
                :expand-macro)
  (:import-from :common-doc.file
                :absolute-path
                :relativize-pathname)
  (:export :gnuplot
           :image-path
           :*gnuplot-command*
           :*gnuplot-default-term*)
  (:documentation "gnuplot contrib package."))
(in-package :common-doc.gnuplot)

;;; Configuration

(defvar *gnuplot-command* "gnuplot"
  "The path/executable name used for @c(gnuplot).  The default is @c(\"gnuplot\").

It is either a string, which indicates an executable name, or full
path to the executable.  Or it is a list of strings.  If it is a list,
the first element is the executable and the rest are command line
arguments.")

(defvar *gnuplot-default-term*
  #+darwin "png"
  #-darwin "pngcairo"
  "Default terminal to use for gnuplot.  The default is @c(pngcairo) except for mac OSX because
@c(pngcairo) is not available.   On OSX the default terminal is @c(png).")
;;; Classes

(define-node gnuplot (macro-node)
  ((path :reader image-path
         :initarg :path
         :type string
         :attribute-name "path"
         :documentation "Path to file where the image will be stored.")
   (term :reader term
	 :initarg :term
	 :type string
	 :attribute-name "term"
	 :documentation "Terminal used by gnuplot as in @c(set term <term>).  The default is taken from @c(*gnuplot-default-term*)."))
  (:tag-name "gnuplot")
  (:documentation "gnuplot plot."))

;;; Macroexpansion

(defmethod expand-macro ((plot gnuplot))
  "Take the gnuplot source code from the children and the image name, render it
with gnuplot into an image."
  (let* ((pathname (absolute-path (image-path plot)))
         ;; The gnuplot commands
         (text (text (first (children plot))))
         ;; The gnuplot commands to set output format, file etc.
         (input (format nil "set term ~S; set output ~S; ~A~%"
			(or (term plot) *gnuplot-default-term*)
                        (namestring pathname)
                        text))
         ;; The gnuplot command
         (command "gnuplot"))
    ;; Run
    (handler-case
        (progn
          (with-input-from-string (stream input)
            (uiop:run-program command :input stream))
          (make-image (namestring (relativize-pathname pathname))))
      (t (e)
        (make-text (format nil "gnuplot error: ~A" e))))))
