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
           :image-path)
  (:documentation "gnuplot contrib package."))
(in-package :common-doc.gnuplot)

;;; Classes

(define-node gnuplot (macro-node)
  ((path :reader image-path
         :initarg :path
         :type string
         :attribute-name "path"
         :documentation "Path to file where the image will be stored."))
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
         (input (format nil "set term pngcairo; set output ~S; ~A~%"
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
      (t ()
        (make-text "gnuplot error.")))))
