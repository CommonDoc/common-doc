(in-package :cl-user)
(defpackage common-doc.gnuplot
  (:use :cl)
  (:documentation "gnuplot contrib package."))
(in-package :common-doc.gnuplot)

;;; Classes

(common-doc:define-node gnuplot (common-doc.macro:macro-node)
  ((path :reader image-path
         :initarg :path
         :type string
         :attribute-name "path"
         :documentation "Path to file where the image will be stored."))
  (:tag-name "gnuplot")
  (:documentation "gnuplot plot."))

;;; Macroexpansion

(defmethod common-doc.macro:expand-macro ((plot gnuplot))
  "Take the gnuplot source code from the children and the image name, render it
with gnuplot into an image."
  (let* ((pathname (common-doc.file:absolute-path (image-path plot)))
         ;; The gnuplot commands
         (text (common-doc:text (first (common-doc:children plot))))
         ;; The gnuplot commands to set output format, file etc.
         (input (format nil "set term png; set output ~S; ~A~%"
                        (namestring pathname)
                        text))
         ;; The gnuplot command
         (command "gnuplot"))
    ;; Run
    (handler-case
        (progn
          (with-input-from-string (stream input)
            (uiop:run-program command :input stream))
          (make-instance 'common-doc:image
                         :source (namestring (common-doc.file:relativize-pathname pathname))))
      (t ()
        (make-instance 'common-doc:text-node :text "gnuplot error.")))))
