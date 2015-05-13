(in-package :common-doc.util)

(defun string-to-slug (string)
  "Take a string, usually the name of a section, and create something that is
more similar to an identifier, i.e. no spaces, same case, etc."
  (let* ((no-space (substitute #\- #\Space string))
         (no-slashes (substitute #\- #\/ no-space))
         (no-colons (substitute #\- #\: no-slashes)))
    (string-downcase no-colons)))
