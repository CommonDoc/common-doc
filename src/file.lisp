(in-package :common-doc.file)

(defvar *base-directory* *default-pathname-defaults*
  "The directory all resources with relative paths are based from. This is
intended to be bound by a @c(let) by specific input formats.")

(defun absolute-path (pathname-or-string)
  "Take a pathname or namestring. If it's absolute, return it, otherwise, merge
it with @c(*base-directory*)."
  (if (stringp pathname-or-string)
      (absolute-path (parse-namestring pathname-or-string))
      (if (uiop:absolute-pathname-p pathname-or-string)
          pathname-or-string
          (merge-pathnames pathname-or-string *base-directory*))))

(defun relativize-pathname (pathname)
  "If a pathname is inside @c(*base-directory*), return a relative
pathname. Otherwise, return the pathname unchanged."
  (if *base-directory*
      (let ((subpath (uiop:subpathp pathname *base-directory*)))
        (or subpath pathname))
      pathname))
