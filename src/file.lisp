(in-package :common-doc.file)

(defparameter *base-directory* *default-pathname-defaults*
  "The directory all resources with relative paths are based from. This is
intended to be bound by a `let` by specific input formats.")

(defgeneric absolute-path (pathname-or-string)
  (:documentation "Take a pathname or string. If it's absolute, return it,
  otherwise, merge it with *base-directory*."))

(defmethod absolute-path ((pathname pathname))
  "Return the absolute path of a pathname."
  (if (fad:pathname-absolute-p pathname)
      pathname
      (merge-pathnames pathname *base-directory*)))

(defmethod absolute-path ((string string))
  "Parse a string into a pathname and return the absolute version. Signals
`common-doc.error:<bad-pathname>` if parsing fails."
  (handler-case
      (absolute-path (parse-namestring string))
    (parse-error (c)
      (declare (ignore c))
      (error 'common-doc.error:<bad-pathname> :path-string string))))
