(in-package :common-doc)

(defparameter *registry* (make-hash-table :test #'equal))

(defmacro define-node (name (&rest superclasses) slots &rest class-options)
  (flet ((tag-name-p (opt)
           (and (listp opt)
                (eq (first opt) :tag-name))))
    (let ((tag-name (find-if #'tag-name-p class-options))
          (class-options (remove-if #'tag-name-p class-options)))
      `(progn
         (defclass ,name ,superclasses
           ,slots
           ,@class-options)
         ,(if tag-name
              `(setf (gethash ,(cadr tag-name) *registry*) ',name))
         t))))

(defun find-node (tag-name)
  (gethash tag-name *registry*))
