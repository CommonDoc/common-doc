(in-package :common-doc)

(defparameter *registry* (make-hash-table :test #'equal)
  "A hash table from tag names to node classes.")

(defparameter *node-slots* (make-hash-table :test #'equal))

(defmacro define-node (name (&rest superclasses) slots &rest class-options)
  "Define a CommonDoc node."
  (flet ((tag-name-p (opt)
           "Whether `opt` is a list of the form `(:tag-name ...)`."
           (and (listp opt)
                (eq (first opt) :tag-name)))
         (extract-slot-names (slots)
           "Extract the value of the `:attribute-name` key of a slot, if it exists."
           (let ((final-slots (list))
                 (special-slots (list)))
             ;; `final-slots` is the list of slots that will be handed to the
             ;; `defclass` form, while `special-slots` is an alist of attribute
             ;; names to slot names
             (loop for slot in slots do
               (let ((slot-attrs (rest slot)))
                 (awhen (getf slot-attrs :attribute-name)
                   (push (cons it (first slot)) special-slots))
                 (push (cons (first slot)
                             (alexandria:remove-from-plist slot-attrs :attribute-name))
                       final-slots)))
             (cons (reverse final-slots)
                   (reverse special-slots)))))
    (let* ((tag-name (find-if #'tag-name-p class-options))
           (class-options (remove-if #'tag-name-p class-options))
           (slots-and-special-slots (extract-slot-names slots))
           (slots (first slots-and-special-slots))
           (special-slots (rest slots-and-special-slots)))
      `(progn
         (defclass ,name ,superclasses
           ,slots
           ,@class-options)
         ,(if tag-name
              `(let ((class (find-class ',name)))
                 (setf (gethash ,(cadr tag-name) *registry*)
                       class)
                 (setf (gethash class *node-slots*)
                       ',special-slots)))
         t))))

(defun find-node (tag-name)
  "Find a node class by its tag name."
  (gethash tag-name *registry*))

(defun find-tag (class)
  "Return a node class' tag name."
  (loop for tag-name being the hash-keys of *registry*
        using (hash-value tag-class)
        if (equal class tag-class)
        return tag-name))

(defun find-special-slots (class)
  "Return a node class' special slots."
  (gethash class *node-slots*))
