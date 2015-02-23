;;;; In this file we keep the print-object methods for all the classes, which
;;;; help specially in debugging
(in-package :common-doc)

(defmethod print-object ((node text-node) stream)
  "Print a text-node."
  (print-unreadable-object (node stream :type t)
    (let ((string (if (> (length (text node)) 33)
                      (concatenate 'string
                                   (subseq (text node) 0 30)
                                   "...")
                      (text node))))
      (format stream "text: ~A" string))))

(defmethod print-object ((node content-node) stream)
  "Print an arbitrary content-node."
  (print-unreadable-object (node stream :type t)
    (format stream "children: ~{~A~#[~:;, ~]~}"
            (loop for child in (children node) collecting
              (type-of child)))))

(defmethod print-object ((node document-link) stream)
  "Print a text-node."
  (print-unreadable-object (node stream :type t)
    (format stream "document: ~A, section: ~A"
            (document-reference node)
            (section-reference node))))

(defmethod print-object ((node base-list) stream)
  "Print a list."
  (print-unreadable-object (node stream :type t)
    (format stream "~A items" (length (children node)))))

(defmethod print-object ((node section) stream)
  "Print a section."
  (print-unreadable-object (node stream :type t)
    (let ((title (common-doc.ops:collect-all-text (title node)))
          (ref (reference node)))
      (format stream "title: ~A, ref: ~A" title ref))))

(defmethod print-object ((doc document) stream)
  "Print a document."
  (print-unreadable-object (doc stream :type t)
    (format stream "~S" (title doc))))
