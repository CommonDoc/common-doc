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
            (node-reference node))))

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

;;; Dumping documents

(defun dump (node &optional (stream *standard-output*))
  "Write the tree structure of the document tree to a stream."
  (labels (;; Utilities
           (write-depth (depth)
             (let ((string (make-string depth
                                        :initial-element #\Space)))
               (write-string string stream)))
           (node-name (node)
             (string-downcase (symbol-name (class-name (class-of node)))))
           (write-metadata (meta)
             (write-char #\Space stream)
             (write-char #\[ stream)
             (loop for key being the hash-keys of meta
                   for value being the hash-values of meta do
               (format stream "~A=~A" key value))
             (write-char #\] stream))
           ;; Actual printing
           (print-node (node depth)
             (write-depth depth)
             (write-string (node-name node) stream)
             (unless (typep node 'document)
               (awhen (metadata node)
                 (write-metadata it)))
             (write-char #\Newline stream)
             (cond
               ((typep node 'text-node)
                (write-depth (+ 2 depth))
                (format stream "~S~%" (text node)))
               ((and (not (or (typep node 'image)
                              (typep node 'definition)
                              (typep node 'table)))
                     (slot-boundp node 'children))
                (loop for child in (children node) do
                  (print-node child (+ 2 depth)))))))
    (print-node node 0)))

(defun dump-to-string (node)
  "Write the tree structure of the document tree to a string."
  (with-output-to-string (stream)
    (dump node stream)))
