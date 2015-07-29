(in-package :common-doc.ops)

(defun fill-unique-refs (doc-or-node)
  "Recur through a document, giving unique reference IDs to each section."
  (let ((table (make-hash-table))
        (taken (make-hash-table :test #'equal))
        (current-pos 0))
    ;; 'table' is a hash table that maps the position of a section (0 for first,
    ;; 1 for second, etc.) to a unique section ID. 'taken' is a hash table of
    ;; non-section node references to booleans
    (labels ((ref-in-table-p (ref)
               ;; Determine if 'ref' is in the table.
               (member ref
                       (alexandria:hash-table-values table)
                       :test #'equal))
             (takenp (ref)
               (or (ref-in-table-p ref)
                   (gethash ref taken)))
             (generate-unique-ref (section)
               ;; Generate a unique ref for a section
               (let* ((title (common-doc.ops:collect-all-text (title section)))
                      (slug (common-doc.util:string-to-slug title)))
                 (loop while (takenp slug) do
                   (setf slug
                         (concatenate 'string
                                      (write-to-string current-pos)
                                      "-"
                                      slug))
                   (incf current-pos))
                 slug))
             (add-section-reference (section)
               (let ((ref (generate-unique-ref section)))
                 ;; Add it to the table
                 (setf (gethash current-pos table) ref)
                 ;; Set the section's reference to the ref
                 (setf (reference section) ref))))
      (with-document-traversal (doc-or-node node)
        (unless (typep node 'document)
          (if (reference node)
              (setf (gethash (reference node) taken) t)
              (when (typep node 'section)
                (add-section-reference node)))))
      doc-or-node)))
