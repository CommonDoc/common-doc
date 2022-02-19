(in-package :common-doc)

;;; Utilities

(defun construct (class children metadata reference)
  "Instantiate a class with children and metadata."
  (make-instance class
                 :children (uiop:ensure-list children)
                 :metadata metadata
                 :reference reference))

;; NOTE:
;; Originally, I wanted something like that, to keep metadata and
;; slots in sync, but reality is more complex and web-link's URI slot
;; contains QURI:URI object whereas Scriba expects that metadata's URI
;; item has a string.
;;
;; Thus I've decided to create correct meta-data in the web-link's contructor :(
;;
;; QUESTION: May be it is Scriba should be fixed, to fill attributes
;;           from slot values of items returned by FIND-SPECIAL-SLOTS?
;; 
;; (defmethod initialize-instance :after ((node document-node) &rest initargs)
;;   (declare (ignore initargs))

;;   ;; We have to keep metadata and slot values syncronized, because
;;   ;; some formats like Scriba when emiting node attributes take
;;   ;; their names and values from node's metadata.
;;   (loop with special-slots = (common-doc:find-special-slots (class-of node))
;;         for (meta-name . slot-name) in special-slots
;;         for slot-value = (when (slot-boundp node slot-name)
;;                            (slot-value node slot-name))
;;         for meta-value = (get-meta node meta-name)
;;         when (and meta-value
;;                   (not (equal meta-value slot-value)))
;;           do (warn "Node ~S has different value for slot ~S. In metadata: ~S and in slot ~S."
;;                    node slot-name meta-value slot-value)
;;         when slot-value
;;           do (setf (get-meta node meta-name)
;;                    slot-value)))

;;; Interface

(defun make-content (children &key metadata reference)
  "Create a content node from its children."
  (construct 'content-node children metadata reference))

(defun make-text (string &key metadata reference)
  "Create a text node from the contents of a string."
  (make-instance 'text-node
                 :text string
                 :metadata metadata
                 :reference reference))

(defun make-paragraph (children &key metadata reference)
  "Create a paragraph node from its children."
  (construct 'paragraph children metadata reference))

(defun make-bold (children &key metadata reference)
  "Create a bold node from its children."
  (construct 'bold children metadata reference))

(defun make-italic (children &key metadata reference)
  "Create an italicized node from its children."
  (construct 'italic children metadata reference))

(defun make-underline (children &key metadata reference)
  "Create an underlined node from its children."
  (construct 'underline children metadata reference))

(defun make-strikethrough (children &key metadata reference)
  "Create an striked out node from its children."
  (construct 'strikethrough children metadata reference))

(defun make-code (children &key metadata reference)
  "Create an inline code node from its children."
  (construct 'code children metadata reference))

(defun make-superscript (children &key metadata reference)
  "Create a superscripted node from its children."
  (construct 'superscript children metadata reference))

(defun make-subscript (children &key metadata reference)
  "Create a subscripted node from its children."
  (construct 'subscript children metadata reference))

(defun make-code-block (language children &key metadata reference)
  "Create a code block node from its children and language."
  (make-instance 'code-block
                 :language language
                 :children (uiop:ensure-list children)
                 :metadata metadata
                 :reference reference))

(defun make-inline-quote (children &key metadata reference)
  "Create an inline quote node from its children."
  (construct 'inline-quote children metadata reference))

(defun make-block-quote (children &key metadata reference)
  "Create a block quote node from its children."
  (construct 'block-quote children metadata reference))

(defun make-document-link (document reference children &key metadata)
  "Create a document link from document and node references and its children."
  (check-type document (or null string))
  (check-type reference (or null string))
  
  (let ((node (make-instance 'document-link
                             :document-reference document
                             :node-reference reference
                             :children (uiop:ensure-list children)
                             :metadata metadata)))
    ;; Scriba expects there will be a STRING in this metadata item:
    (when document
      (setf (get-meta node "doc")
            document))
    (when reference
      (setf (get-meta node "id")
            reference))
    (values node)))

(defun make-web-link (uri children &key metadata reference)
  "Create a web link."
  (let ((node (make-instance 'web-link
                             :uri (quri:uri uri)
                             :children (uiop:ensure-list children)
                             :metadata metadata
                             :reference reference)))
    ;; Scriba expects there will be a STRING in this metadata item:
    (setf (get-meta node "uri")
          uri)
    (values node)))

(defun make-list-item (children &key metadata reference)
  "Create a list item."
  (construct 'list-item children metadata reference))

(defun make-definition (term definition &key metadata reference)
  "Create a definition list item."
  (make-instance 'definition
                 :term term
                 :definition definition
                 :metadata metadata
                 :reference reference))

(defun make-unordered-list (children &key metadata reference)
  "Create an unordered list."
  (construct 'unordered-list children metadata reference))

(defun make-ordered-list (children &key metadata reference)
  "Create an ordered list."
  (construct 'ordered-list children metadata reference))

(defun make-definition-list (children &key metadata reference)
  "Create a definition list."
  (construct 'definition-list children metadata reference))

(defun make-image (source &key description metadata reference)
  "Create an image."
  (make-instance 'image
                 :source source
                 :description description
                 :metadata metadata
                 :reference reference))

(defun make-figure (image description &key metadata reference)
  "Create a figure."
  (make-instance 'figure
                 :image image
                 :description description
                 :metadata metadata
                 :reference reference))

(defun make-table (rows &key metadata reference)
  "Create a table from a list of rows."
  (make-instance 'table
                 :rows rows
                 :metadata metadata
                 :reference reference))

(defun make-row (cells &key metadata reference)
  "Create a row from a list of cells."
  (make-instance 'row
                 :cells cells
                 :metadata metadata
                 :reference reference))

(defun make-cell (children &key metadata reference)
  "Create a cell from its children."
  (construct 'cell children metadata reference))

(defun make-section (title &key children reference metadata)
  "Create a section from its title and children."
  
  (let ((title (loop for item in (uiop:ensure-list title)
                     collect (etypecase item
                               (string (make-text item))
                               (document-node item)))))
    (make-instance 'section
                   :title title
                   :reference reference
                   :children (uiop:ensure-list children)
                   :metadata metadata)))

(defun make-document (title &key children keywords &allow-other-keys)
  "Create a document."
  (make-instance 'document
                 :title title
                 :children (uiop:ensure-list children)
                 :keywords keywords))
