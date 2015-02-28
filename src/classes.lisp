(in-package :common-doc)

;;; Basic classes

(define-node document-node ()
  ((metadata :accessor metadata
             :initarg :metadata
             :type (or null hash-table)
             :initform nil
             :documentation "Node metadata."))
  (:documentation "The base class of all document classes."))

(define-node content-node (document-node)
  ((children :accessor children
             :initarg :children
             :initform nil
             :type (proper-list document-node)
             :documentation "The node's children."))
  (:documentation "A node with children. This is the base class of all nodes
  that have a \\c{children} slot (Except \\c{document}, since this class
  inherits from document-node) and can also be used as a way to represent a
  generic grouping of elements. This is useful when building a CommonDoc
  document by parsing some input language."))

(define-node text-node (document-node)
  ((text :accessor text
         :initarg :text
         :type string
         :documentation "The node's text."))
  (:documentation "A node representing a bare string of text."))

(define-node paragraph (content-node)
  ()
  (:tag-name "p")
  (:documentation "A paragraph."))

;;; Markup

(define-node markup (content-node)
  ()
  (:documentation "The superclass of all inline markup elements."))

(define-node bold (markup)
  ()
  (:tag-name "b")
  (:documentation "Text in this element is bold."))

(define-node italic (markup)
  ()
  (:tag-name "i")
  (:documentation "Text in this element is italicized."))

(define-node underline (markup)
  ()
  (:tag-name "u")
  (:documentation "Text in this element is underlined."))

(define-node strikethrough (markup)
  ()
  (:tag-name "strike")
  (:documentation "Text in this element is striked out."))

(define-node code (markup)
  ()
  (:tag-name "c")
  (:documentation "Text in this element is monospaced or otherwise marked as
  code or computer output."))

(define-node superscript (markup)
  ()
  (:tag-name "sup")
  (:documentation "Text in this element is superscripted relative to containing
  elements."))

(define-node subscript (markup)
  ()
  (:tag-name "sub")
  (:documentation "Text in this element is subscripted relative to containing
  elements."))

;;; Code

(define-node code-block (content-node)
  ((language :accessor language
             :initarg :language
             :initform nil
             :type (or null string)
             :attribute-name "lang"
             :documentation "The language of the code block's contents."))
  (:tag-name "code")
  (:documentation "A block of code."))

;;; Quotes

(define-node base-quote (content-node)
  ()
  (:documentation "The base class of all quotes."))

(define-node inline-quote (base-quote)
  ()
  (:tag-name "q")
  (:documentation "A quote that occurs inside a paragraph in the document."))

(define-node block-quote (base-quote)
  ()
  (:tag-name "quote")
  (:documentation "A block quote."))

;;; Links

(define-node link (content-node)
  ()
  (:documentation "The base class for all links, internal and external."))

(define-node document-link (link)
  ((document-reference :accessor document-reference
                       :initarg :document-reference
                       :initform nil
                       :type (or null string)
                       :attribute-name "doc"
                       :documentation "A reference key for the linked document.
 If \\c{nil}, the link is only to a section within the document.")
   (section-reference :accessor section-reference
                      :initarg :section-reference
                      :type (or null string)
                      :attribute-name "sec"
                      :documentation "A reference key for the linked section."))
  (:tag-name "ref")
  (:documentation "A link to a section of this document, to another document and
  optionally a section within that document. See also the \\c{reference} slot in
  the \\c{document} class."))

(define-node web-link (link)
  ((uri :accessor uri
        :initarg :uri
        :type quri:uri
        :attribute-name "uri"
        :documentation "The URI of the external resource."))
  (:tag-name "link")
  (:documentation "An external link."))

;;; Lists

(define-node base-list (document-node)
  ()
  (:documentation "The base class of all lists."))

(define-node list-item (content-node)
  ()
  (:tag-name "item")
  (:documentation "The item in a non-definition list."))

(define-node definition (document-node)
  ((term :accessor term
         :initarg :term
         :type (proper-list document-node)
         :documentation "The definition term.")
   (definition :accessor definition
               :initarg :definition
               :type (proper-list document-node)
               :documentation "Defines the term."))
  (:documentation "An item in a definition list."))

(define-node unordered-list (base-list)
  ((children :accessor children
             :initarg :children
             :type (proper-list list-item)
             :documentation "The list of \\c{list-item} instances."))
  (:tag-name "list")
  (:documentation "A list where the elements are unordered."))

(define-node ordered-list (base-list)
  ((children :accessor children
             :initarg :children
             :type (proper-list list-item)
             :documentation "The list of \\c{list-item} instances."))
  (:tag-name "enum")
  (:documentation "A list where the elements are ordered."))

(define-node definition-list (base-list)
  ((children :accessor children
             :initarg :children
             :type (proper-list definition)
             :documentation "The list of \\c{definition} instances."))
  (:tag-name "deflist")
  (:documentation "A list of definitions."))

;;; Figures

(define-node image (document-node)
  ((source :accessor source
           :initarg :source
           :type string
           :attribute-name "src"
           :documentation "The source where the image is stored.")
   (description :accessor description
                :initarg :description
                :type (or null string)
                :initform nil
                :attribute-name "desc"
                :documentation "A plain text description of the image."))
  (:tag-name "image")
  (:documentation "An image."))

(define-node figure (document-node)
  ((image :accessor image
          :initarg :image
          :type image
          :documentation "The figure's image.")
   (description :accessor description
                :initarg :description
                :type (proper-list document-node)
                :documentation "A description of the image."))
  (:tag-name "figure")
  (:documentation "A figure, an image plus an annotation."))

;;; Tables

(define-node table (document-node)
  ((rows :accessor rows
         :initarg :rows
         :type (proper-list row)
         :documentation "The list of rows in a table."))
  (:tag-name "table")
  (:documentation "A table."))

(define-node row (document-node)
  ((header :accessor header
           :initarg :header
           :type (proper-list document-node)
           :documentation "The row header.")
   (footer :accessor footer
           :initarg :footer
           :type (proper-list document-node)
           :documentation "The row footer.")
   (cells :accessor cells
          :initarg :cells
          :type (proper-list cell)
          :documentation "The cells in the row."))
  (:tag-name "row")
  (:documentation "A row in a table."))

(define-node cell (content-node)
  ()
  (:tag-name "cell")
  (:documentation "A cell in a table."))

;;; Large-scale structure

(define-node section (content-node)
  ((title :accessor title
          :initarg :title
          :type (proper-list document-node)
          :attribute-name "title"
          :documentation "The section title.")
   (reference :accessor reference
              :initarg :reference
              :initform nil
              :type (or null string)
              :attribute-name "ref"
              :documentation "A reference key for this section."))
  (:tag-name "section")
  (:documentation "Represents a section in the document. Unlike HTML, where a
  section is just another element, sections in CommonDoc contain their contents."))

(defclass document ()
  ((children :accessor children
             :initarg :children
             :type (proper-list document-node)
             :documentation "The document's children nodes.")
   ;;; Metadata, mostly based on Dublin Core[1] and the OpenDocument[2] format.
   ;;;
   ;;; [1]: https://en.wikipedia.org/wiki/Dublin_Core
   ;;; [2]: https://en.wikipedia.org/wiki/OpenDocument_technical_specification#Metadata
   (title :accessor title
          :initarg :title
          :type string
          :documentation "The document's title.")
   (creator :accessor creator
            :initarg :creator
            :type string
            :documentation "The creator of the document.")
   (publisher :accessor publisher
              :initarg :publisher
              :type string
              :documentation "The document's publisher.")
   (subject :accessor subject
            :initarg :subject
            :type string
            :documentation "The subject the document deals with.")
   (description :accessor description
            :initarg :description
            :type string
            :documentation "A description of the document.")
   (keywords :accessor keywords
             :initarg :keywords
             :type (proper-list string)
             :documentation "A list of strings, each being a keyword for the document.")
   (reference :accessor reference
              :initarg :reference
              :type string
              :documentation "A reference string to uniquely identify the
              document within a certain context.")
   (language :accessor language
            :initarg :language
             :type string
             :documentation "An \\link[uri=http://www.ietf.org/rfc/rfc4646.txt]{RFC4646} string denoting the language the document is written in.")
   (rights :accessor rights
           :initarg :rights
           :type string
           :documentation "Information on the document's copyright.")
   (version :accessor version
            :initarg :version
            :type string
            :documentation "The document version.")
   (created-on :accessor created-on
               :initarg :created-on
               :type local-time:timestamp
               :initform (local-time:now)
               :documentation "The date and time when the document was
               created. By default, this is the date and time at instance
               creation."))
  (:documentation "A document."))
