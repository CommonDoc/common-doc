(in-package :cl-user)
(defpackage common-doc
  (:use :cl :trivial-types))
(in-package :common-doc)

;;; Basic classes

(defclass <document-node> ()
  ((metadata :accessor metadata
             :initarg :metadata
             :type (or null hash-table)
             :initform nil
             :documentation "Node metadata."))
  (:documentation "The base class of all document classes."))

(defclass <content-node> (<document-node>)
  ((children :accessor children
             :initarg :children
             :type (proper-list <document-node>)
             :documentation "The node's children."))
  (:documentation "A node with generic content."))

(defclass <text-node> (<document-node>)
  ((text :accessor text
         :initarg :text
         :type string
         :documentation "The node's text."))
  (:documentation "A node representing a bare string of text."))

(defclass <paragraph> (<content-node>)
  ()
  (:documentation "A paragraph."))

;;; Markup

(defclass <markup> (<content-node>)
  ()
  (:documentation "The superclass of all inline markup elements."))

(defclass <bold> (<markup>)
  ()
  (:documentation "Text in this element is bold."))

(defclass <italic> (<markup>)
  ()
  (:documentation "Text in this element is italicized."))

(defclass <underline> (<markup>)
  ()
  (:documentation "Text in this element is underlined."))

(defclass <strikethrough> (<markup>)
  ()
  (:documentation "Text in this element is striked out."))

(defclass <code> (<markup>)
  ()
  (:documentation "Text in this element is monospaced or otherwise marked as
  code or computer output."))

(defclass <superscript> (<markup>)
  ()
  (:documentation "Text in this element is superscripted relative to containing
  elements."))

(defclass <subscript> (<markup>)
  ()
  (:documentation "Text in this element is subscripted relative to containing
  elements."))

;;; Code

(defclass <code-block> (<content-node>)
  ((language :accessor language
             :initarg :language
             :type string
             :documentation "The language of the code block's contents."))
  (:documentation "A block of code."))

(defclass <verbatim> (<document-node>)
  ((text :accessor text
         :initarg :text
         :type string
         :documentation "The node's text."))
  (:documentation "Contains a verbatim text string."))

;;; Quotes

(defclass <quote> (<content-node>)
  ()
  (:documentation "The base class of all quotes."))

(defclass <inline-quote> (<quote>)
  ()
  (:documentation "A quote that occurs inside a paragraph in the document."))

(defclass <block-quote> (<quote>)
  ()
  (:documentation "A block quote."))

;;; Links

(defclass <link> (<content-node>)
  ()
  (:documentation "The base class for all links, internal and external."))

(defclass <internal-link> (<link>)
  ((section-reference :accessor section-reference
                      :initarg :section-reference
                      :type string
                      :documentation "A reference key for the linked section."))
  (:documentation "A link to a section of this document."))

(defclass <external-link> (<link>)
  ((document-reference :accessor document-reference
                      :initarg :document-reference
                      :type string
                      :documentation "A reference key for the linked document.")
   (section-reference :accessor section-reference
                      :initarg :section-reference
                      :type string
                      :documentation "A reference key for the linked section."))
  (:documentation "A link to another document (See `reference` slot in the
  `<document>` class), and optionally a section within that document."))

(defclass <web-link> (<link>)
  ((uri :accessor uri
        :initarg :uri
        :type quri:uri
        :documentation "The URI of the external resource."))
  (:documentation "An external link."))

;;; Lists

(defclass <list> (<document-node>)
  ()
  (:documentation "The base class of all list nodes."))

(defclass <list-item> (<content-node>)
  ()
  (:documentation "The item in a non-definition list."))

(defclass <definition> (<document-node>)
  ((term :accessor term
         :initarg :term
         :type <document-node>
         :documentation "The definition term.")
   (definition :accessor definition
               :initarg :definition
               :type <document-node>
               :documentation "Defines the term."))
  (:documentation "An item in a definition list."))

(defclass <unordered-list> (<document-node>)
  ((items :accessor items
          :initarg :items
          :type (proper-list <list-item>)
          :documentation "The list of `<list-item>` instances."))
  (:documentation "A list where the elements are unordered."))

(defclass <ordered-list> (<document-node>)
  ((items :accessor items
          :initarg :items
          :type (proper-list <list-item>)
          :documentation "The list of `<list-item>` instances."))
  (:documentation "A list where the elements are ordered."))

(defclass <definition-list> (<document-node>)
  ((items :accessor items
          :initarg :items
          :type (proper-list <definition>)
          :documentation "The list of `<definition>` instances."))
  (:documentation "A list of definitions."))

;;; Figures

(defclass <image> (<document-node>)
  ((source :accessor source
           :initarg :source
           :type string
           :documentation "The source where the image is stored.")
   (description :accessor description
                :initarg :description
                :type string
                :documentation "A plain text description of the image."))
  (:documentation "An image."))

(defclass <figure> (<document-node>)
  ((image :accessor image
          :initarg :image
          :type <image>
          :documentation "The figure's image.")
   (description :accessor description
                :initarg :description
                :type (proper-list <document-node>)
                :documentation "A description of the image."))
  (:documentation "A figure, an image plus an annotation."))

;;; Large-scale structure

(defclass <section> (<content-node>)
  ((title :accessor title
          :initarg :title
          :type <document-node>
          :documentation "The section title.")
   (reference :accessor reference
              :initarg :reference
              :type string
              :documentation "A reference key for this section."))
  (:documentation "Represents a section in the document. Unlike HTML, where a
  section is just another element, sections in CommonDoc contain their contents."))

(defclass <document> ()
  ((content :accessor content
            :initarg :content
            :type (proper-list <document-node>)
            :documentation "The document's contents.")
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
             :documentation "An [RFC4646](http://www.ietf.org/rfc/rfc4646.txt) string denoting the language the document is written in.")
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

;;; Code

(defgeneric traverse-document (node function)
  (:documentation "Apply a side effectful function recursively to every element
  in the document. Depth-first.")

  (:method ((doc <document>) function)
    (funcall function doc)
    (loop for child in (content doc) do
      (traverse-document child function)))

  (:method ((cnode <content-node>) function)
    (funcall function cnode)
    (loop for child in cnode do
      (traverse-document child function)))

  (:method ((dnode <document-node>) function)
    (funcall function dnode)))

(defmethod collect-figures ((doc <document>))
  (let ((figures (list)))
    (traverse-document doc
                       #'(lambda (node)
                           (when (typep node '<figure>)
                             (push node figures))))
    figures))
