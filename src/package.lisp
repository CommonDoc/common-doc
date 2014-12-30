(in-package :cl-user)
(defpackage common-doc
  (:use :cl :trivial-types)
  (:export ;; Document classes
           :<document-node>
           :<content-node>
           :<text-node>
           :<paragraph>
           :<markup>
           :<bold>
           :<italic>
           :<underline>
           :<strikethrough>
           :<code>
           :<superscript>
           :<subscript>
           :<code-block>
           :<quote>
           :<inline-quote>
           :<block-quote>
           :<link>
           :<internal-link>
           :<external-link>
           :<web-link>
           :<list>
           :<list-item>
           :<definition>
           :<unordered-list>
           :<ordered-list>
           :<definition-list>
           :<image>
           :<figure>
           :<table>
           :<row>
           :<cell>
           :<section>
           :<document>
           ;; Accessors
           :metadata
           :children
           :text
           :language
           :section-reference
           :document-reference
           :uri
           :term
           :definition
           :items
           :source
           :description
           :image
           :rows
           :header
           :footer
           :cells
           :title
           :reference
           :title
           :creator
           :publisher
           :subject
           :description
           :keywords
           :rights
           :version
           :created-on
           ;; Methods
           :traverse-document
           :collect-figures
           :collect-tables
           ;; Format
           :<format>
           :parse-document
           :emit-document
           :emit-to-string
           ;; Macros
           :expand-macro
           ;; Utilities
           :doc)
  (:documentation "The CommonDoc package."))
(in-package :common-doc)
