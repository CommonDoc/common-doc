(in-package :common-doc.format)

(defclass <format> ()
  ()
  (:documentation "A data format that can be parsed into a CommonDoc document,
  or that a document can be formatted to."))

(defgeneric parse-document (format input)
  (:documentation "Parse an input into a CommonDoc document."))

(defgeneric emit-document (format document stream)
  (:documentation "Dump a CommonDoc document into a stream."))

(defmethod emit-to-string ((format <format>) (document <document>))
  (with-output-to-string (stream)
    (emit-document format document stream)))
