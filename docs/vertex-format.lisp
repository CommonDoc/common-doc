(defclass vertex (document-format)
  ()
  (:documentation "The VerTeX format."))

(defmethod parse-document ((vertex vertex)
                           (string string))
  "Return a VerTeX document parsed from a string."
  (vertex.parser:parse-string string))

(defmethod parse-document ((vertex vertex)
                           (pathname pathname))
  "Return a VerTeX document parsed from a file."
  (vertex.parser:parse-file pathname))

(defmethod emit-document ((vertex vertex)
                          (node common-doc:document-node)
                          stream)
  (vertex.emitter:emit-to-stream node stream))

(defmethod emit-document ((vertex vertex)
                          (doc common-doc:document)
                          stream)
  (vertex.emitter:emit-to-stream doc stream))
