(defsystem common-doc-plantuml
  :author "Willem Rein Oudshoorn <woudshoo@xs4all.nl>"
  :maintainer "Willem Rein Oudshoorn <woudshoo@xs4all.nl>"
  :license "MIT"
  :version "0.1"
  :depends-on (:common-doc
               :trivial-shell)
  :components ((:module "contrib"
                :components
                ((:module "plantuml"
                  :components
                  ((:file "plantuml"))))))
  :description "PlantUML macro for CommonDoc."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "contrib/plantuml/README.md")))
