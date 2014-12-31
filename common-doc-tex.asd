(defsystem common-doc-tex
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:common-doc)
  :components ((:module "contrib"
                :components
                ((:module "tex"
                  :components
                  ((:file "tex"))))))
  :description "TeX math macros for CommonDoc."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "contrib/tex/README.md")))
