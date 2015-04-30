(defsystem common-doc-gnuplot
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :depends-on (:common-doc
               :split-sequence)
  :components ((:module "contrib"
                :components
                ((:module "gnuplot"
                  :components
                  ((:file "gnuplot"))))))
  :description "Render gnuplot plots."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "contrib/gnuplot/README.md")))
