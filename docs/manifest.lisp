(:docstring-markup-format :scriba
 :systems (:common-doc
           :common-doc-gnuplot
           :common-doc-include
           :common-doc-split-paragraphs
           :common-doc-tex)
 :documents ((:title "CommonDoc"
              :authors ("Fernando Borretti")
              :output-format (:type :multi-html
                              :template :minima)
              :sources ("overview.scr"
                        "formats.scr"
                        "nodes.scr"
                        "defining-nodes.scr"
                        "operations.scr"
                        "files.scr"
                        "errors.scr"
                        ;"macros.scr"
                        "libraries.scr"
                        "extensions.scr"
                        "future-work.scr"))))
