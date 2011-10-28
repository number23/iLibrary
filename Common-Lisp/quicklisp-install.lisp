(load "http://beta.quicklisp.org/quicklisp.lisp")
(quicklisp-quickstart:install)

(ql:quickload '(:quicklisp-slime-helper
                :linedit
                :clsql
                :restas-doc
                :weblocks))

(ql:add-to-init-file)
