(defsystem "shcl"
  :description "Shcl, a lisp shell"
  :version "0.0.1"
  :author "Brad Jensen <brad@bradjensen.net>"
  :licence "All rights reserved."
  :depends-on ("alexandria" "trivial-garbage" "cl-fad" "cffi")
  :components ((:file "packages")
               (:file "utility" :depends-on ("packages"))
               (:file "lexer" :depends-on ("packages" "utility"))
               (:file "parser" :depends-on ("packages" "utility" "lexer"))
               (:file "shell-grammar" :depends-on ("packages" "utility" "parser" "lexer"))
               (:file "fork-exec" :depends-on ("packages" "utility" "shell-grammar"))
               (:file "evaluate" :depends-on ("packages" "utility" "parser" "lexer" "fork-exec"))
               (:file "main" :depends-on ("packages" "evaluate" "shell-grammar" "lexer" "utility"))))
