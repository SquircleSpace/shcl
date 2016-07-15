(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cffi-grovel))

(defsystem "shcl"
  :description "Shcl, a lisp shell"
  :version "0.0.1"
  :author "Brad Jensen <brad@bradjensen.net>"
  :licence "All rights reserved."
  :depends-on ("alexandria" "trivial-garbage" "cl-fad" "cffi" "cffi-grovel")
  :components ((:file "packages")
               (:file "utility" :depends-on ("packages"))
               (:file "lexer" :depends-on ("packages" "utility"))
               (:file "parser" :depends-on ("packages" "utility" "lexer"))
               (:file "shell-grammar" :depends-on ("packages" "utility" "parser" "lexer"))
               (:cffi-grovel-file "posix-types")
               (:file "posix" :depends-on ("packages" "posix-types"))
               (:file "fork-exec" :depends-on ("packages" "utility" "shell-grammar" "posix"))
               (:file "evaluate" :depends-on ("packages" "utility" "parser" "lexer" "fork-exec"))
               (:file "main" :depends-on ("packages" "evaluate" "shell-grammar" "lexer" "utility"))))
