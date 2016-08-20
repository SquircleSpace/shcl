(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :cffi-grovel))

(defsystem "shcl"
  :description "Shcl, a lisp shell"
  :version "0.0.1"
  :author "Brad Jensen <brad@bradjensen.net>"
  :licence "All rights reserved."
  :depends-on ("alexandria" "trivial-garbage" "cl-fad" "cffi" "cffi-grovel" "bordeaux-threads" "fset" "cl-unicode" "closer-mop")
  :components ((:file "packages")
               (:file "utility" :depends-on ("packages"))
               (:file "thread" :depends-on ("packages" "utility"))
               (:file "lexer" :depends-on ("packages" "utility"))
               (:file "parser" :depends-on ("packages" "utility" "lexer"))
               (:file "shell-grammar" :depends-on ("packages" "utility" "parser" "lexer"))
               (:cffi-grovel-file "posix-types")
               (:file "posix" :depends-on ("packages" "posix-types"))
               (:file "fork-exec" :depends-on ("packages" "utility" "shell-grammar" "posix"))
               (:file "environment" :depends-on ("packages" "utility" "posix"))
               (:file "expand" :depends-on ("packages" "utility" "lexer" "environment"))
               (:file "baking" :depends-on ("packages" "utility" "thread"))
               (:file "evaluate" :depends-on ("packages" "utility" "parser" "lexer" "fork-exec" "thread" "environment"))
               (:file "lisp-interpolation" :depends-on ("packages" "utility" "lexer" "shell-grammar" "evaluate" "expand" "baking"))
               (:file "main" :depends-on ("packages" "evaluate" "shell-grammar" "lexer" "baking" "lisp-interpolation" "utility"))))

(defsystem "shcl-test"
  :description "Shcl tests, tests for a lisp shell"
  :version "0.0.1"
  :author "Brad Jensen <brad@bradjensen.net>"
  :licence "All rights reserved."
  :depends-on ("fiveam")
  :components ((:file "test/packages")
               (:file "test/suites" :depends-on ("test/packages"))
               (:file "test/lexer" :depends-on ("test/packages" "test/suites"))))
