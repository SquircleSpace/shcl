(defsystem "shcl"
  :description "Shcl, a lisp shell"
  :version "0.0.1"
  :author "Brad Jensen <brad@bradjensen.net>"
  :licence "All rights reserved."
  :depends-on ("alexandria" "trivial-garbage" "cl-fad" "cffi" "cffi-grovel" "bordeaux-threads" "fset" "cl-unicode" "closer-mop" "cl-cli" "trivial-gray-streams")
  :defsystem-depends-on ("cffi-grovel")
  :components ((:file "packages")
               (:file "utility" :depends-on ("packages"))
               (:file "thread" :depends-on ("packages" "utility"))
               (:file "lexer" :depends-on ("packages" "utility"))
               (:file "parser" :depends-on ("packages" "utility" "lexer"))
               (:file "shell-grammar" :depends-on ("packages" "utility" "parser" "lexer"))
               (:cffi-grovel-file "posix-types" :depends-on ("packages"))
               (:file "posix" :depends-on ("packages" "utility" "posix-types"))
               (:file "fork-exec" :depends-on ("packages" "utility" "shell-grammar" "posix"))
               (:file "environment" :depends-on ("packages" "utility" "posix"))
               (:file "expand" :depends-on ("packages" "utility" "lexer" "environment"))
               (:file "baking" :depends-on ("packages" "utility" "thread"))
               (:file "builtin" :depends-on ("packages" "utility"))
               (:file "evaluate" :depends-on ("packages" "utility" "parser" "lexer" "fork-exec" "thread" "environment" "builtin" "posix"))
               (:file "lisp-interpolation" :depends-on ("packages" "utility" "lexer" "shell-grammar" "evaluate" "expand" "baking"))
               (:file "main" :depends-on ("packages" "evaluate" "shell-grammar" "lexer" "baking" "lisp-interpolation" "utility"))))

(defsystem "shcl-test"
  :description "Shcl tests, tests for a lisp shell"
  :version "0.0.1"
  :author "Brad Jensen <brad@bradjensen.net>"
  :licence "All rights reserved."
  :depends-on ("prove")
  :components ((:file "test/lexer")
               (:file "test/utility")
               (:file "test/environment")
               (:file "test/posix")))
