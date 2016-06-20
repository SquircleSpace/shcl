(defsystem "shcl"
  :description "Shcl, a lisp shell"
  :version "0.0.1"
  :author "Brad Jensen <brad@bradjensen.net>"
  :licence "All rights reserved."
  :depends-on ("yacc")
  :components ((:file "packages")
               (:file "utility" :depends-on ("packages"))
               (:file "lexer" :depends-on ("packages" "utility"))
               (:file "yacc-parser" :depends-on ("packages" "utility" "lexer"))
               (:file "main" :depends-on ("packages"))))
