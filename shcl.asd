(defsystem "shcl/posix-types"
  :defsystem-depends-on ("cffi-grovel")
  :components ((:file "posix-types")
               (:cffi-grovel-file "posix-types-grovel" :depends-on ("posix-types"))))

(defsystem "shcl"
  :class :package-inferred-system
  :defsystem-depends-on ("cffi-grovel")
  :description "Shcl, a lisp shell"
  :version "0.0.1"
  :author "Brad Jensen <brad@bradjensen.net>"
  :licence "All rights reserved."
  :depends-on ("shcl/main"))

(defsystem "shcl-test"
  :description "Shcl tests, tests for a lisp shell"
  :version "0.0.1"
  :author "Brad Jensen <brad@bradjensen.net>"
  :licence "All rights reserved."
  :depends-on ("prove")
  :components ((:file "test/lexer")
               (:file "test/utility")
               (:file "test/environment")
               (:file "test/posix")
               (:file "test/main")))
