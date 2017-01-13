(defsystem "shcl/core/posix-types"
  :defsystem-depends-on ("cffi-grovel")
  :components ((:file "core/posix-types")
               (:cffi-grovel-file "core/posix-types-grovel" :depends-on ("core/posix-types"))))

(defsystem "shcl/shell/prompt-types"
  :defsystem-depends-on ("cffi-grovel")
  :components ((:file "shell/prompt-types")
               (:cffi-grovel-file "shell/prompt-types-grovel" :depends-on ("shell/prompt-types"))))

(defsystem "shcl"
  :class :package-inferred-system
  :defsystem-depends-on ("cffi-grovel")
  :description "Shcl, a lisp shell"
  :version "0.0.1"
  :author "Brad Jensen <brad@bradjensen.net>"
  :licence "Modified BSD License"
  :depends-on ("shcl/shell/main"))
