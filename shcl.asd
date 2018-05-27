;; Copyright 2017 Bradley Jensen
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(defsystem "shcl/core/posix-types/grovel"
  :defsystem-depends-on ("cffi-grovel")
  :components ((:file "core/posix-types")
               (:cffi-grovel-file "core/posix-types-grovel" :depends-on ("core/posix-types"))))

(register-system-packages "shcl/core/posix-types/grovel" '(:shcl/core/posix-types))

(defsystem "shcl/shell/prompt-types/grovel"
  :defsystem-depends-on ("cffi-grovel")
  :components ((:file "shell/prompt-types")
               (:cffi-grovel-file "shell/prompt-types-grovel" :depends-on ("shell/prompt-types"))))

(register-system-packages "shcl/shell/prompt-types/grovel" '(:shcl/shell/prompt-types))

(defsystem "shcl"
  :class :package-inferred-system
  :defsystem-depends-on ("cffi-grovel")
  :description "Shcl, a lisp shell"
  :version "0.0.1"
  :author "Brad Jensen <brad@bradjensen.net>"
  :licence "Modified BSD License"
  :depends-on ("shcl/shell/main")
  :entry-point "shcl/shell/main::main"
  :output-files (program-op (o c) (values (list (make-pathname :name "shcl" :type nil :defaults *load-truename*)) t)))

(register-system-packages "osicat" '(:osicat-posix))
