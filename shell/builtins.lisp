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

(defpackage :shcl/shell/builtins
  (:use :common-lisp :shcl/core/utility :shcl/core/command)
  (:import-from :fset))
(in-package :shcl/shell/builtins)

(define-builtin -shcl-dump-logs ()
  "Dump available logs to stdout."
  (dump-logs)
  0)

(define-builtin -shcl-list-commands ()
  "List all the commands known in the current command namespace."
  (fset:do-map (name map (command-namespace-table *command-namespace*))
    (when (and (not (fset:empty? map))
               (stringp name))
      (format t "~A~%" name)))
  0)
