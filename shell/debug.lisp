;; Copyright 2018 Bradley Jensen
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

(defpackage :shcl/shell/debug
  (:use :common-lisp :shcl/core/utility)
  (:import-from :shcl/core/command #:define-builtin)
  (:import-from :shcl/core/debug
   #:undocumented-symbols #:undocumented-symbols-in-package)
  (:import-from :fset))
(in-package :shcl/shell/debug)

(define-builtin -shcl-undocumented-symbols (&option (package "-p"))
  (let (syms)
    (cond
      ((zerop (length package))
       (setf syms (undocumented-symbols)))
      (t
       (setf syms (fset:empty-set))
       (loop :for package-name :across package :do
          (undocumented-symbols-in-package (find-package package-name)))))
    (fset:do-set (sym syms)
       (format t "~A:~A~%" (package-name (symbol-package sym))
               (symbol-name sym)))
    0))
