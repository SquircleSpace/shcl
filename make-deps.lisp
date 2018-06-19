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

(defpackage :shcl/make-deps
  (:use :common-lisp))
(in-package :shcl/make-deps)

(when (equal "1" (uiop:getenv "SHCL_DEBUG"))
  (push :shcl-debug *features*))

(let ((asdf:*central-registry* (cons (directory-namestring *load-truename*) asdf:*central-registry*)))
  (if (find-package :ql)
      (funcall (intern "QUICKLOAD" :ql) :shcl/deps)
      (asdf:load-system :shcl/deps))
  (funcall (intern "MAIN" :shcl/deps)))
