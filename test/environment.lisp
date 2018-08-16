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

(defpackage :shcl/test/environment
  (:use :common-lisp :prove :shcl/core/utility :shcl/core/environment
        :shcl/test/foundation))
(in-package :shcl/test/environment)

(optimization-settings)

(link-package-to-system :shcl/core/environment)

(defun contains-p (key)
  (nth-value 1 (env key)))

(define-test basics
  (let ((*environment* *environment*))
    (clear-environment)
    (ok (not (contains-p "foo")))

    (setf (env "foo") "bar")
    (ok (equal (env "foo") "bar"))
    (ok (not (env-exported-p "foo")))

    (setf (env-exported-p "foo") t)
    (ok (env-exported-p "foo"))

    (setf (env-exported-p "foo") nil)
    (ok (not (env-exported-p "foo")))

    (setf (env "foo") nil)
    (ok (not (contains-p "foo")))

    (setf (env "not_exported") "value")
    (with-environment-scope ()
      (setf (env "bar") "baz")
      (ok (equal (env "bar") "baz"))
      (export-variable "not_exported"))
    (ok (not (contains-p "bar")))
    (ok (not (env-exported-p "not_exported")))

    (setf $path "path")
    (ok (equal $path "path"))))
