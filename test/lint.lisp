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

(defpackage :shcl/test/lint
  (:use :common-lisp :shcl/core/utility :shcl/core/debug
        :shcl/test/foundation :prove))
(in-package :shcl/test/lint)

(optimization-settings)

(define-test no-undocumented-symbols
  (let ((symbols (undocumented-symbols)))
    (if (equal 0 (fset:size symbols))
        (pass "Great!  No undocumented symbols found!")
        (fail (with-output-to-string (out)
                (format out "Found undocumented symbols:")
                (let ((*print-gensym* nil))
                  (fset:do-set (sym symbols)
                    (multiple-value-bind (our-sym found-p) (find-symbol (symbol-name sym))
                      (if (and found-p (eq our-sym sym))
                          (format out " ~S:~S"
                                  ;; Use symbol printer so that we get
                                  ;; the automatic quoting behavior
                                  (make-symbol (package-name (symbol-package sym)))
                                  sym)
                          (format out " ~S" sym))))))))))
