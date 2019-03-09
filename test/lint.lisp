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
   :shcl/test/foundation :prove)
  (:import-from :shcl/core/sequence
   #:lazy-filter #:eager-flatmap-sequence #:empty-p #:do-while-popf))
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

(defun symbol-exported-p (sym)
  (eq :external (nth-value 1 (find-symbol (symbol-name sym) (symbol-package sym)))))

(defun package-imported-symbols (package)
  (unless (typep package 'package)
    (setf package (find-package package)))
  (let ((result (fset:empty-set)))
    (do-symbols (sym package)
      (when (and (not (eq (symbol-package sym) package))
                 (not (symbol-exported-p sym)))
        (fset:adjoinf result (cons sym package))))
    ;; Some packages (e.g. Prove circa 2019) re-export symbols from an
    ;; internal package that aren't exported in their home package
    ;; (e.g. prove:reset-suite).  Luckily, we're only bringing in
    ;; those tricky exported-but-apparently-not symbols via :use
    ;; clauses of defpackage, so its easy to prune them out.  Anything
    ;; exported by a package we use is a-ok... even if that package is
    ;; sneakily importing the symbols from elsewhere.
    (dolist (used-package (package-use-list package))
      (do-external-symbols (used-symbol used-package)
        (setf result (fset:less result (cons used-symbol package)))))
    result))

(define-test no-unexported-imports
  (let* ((shcl-packages (lazy-filter (list-all-packages) (lambda (package) (shcl-package-name-p (package-name package)))))
         (symbols (eager-flatmap-sequence shcl-packages 'package-imported-symbols (fset:empty-set))))
    (if (empty-p symbols)
        (pass "Awesome!  No sneaky import-froms of unexported symbols")
        (fail (with-output-to-string (out)
                (format out "Oh no!  Found sneaky imports:~%")
                (do-while-popf (sym-record symbols)
                  (destructuring-bind (sym . package) sym-record
                    (format out "~A imports ~A::~A~%"
                            (package-name package)
                            (package-name (symbol-package sym))
                            (symbol-name sym)))))))))
