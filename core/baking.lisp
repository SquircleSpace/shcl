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

(defpackage :shcl/core/baking
  (:use
   :common-lisp :shcl/core/utility :shcl/core/lexer
   :shcl/core/iterator)
  (:import-from :shcl/core/parser #:syntax-tree-parts #:syntax-tree)
  (:export #:bake-form))
(in-package :shcl/core/baking)

(optimization-settings)

(defgeneric bake-form (object))

(defmethod bake-form (object)
  (declare (ignore object))
  nil)

(defun bake-form-for-parts (parts)
  (let ((result (make-extensible-vector)))
    (loop :for part :across parts :do
       (let ((expansion (bake-form part)))
         (when expansion
           (vector-push-extend expansion result))))
    (unless (zerop (length result))
      `(progn
         ,@(coerce result 'list)))))

(defmethod bake-form ((token compound-word))
  (bake-form-for-parts (compound-word-parts token)))

(defmethod bake-form ((token double-quote))
  (bake-form-for-parts (double-quote-parts token)))

(defmethod bake-form ((syntax-tree syntax-tree))
  (let (forms)
    (loop :for subtree :across (syntax-tree-parts syntax-tree) :do
       (let* ((raw-subform (bake-form subtree)))
         (cond
           ;; De-nest progns for prettier output
           ((eq 'progn (car raw-subform))
            (dolist (form (cdr raw-subform))
              (push form forms)))
           (raw-subform
            (push raw-subform forms)))))
    (apply 'progn-concatenate forms)))
