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

(defpackage :shcl/shell/complete
  (:use :common-lisp :shcl/core/utility :shcl/core/iterator)
  ;;  (:nicknames :shcl/shell/sisyphus)
  (:export #:completion-suggestions-for-input))
(in-package :shcl/shell/complete)

(optimization-settings)

(defvar *empty-iterator*
  (make-iterator ()
    (stop)))

(defun iterator-without-duplicates (iter)
  (let ((seen-values (fset:empty-set)))
    (filter-iterator
     iter
     (lambda (obj)
       (unless (fset:contains? seen-values obj)
         (fset:adjoinf seen-values obj)
         t)))))

(defun expected-types-for-parse-error (err sigil-token)
  (labels
      ((expected-type (error)
         (cond
           ((and (typep error 'shcl/core/parser:expected-eof)
                 (eq sigil-token
                     (shcl/core/parser:expected-eof-got error)))
            (values t :eof))

           ((and (typep error 'shcl/core/parser:type-mismatch)
                 (eq sigil-token
                     (shcl/core/parser:type-mismatch-got error)))
            (values t (shcl/core/parser:type-mismatch-expected-type error)))

           (t
            (values nil)))))
    (typecase err
      (shcl/core/parser:choice
       (map-iterator
        (filter-iterator
         (shcl/core/parser:choice-errors-iterator err :recursive-p t)
         #'expected-type)
        (lambda (e)
          (nth-value 1 (expected-type e)))))

      (t
       (multiple-value-bind (valid-p type) (expected-type err)
         (cond
           (valid-p
            (list-iterator (list type)))
           (t
            (warn "Unexpected parse error ~A" err)
            *empty-iterator*)))))))

(defgeneric simple-expansions-for-type (type))

(defmethod simple-expansions-for-type ((class closer-mop:standard-class))
  *empty-iterator*)

(defmethod simple-expansions-for-type ((class shcl/core/lexer:literal-token-class))
  (list-iterator (list (shcl/core/lexer:literal-token-string class))))

(defmethod simple-expansions-for-type ((type symbol))
  (let ((class (find-class type nil)))
    (if class
        (simple-expansions-for-type class)
        *empty-iterator*)))

(defclass sigil-token ()
  ())

(defmethod shcl/core/lexer:token-value ((sigil sigil-token))
  nil)

(defun valid-types-for-next-token (leading-tokens)
  (let* ((sigil-token (make-instance 'sigil-token))
         (command-iterator (shcl/core/shell-grammar:command-iterator
                            (lookahead-iterator-wrapper
                             (concatenate-iterators*
                              (iterator leading-tokens)
                              (list-iterator (list sigil-token))))))
         (shcl/core/shell-grammar:*intermediate-parse-error-hook*
          shcl/core/shell-grammar:*intermediate-parse-error-hook*)
         (all-errors (make-extensible-vector))
         error)
    (add-hook
     'shcl/core/shell-grammar:*intermediate-parse-error-hook*
     (lambda (err) (vector-push-extend err all-errors)))
    (handler-case
        (do-iterator (command command-iterator)
          (declare (ignore command)))
      (shcl/core/parser:parse-failure (err)
        (setf error (shcl/core/parser:parse-failure-error-object err))))
    (when error
      (vector-push-extend error all-errors))
    (setf error (make-instance 'shcl/core/parser:choice :errors all-errors))
    (iterator-without-duplicates
     (expected-types-for-parse-error error sigil-token))))

(defun completion-suggestions-for-start-of-new-token-with-valid-token-types (valid-token-types)
  (iterator-without-duplicates
   (concatenate-iterators
    (map-iterator valid-token-types 'simple-expansions-for-type))))

(defun completion-suggestions-for-start-of-new-token (leading-tokens)
  (completion-suggestions-for-start-of-new-token-with-valid-token-types
   (valid-types-for-next-token leading-tokens)))

(defun completion-suggestions-for-tokens (leading-tokens token-to-complete cursor-point)
  (declare (ignore cursor-point))
  (let* ((token-to-complete-value (shcl/core/lexer:token-value token-to-complete))
         (valid-next-token-types (valid-types-for-next-token leading-tokens))
         (new-token-suggestions
          (completion-suggestions-for-start-of-new-token-with-valid-token-types
           valid-next-token-types))
         (applicable-new-token-suggestions
          (filter-iterator
           new-token-suggestions
           (lambda (suggestion)
             (and
              (<= (length token-to-complete-value) (length suggestion))
              (string= suggestion token-to-complete-value
                       :end1 (min (length suggestion)
                                  (length token-to-complete-value))))))))
    applicable-new-token-suggestions))

(defun completion-suggestions-for-input (input-text cursor-point readtable)
  "Compute possible completions.

`input-text' is the text the user is asking for completion suggestions on.

`cursor-point' is a number describing where the cursor is located.  0
indicated that the cursor will insert new text before the first
character.  If `cursor-point' is equal to the length of `input-text'
then new text will be inserted after the last character.

`readtable' is the readtable that should be used when lexing the input text.

This function returns an iterator of strings.  Each string represents
text that could replace the token under point."
  (let ((token-iterator (shcl/core/lexer:token-iterator
                         (make-instance 'shcl/core/positional-stream:positional-input-stream
                                        :underlying-stream (make-string-input-stream input-text))
                         :readtable readtable))
        (tokens (make-extensible-vector))
        end-found)
    (do-iterator (token token-iterator)
      (let* ((token-start (shcl/core/positional-stream:position-record-offset
                           (shcl/core/lexer:token-position token)))
             (token-end (+ token-start (length (shcl/core/lexer:token-value token)))))
        (when (<= token-start cursor-point)
          (vector-push-extend token tokens))
        (when (>= token-end cursor-point)
          (setf end-found t)
          (return))))
    (if end-found
        (completion-suggestions-for-tokens tokens (vector-pop tokens) cursor-point)
        (completion-suggestions-for-start-of-new-token tokens))))
