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
  (let ((seen-values (make-hash-table :test 'equal)))
    (filter-iterator
     iter
     (lambda (obj)
       (unless (gethash obj seen-values)
         (setf (gethash obj seen-values) t)
         t)))))

(defgeneric parse-error-involves-sigil-token-p (err sigil-token))

(defmethod parse-error-involves-sigil-token-p (err sigil-token)
  nil)

(defgeneric parse-error-expected-types (err))

(defmethod parse-error-involves-sigil-token-p ((err shcl/core/parser:expected-eof) sigil-token)
  (eq sigil-token (shcl/core/parser:expected-eof-got err)))

(defmethod parse-error-expected-types ((err shcl/core/parser:expected-eof))
  (list :eof))

(defmethod parse-error-involves-sigil-token-p ((err shcl/core/parser:type-mismatch) sigil-token)
  (eq sigil-token (shcl/core/parser:type-mismatch-got err)))

(defmethod parse-error-expected-types ((err shcl/core/parser:type-mismatch))
  (list (shcl/core/parser:type-mismatch-expected-type err)))

(defmethod parse-error-expected-types ((err shcl/core/parser:choice))
  (concatmap-iterator
   (shcl/core/parser:choice-errors-iterator err :recursive-p t)
   'parse-error-expected-types))

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

(defclass completion-context ()
  ((cursor-point
    :reader cursor-point
    :initarg :cursor-point
    :initform (required))))

(defgeneric completion-suggestions (desired-token-type token-fragment context)
  (:method-combination concatenate-iterables))

(defmethod completion-suggestions concatenate-iterables
    (desired-token-type token-fragment context)
  (declare (ignore desired-token-type token-fragment context))
  *empty-iterator*)

(defmethod completion-suggestions concatenate-iterables
    ((desired shcl/core/lexer:literal-token-class) token context)
  (let ((desired-string (shcl/core/lexer:literal-token-string desired))
        (token-value (shcl/core/lexer:token-value token)))
    (if (and (<= (length token-value) (length desired-string))
             (string= desired-string token-value
                      :end1 (min (length token-value) (length desired-string))))
        (list-iterator (list desired-string))
        *empty-iterator*)))

(shcl/core/data:define-data empty-token ()
  ())

(defmethod shcl/core/lexer:token-value ((empty-token empty-token))
  "")

(defvar *empty-token* (make-instance 'empty-token))

(defgeneric expand-into-subclass-p (superclass subclass))

(defmethod expand-into-subclass-p (superclass subclass)
  t)

(defmethod expand-into-subclass-p ((superclass (eql (find-class 'shcl/core/lexer:a-word)))
                                   (subclass (eql (find-class 'shcl/core/lexer:reserved-word))))
  nil)

(defun make-unique-table ()
  (make-hash-table :test 'equal))

(defun expand-type (type &key (unique-table (make-unique-table)))
  (when (gethash type unique-table)
    (return-from expand-type *empty-iterator*))

  (let ((class (typecase type
                 (symbol
                  (find-class type nil))
                 (standard-class
                  type))))
    (unless class
      (setf (gethash type unique-table) t)
      (return-from expand-type
        (list-iterator (list type))))

    (let ((seen-class (gethash class unique-table)))
      (setf (gethash type unique-table) t)
      (when seen-class
        (return-from expand-type
          *empty-iterator*)))

    (setf (gethash class unique-table) t)

    (concatenate-iterables
     (list class)
     (concatenate-iterable-collection
      (map-iterator
       (filter-iterator (iterator (closer-mop:class-direct-subclasses class))
                        (lambda (subclass)
                          (expand-into-subclass-p class subclass)))
       (lambda (subclass) (expand-type subclass :unique-table unique-table)))))))

(defclass sigil-token ()
  ())

(defmethod shcl/core/lexer:token-value ((sigil sigil-token))
  nil)

(defun completion-relevant-parse-errors-for-leading-tokens (leading-tokens)
  (let* ((sigil-token (make-instance 'sigil-token))
         (command-iterator (shcl/core/shell-grammar:command-iterator
                            (lookahead-iterator-wrapper
                             (concatenate-iterables
                              leading-tokens
                              (list sigil-token)))))
         (shcl/core/shell-grammar:*intermediate-parse-error-hook*
          shcl/core/shell-grammar:*intermediate-parse-error-hook*)
         (all-errors (make-extensible-vector))
         (seen-errors (make-hash-table :test 'eq)))
    (labels
        ((add-error (err)
           (when (and (parse-error-involves-sigil-token-p err sigil-token)
                      (not (gethash err seen-errors)))
             (vector-push-extend err all-errors)
             (setf (gethash err seen-errors) t))))
      (add-hook
       'shcl/core/shell-grammar:*intermediate-parse-error-hook*
       #'add-error)
      (handler-case
          (do-iterator (command command-iterator)
            (declare (ignore command)))
        (shcl/core/parser:parse-failure (err)
          (add-error (shcl/core/parser:parse-failure-error-object err))))
      all-errors)))

(defun valid-types-for-next-token (leading-tokens)
  (let* ((all-errors (completion-relevant-parse-errors-for-leading-tokens leading-tokens))
         (error (make-instance 'shcl/core/parser:choice :errors all-errors)))
    (parse-error-expected-types error)))

(defun completion-suggestions-for-tokens (leading-tokens token-to-complete context)
  (let* ((unique-table (make-unique-table))
         (raw-type-iter (valid-types-for-next-token leading-tokens))
         (type-iter (concatmap-iterator raw-type-iter
                                        (lambda (type)
                                          (expand-type type :unique-table unique-table)))))
    (concatmap-iterator type-iter
                        (lambda (type)
                          (completion-suggestions type token-to-complete context)))))

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
    (completion-suggestions-for-tokens
     tokens
     (if end-found (vector-pop tokens) *empty-token*)
     (make-instance 'completion-context :cursor-point cursor-point))))
