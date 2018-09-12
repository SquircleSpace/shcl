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

(defgeneric expand-type (type unique-table))

(defmethod expand-type :around (type unique-table)
  (when (unique-table-contains-p unique-table type)
    (return-from expand-type nil))
  (unique-table-insert unique-table type)
  (call-next-method))

(defmethod expand-type (type unique-table)
  (declare (ignore unique-table))
  (list type))

(defgeneric expand-compound-type (type-car type unique-table))

(defmethod expand-compound-type (type-car type unique-table)
  (declare (ignore type-car unique-table))
  (list type))

(defvar *collect-tab-complete-info* nil)

(defvar *command-words* nil)

(shcl/core/advice:define-advice shcl/core/shell-grammar:parse-simple-command
    :around tab-complete
    (iter)
  (declare (ignore iter))
  (unless *collect-tab-complete-info*
    (return-from shcl/core/shell-grammar:parse-simple-command
      (call-next-method)))

  (let ((*command-words* (make-extensible-vector)))
    (call-next-method)))

(deftype command-word (real-type)
  real-type)

(defmethod expand-compound-type ((type-car (eql 'command-word)) type unique-table)
  (let ((class (find-class (second type))))
    (unless class
      (warn "Unexpected command word type: ~A" type)
      (return-from expand-compound-type
        (expand-type (second type) unique-table)))

    (concatenate-iterables
     (list class)
     (concatmap-iterator
      (closer-mop:class-direct-subclasses class)
      (lambda (subclass)
        (unless (eq subclass (find-class 'shcl/core/lexer:reserved-word))
          (expand-type subclass unique-table)))))))

(defgeneric wrap-expected-type (error-object))

(defmethod wrap-expected-type ((err shcl/core/parser:unexpected-eof))
  (shcl/core/data:clone err :expected-type `(command-word ,(shcl/core/parser:unexpected-eof-expected-type err))))

(defmethod wrap-expected-type ((err shcl/core/parser:type-mismatch))
  (shcl/core/data:clone err :expected-type `(command-word ,(shcl/core/parser:type-mismatch-expected-type err))))

(shcl/core/advice:define-advice shcl/core/shell-grammar:parse-simple-command-word
    :around tab-complete
    (iter)
  (declare (ignore iter))
  (unless *collect-tab-complete-info*
    (return-from shcl/core/shell-grammar:parse-simple-command-word
      (call-next-method)))

  (assert *command-words*)
  (shcl/core/parser:parser-bind (value error-p) (call-next-method)
    (cond
      (error-p
       (shcl/core/parser:parser-error (wrap-expected-type value)))
      (t
       (vector-push-extend value *command-words*)
       (shcl/core/parser:parser-value value)))))

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

(defun make-unique-table ()
  (make-hash-table :test 'equal))

(defun unique-table-contains-p (unique-table value)
  (nth-value 1 (gethash value unique-table)))

(defun unique-table-insert (unique-table value)
  (setf (gethash value unique-table) t)
  value)

(defmethod expand-type ((type cons) unique-table)
  (expand-compound-type (car type) type unique-table))

(defmethod expand-type ((type symbol) unique-table)
  (let ((class (find-class type nil)))
    (if class
        (expand-type class unique-table)
        (call-next-method))))

(defmethod expand-type ((type standard-class) unique-table)
  (concatenate-iterables
   (list type)
   (concatmap-iterator
    (closer-mop:class-direct-subclasses type)
    (lambda (subclass)
      (expand-type subclass unique-table)))))

(defmethod expand-compound-type ((type-car (eql 'or)) type-cdr unique-table)
  (concatmap-iterator type-cdr (lambda (type) (expand-type type unique-table))))

(defclass sigil-token ()
  ())

(defmethod shcl/core/lexer:token-value ((sigil sigil-token))
  nil)

(defun completion-relevant-parse-errors-for-leading-tokens (leading-tokens)
  (let* ((*collect-tab-complete-info* t)
         (sigil-token (make-instance 'sigil-token))
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
  (let ((all-errors (completion-relevant-parse-errors-for-leading-tokens leading-tokens)))
    (concatmap-iterator all-errors 'parse-error-expected-types)))

(defun completion-suggestions-for-tokens (leading-tokens token-to-complete context)
  (let* ((unique-table (make-unique-table))
         (raw-type-iter (valid-types-for-next-token leading-tokens))
         (type-iter (concatmap-iterator raw-type-iter
                                        (lambda (type)
                                          (expand-type type unique-table)))))
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
