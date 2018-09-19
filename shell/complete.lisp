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

(defun type-expander (&optional (unique-table (make-unique-table)))
  (lambda (type)
    (expand-type type unique-table)))

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

  (let ((*command-words* (fset:empty-seq)))
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
       (fset:push-last *command-words* value)
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
    (if (starts-with-p desired-string token-value)
        (list-iterator (list desired-string))
        *empty-iterator*)))

(defun directory-p (at-fd path)
  (handler-case
      (shcl/core/fd-table:receive-ref-counted-fd
          (file (shcl/core/fd-table:retained-fd-openat at-fd path shcl/core/posix-types:o-rdonly))
        (shcl/core/support:s-isdir (slot-value (shcl/core/posix:fstat (shcl/core/fd-table:fd-wrapper-value file)) 'shcl/core/posix-types:st-mode)))
    (shcl/core/posix:syscall-error ()
      nil)))

(defun executable-p (at-fd path)
  (handler-case
      (progn
        (shcl/core/posix:faccessat (shcl/core/fd-table:fd-wrapper-value at-fd) path shcl/core/posix-types:x-ok shcl/core/posix-types:at-eaccess)
        t)
    (shcl/core/posix:syscall-error ()
      nil)))

(defmacro do-executables-in-dir-fd ((executable-name dir-fd &optional result) &body body)
  (let ((dir (gensym "DIR"))
        (dir-ptr (gensym "DIR-PTR"))
        (file-name (gensym "FILE-NAME")))
    `(let ((,dir ,dir-fd))
       (shcl/core/fd-table:with-dir-ptr-for-fd (,dir-ptr ,dir)
         (shcl/core/posix:do-directory-contents (,file-name ,dir-ptr ,result)
           (when (and (not (equal "." ,file-name))
                      (not (equal ".." ,file-name))
                      (not (directory-p ,dir ,file-name))
                      (executable-p ,dir ,file-name))
             (let ((,executable-name ,file-name))
               ,@body)))))))

(defun executables-in-directory (path)
  (let ((result (make-extensible-vector)))
    (labels
        ((retained-fd-open-dir ()
           (handler-case
               (shcl/core/fd-table:retained-fd-openat
                (shcl/core/working-directory:get-fd-current-working-directory)
                path shcl/core/posix-types:o-rdonly)
             (shcl/core/posix:syscall-error ()
               (return-from executables-in-directory result)))))
      (shcl/core/fd-table:receive-ref-counted-fd
          (dir-fd (retained-fd-open-dir))
        (do-executables-in-dir-fd (executable-name dir-fd)
          (vector-push-extend executable-name result))))
    result))

(defun all-binary-commands ()
  (let ((result-vector (make-extensible-vector)))
    (do-iterator (path (shcl/core/environment:colon-list-iterator shcl/core/environment:$path))
      (when (equal "" path)
        (setf path "."))
      (vector-push-extend (executables-in-directory path) result-vector))
    (concatenate-iterable-collection result-vector)))

(defmethod completion-suggestions concatenate-iterables
    ((desired (eql (find-class 'shcl/core/lexer:simple-word)))
     (token shcl/core/lexer:simple-word)
     context)
  (unless *command-words*
    (return-from completion-suggestions))

  (when (equal 0 (fset:size *command-words*))
    (labels
        ((compatible-p (command)
           (starts-with-p command (shcl/core/lexer:simple-word-text token))))
      (filter-iterator (all-binary-commands) #'compatible-p))))

(defvar *empty-token* (make-instance 'shcl/core/lexer:simple-word :text ""))

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

(defun completion-suggestions-for-tokens (leading-tokens token-to-complete context)
  (let* ((*collect-tab-complete-info* t)
         (sigil-token (make-instance 'sigil-token))
         (command-iterator (shcl/core/shell-grammar:command-iterator
                            (lookahead-iterator-wrapper
                             (concatenate-iterables
                              leading-tokens
                              (list sigil-token)))))
         (shcl/core/shell-grammar:*intermediate-parse-error-hook*
          shcl/core/shell-grammar:*intermediate-parse-error-hook*)
         (seen-errors (make-hash-table :test 'eq))
         (suggestions (fset:empty-set)))
    (labels
        ((add-error (err)
           (when (and (parse-error-involves-sigil-token-p err sigil-token)
                      (not (nth-value 1 (gethash err seen-errors))))
             (setf (gethash err seen-errors) t)
             (let* ((expected-types (parse-error-expected-types err))
                    (all-expected-types (concatmap-iterator expected-types (type-expander)))
                    (suggestion-producer (lambda (type)
                                           (completion-suggestions type token-to-complete context)))
                    (err-suggestions (concatmap-iterator all-expected-types suggestion-producer)))
               ;; Consume suggestions eagerly so they are computed in
               ;; the dynamic context where the error was produced
               (do-iterator (suggestion err-suggestions)
                 (fset:adjoinf suggestions suggestion))))))
      (add-hook
       'shcl/core/shell-grammar:*intermediate-parse-error-hook*
       #'add-error)
      (handler-case
          (do-iterator (command command-iterator)
            (declare (ignore command)))
        (shcl/core/parser:parse-failure (err)
          ;; This really should have already been handled, but just in
          ;; case...
          (add-error (shcl/core/parser:parse-failure-error-object err))))
      suggestions)))

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
