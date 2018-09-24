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
  (:import-from :shcl/core/advice #:define-advice)
  (:import-from :shcl/core/shell-grammar
   #:parse-simple-command #:parse-simple-command-word #:command-iterator
   #:*intermediate-parse-error-hook*)
  (:import-from :shcl/core/data #:clone)
  (:import-from :shcl/core/lexer
   #:reserved-word #:literal-token-class #:literal-token-string #:token-value
   #:simple-word #:simple-word-text #:token-iterator #:token-position)
  (:import-from :shcl/core/parser
   #:unexpected-eof #:unexpected-eof-expected-type #:type-mismatch
   #:type-mismatch-expected-type #:parser-bind #:parser-error #:parser-value
   #:expected-eof #:expected-eof-got #:type-mismatch-got #:choice
   #:choice-errors-iterator #:parse-failure #:parse-failure-error-object)
  (:import-from :shcl/core/fd-table
   #:receive-ref-counted-fd #:retained-fd-openat #:fd-wrapper-value
   #:with-dir-ptr-for-fd)
  (:import-from :shcl/core/posix
   #:fstat #:faccessat #:syscall-error #:do-directory-contents)
  (:import-from :shcl/core/posix-types #:o-rdonly #:st-mode #:x-ok #:at-eaccess)
  (:import-from :shcl/core/support #:s-isdir)
  (:import-from :shcl/core/environment #:colon-list-iterator #:$path)
  (:import-from :shcl/core/positional-stream
   #:positional-input-stream #:position-record-offset)
  (:import-from :shcl/core/working-directory #:get-fd-current-working-directory)
  (:import-from :closer-mop #:class-direct-subclasses)
  (:import-from :fset)
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

(define-advice parse-simple-command
    :around tab-complete
    (iter)
  (declare (ignore iter))
  (unless *collect-tab-complete-info*
    (return-from parse-simple-command
      (call-next-method)))

  (let ((*command-words* (fset:empty-seq)))
    (call-next-method)))

(deftype command-word (real-type)
  real-type)

(defmethod expand-compound-type ((type-car (eql 'command-word)) type unique-table)
  (destructuring-bind (type-car type) type
    (declare (ignore type-car))
    (let ((class (etypecase type
                   (symbol (find-class type))
                   (standard-class type))))

      (unless (eq class (find-class 'reserved-word))
        (concatenate-iterables
         (list class)
         (concatmap-iterator
          (class-direct-subclasses class)
          (lambda (subclass)
            (expand-type `(command-word ,subclass) unique-table))))))))

(defgeneric wrap-expected-type (error-object))

(defmethod wrap-expected-type ((err unexpected-eof))
  (clone err :expected-type `(command-word ,(unexpected-eof-expected-type err))))

(defmethod wrap-expected-type ((err type-mismatch))
  (clone err :expected-type `(command-word ,(type-mismatch-expected-type err))))

(define-advice parse-simple-command-word
    :around tab-complete
    (iter)
  (declare (ignore iter))
  (unless *collect-tab-complete-info*
    (return-from parse-simple-command-word
      (call-next-method)))

  (assert *command-words*)
  (parser-bind (value error-p) (call-next-method)
    (cond
      (error-p
       (parser-error (wrap-expected-type value)))
      (t
       (fset:push-last *command-words* value)
       (parser-value value)))))

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

(defmethod parse-error-involves-sigil-token-p ((err expected-eof) sigil-token)
  (eq sigil-token (expected-eof-got err)))

(defmethod parse-error-expected-types ((err expected-eof))
  (list :eof))

(defmethod parse-error-involves-sigil-token-p ((err type-mismatch) sigil-token)
  (eq sigil-token (type-mismatch-got err)))

(defmethod parse-error-expected-types ((err type-mismatch))
  (list (type-mismatch-expected-type err)))

(defmethod parse-error-expected-types ((err choice))
  (concatmap-iterator
   (choice-errors-iterator err :recursive-p t)
   'parse-error-expected-types))

(defclass completion-context ()
  ((cursor-point
    :reader completion-context-cursor-point
    :initarg :cursor-point
    :initform (required))
   (readtable
    :reader completion-context-readtable
    :initarg :readtable
    :initform (required))))

(defgeneric completion-suggestions (desired-token-type token-fragment context)
  (:method-combination concatenate-iterables))

(defmethod completion-suggestions concatenate-iterables
    (desired-token-type token-fragment context)
  (declare (ignore desired-token-type token-fragment context))
  *empty-iterator*)

(defmethod completion-suggestions concatenate-iterables
    ((desired literal-token-class) token context)
  (let ((desired-string (literal-token-string desired))
        (token-value (token-value token)))
    (if (starts-with-p desired-string token-value)
        (list-iterator (list desired-string))
        *empty-iterator*)))

(defun directory-p (at-fd path)
  (handler-case
      (receive-ref-counted-fd
          (file (retained-fd-openat at-fd path o-rdonly))
        (s-isdir (slot-value (fstat (fd-wrapper-value file)) 'st-mode)))
    (syscall-error ()
      nil)))

(defun executable-p (at-fd path)
  (handler-case
      (progn
        (faccessat (fd-wrapper-value at-fd) path x-ok at-eaccess)
        t)
    (syscall-error ()
      nil)))

(defmacro do-executables-in-dir-fd ((executable-name dir-fd &optional result) &body body)
  (let ((dir (gensym "DIR"))
        (dir-ptr (gensym "DIR-PTR"))
        (file-name (gensym "FILE-NAME")))
    `(let ((,dir ,dir-fd))
       (with-dir-ptr-for-fd (,dir-ptr ,dir)
         (do-directory-contents (,file-name ,dir-ptr ,result)
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
               (retained-fd-openat
                (get-fd-current-working-directory)
                path o-rdonly)
             (syscall-error ()
               (return-from executables-in-directory result)))))
      (receive-ref-counted-fd
          (dir-fd (retained-fd-open-dir))
        (do-executables-in-dir-fd (executable-name dir-fd)
          (vector-push-extend executable-name result))))
    result))

(defun all-binary-commands ()
  (let ((result-vector (make-extensible-vector)))
    (do-iterator (path (colon-list-iterator $path))
      (when (equal "" path)
        (setf path "."))
      (vector-push-extend (executables-in-directory path) result-vector))
    (concatenate-iterable-collection result-vector)))

(defmethod completion-suggestions concatenate-iterables
    ((desired (eql (find-class 'simple-word)))
     (token simple-word)
     context)
  (unless *command-words*
    (return-from completion-suggestions))

  (when (equal 0 (fset:size *command-words*))
    (labels
        ((compatible-p (command)
           (starts-with-p command (simple-word-text token))))
      (filter-iterator (all-binary-commands) #'compatible-p))))

(defvar *empty-token* (make-instance 'simple-word :text ""))

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
    (class-direct-subclasses type)
    (lambda (subclass)
      (expand-type subclass unique-table)))))

(defmethod expand-compound-type ((type-car (eql 'or)) type-cdr unique-table)
  (concatmap-iterator type-cdr (lambda (type) (expand-type type unique-table))))

(defclass sigil-token ()
  ())

(defmethod token-value ((sigil sigil-token))
  nil)

(defun completion-suggestions-for-tokens (leading-tokens token-to-complete context)
  (let* ((*collect-tab-complete-info* t)
         (sigil-token (make-instance 'sigil-token))
         (command-iterator (command-iterator
                            (lookahead-iterator-wrapper
                             (concatenate-iterables
                              leading-tokens
                              (list sigil-token)))))
         (*intermediate-parse-error-hook* *intermediate-parse-error-hook*)
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
      (add-hook '*intermediate-parse-error-hook* #'add-error)
      (handler-case
          (do-iterator (command command-iterator)
            (declare (ignore command)))
        (parse-failure (err)
          ;; This really should have already been handled, but just in
          ;; case...
          (add-error (parse-failure-error-object err))))
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
  (let ((token-iterator (token-iterator
                         (make-instance 'positional-input-stream
                                        :underlying-stream (make-string-input-stream input-text))
                         :readtable readtable))
        (tokens (make-extensible-vector))
        end-found)
    (do-iterator (token token-iterator)
      (let* ((token-start (position-record-offset (token-position token)))
             (token-end (+ token-start (length (token-value token)))))
        (when (<= token-start cursor-point)
          (vector-push-extend token tokens))
        (when (>= token-end cursor-point)
          (setf end-found t)
          (return))))
    (completion-suggestions-for-tokens
     tokens
     (if end-found (vector-pop tokens) *empty-token*)
     (make-instance 'completion-context :cursor-point cursor-point
                    :readtable readtable))))
