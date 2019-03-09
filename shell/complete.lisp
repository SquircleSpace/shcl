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
  (:import-from :shcl/core/sequence
   #:attachf #:empty-p #:concatenate-sequences #:eager-flatmap-sequence
   #:do-while-popf #:eager-map #:eager-filter #:flatten-sequence #:walk)
  (:import-from :shcl/core/advice #:define-advice)
  (:import-from :shcl/core/shell-grammar
   #:parse-simple-command #:parse-simple-command-word #:commands-for-tokens)
  (:import-from :shcl/core/data #:clone #:define-data #:define-cloning-setf-expander #:clone)
  (:import-from :shcl/core/lexer
   #:reserved-word #:literal-token-class #:literal-token-string #:token-value
   #:simple-word #:simple-word-text #:token-position #:tokens-in-string)
  (:import-from :shcl/core/parser
   #:unexpected-eof #:unexpected-eof-expected-type #:type-mismatch #:parser-try #:parser-var-let*
   #:type-mismatch-expected-type #:expected-eof #:expected-eof-got #:type-mismatch-got #:choice
   #:choice-errors #:parse-failure #:parse-failure-error-object #:parser-var
   #:parser-error-vars)
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
  (:import-from :shcl/shell/prompt
   #:completion-suggestion-display-text #:completion-suggestion-replacement-text
   #:completion-suggestion-replacement-range
   #:completion-suggestion-wants-trailing-space-p)
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

(define-advice parse-simple-command
    :around tab-complete
    ()
  (unless *collect-tab-complete-info*
    (return-from parse-simple-command
      (call-next-method)))

  (parser-var-let*
      ((parsed-simple-command-words (fset:empty-seq)))
    (call-next-method)))

(defmethod expand-type ((class standard-class) unique-table)
  (unless (eq class (find-class 'reserved-word))
    (concatenate-sequences
     (list class)
     (eager-flatmap-sequence
      (class-direct-subclasses class)
      (lambda (subclass)
        (expand-type subclass unique-table))
      (fset:empty-seq)))))

(define-advice parse-simple-command-word
    :around tab-complete
    ()
  (unless *collect-tab-complete-info*
    (return-from parse-simple-command-word
      (call-next-method)))

  (assert (parser-var 'parsed-simple-command-words))
  (let ((return-values (multiple-value-list (call-next-method))))
    (attachf (parser-var 'parsed-simple-command-words) (car return-values))
    (values-list return-values)))

(defgeneric parse-error-involves-sigil-token-p (err sigil-token))

(defmethod parse-error-involves-sigil-token-p (err sigil-token)
  nil)

(defgeneric parse-error-expected-types (err))

(defmethod parse-error-involves-sigil-token-p ((err expected-eof) sigil-token)
  (eq sigil-token (expected-eof-got err)))

(defmethod parse-error-expected-types ((err expected-eof))
  nil)

(defmethod parse-error-involves-sigil-token-p ((err type-mismatch) sigil-token)
  (eq sigil-token (type-mismatch-got err)))

(defmethod parse-error-expected-types ((err type-mismatch))
  (list (type-mismatch-expected-type err)))

(defmethod parse-error-expected-types ((err choice))
  (eager-flatmap-sequence
   (choice-errors err)
   'parse-error-expected-types
   (fset:empty-seq)))

(define-data completion-context ()
  ((cursor-point
    :reader completion-context-cursor-point
    :initarg :cursor-point
    :initform (required))
   (readtable
    :reader completion-context-readtable
    :initarg :readtable
    :initform (required))
   (token-range
    :reader completion-context-token-range
    :initarg :token-range
    :initform (required))))

(defgeneric completion-suggestions (desired-token-type token-fragment context parser-vars)
  (:method-combination concatenate-sequences))

(defmethod completion-suggestions concatenate-sequences
    (desired-token-type token-fragment context parser-vars)
  (declare (ignore desired-token-type token-fragment context parser-vars))
  nil)

(defmethod completion-suggestions concatenate-sequences
    ((desired literal-token-class) token context parser-vars)
  (declare (ignore parser-vars))
  (let ((desired-string (literal-token-string desired))
        (token-value (token-value token)))
    (if (sequence-starts-with-p desired-string token-value)
        (list (make-simple-completion-suggestion desired-string context))
        nil)))

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
  (let ((result (fset:empty-seq)))
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
          (attachf result executable-name))))
    result))

(defun all-binary-commands ()
  (let ((result (fset:empty-seq)))
    (do-iterator (path (colon-list-iterator $path))
      (when (equal "" path)
        ;; POSIX says we need to do this...
        (setf path "."))
      (attachf result (executables-in-directory path)))
    (flatten-sequence result)))

(defmethod completion-suggestions concatenate-sequences
    ((desired (eql (find-class 'simple-word)))
     (token simple-word)
     context
     parser-vars)
  (multiple-value-bind (command-words valid-p) (fset:lookup parser-vars 'parsed-simple-command-words)
    (unless valid-p
      (return-from completion-suggestions))

    (when (empty-p command-words)
      (labels
          ((compatible-p (command)
             (sequence-starts-with-p command (simple-word-text token))))
        (eager-map (eager-filter (all-binary-commands) #'compatible-p (fset:empty-seq))
                   (lambda (str)
                     (make-simple-completion-suggestion str context))
                   (fset:empty-seq))))))

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

(defmethod expand-compound-type ((type-car (eql 'or)) type-cdr unique-table)
  (eager-flatmap-sequence type-cdr (lambda (type) (expand-type type unique-table)) (fset:empty-seq)))

(defclass sigil-token ()
  ())

(defmethod token-value ((sigil sigil-token))
  nil)

(define-data completion-suggestion ()
  ((display-text
    :initarg :display-text
    :reader completion-suggestion-display-text
    :initform (required))
   (replacement-text
    :initarg :replacement-text
    :reader completion-suggestion-replacement-text
    :initform (required))
   (replacement-range
    :initarg :replacement-range
    :reader completion-suggestion-replacement-range
    :initform (required))
   (wants-trailing-space-p
    :initarg :wants-trailing-space-p
    :reader completion-suggestion-wants-trailing-space-p
    :initform (required))))

(defun make-simple-completion-suggestion (suggestion-text completion-context)
  (make-instance 'completion-suggestion :display-text suggestion-text
                 :replacement-text suggestion-text
                 :replacement-range (completion-context-token-range completion-context)
                 :wants-trailing-space-p t))

(defun completion-suggestions-for-tokens (leading-tokens token-to-complete context)
  (let* ((*collect-tab-complete-info* t)
         (sigil-token (make-instance 'sigil-token))
         (commands (commands-for-tokens
                    (concatenate-sequences
                     leading-tokens
                     (list sigil-token))))
         (suggestions (fset:empty-set)))
    (labels
        ((add-error (err)
           (when (typep err 'choice)
             (let ((errors (choice-errors err)))
               (do-while-popf (other-err errors)
                 (add-error other-err)))
             (return-from add-error))

           (when (parse-error-involves-sigil-token-p err sigil-token)
             (let* ((expected-types (parse-error-expected-types err))
                    (all-expected-types (eager-flatmap-sequence expected-types (type-expander) (fset:empty-seq)))
                    (suggestion-producer (lambda (type)
                                           (completion-suggestions type token-to-complete context (parser-error-vars err)))))
               (setf suggestions (eager-flatmap-sequence all-expected-types suggestion-producer suggestions))))))
      (handler-case
          (do-while-popf (command commands)
            (declare (ignore command)))
        (parse-failure (err)
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
  (let ((tokens (tokens-in-string input-text
                                  :readtable readtable))
        (tokens-under-consideration (fset:empty-seq))
        end-found)
    (do-while-popf (token tokens)
      (let* ((token-start (position-record-offset (token-position token)))
             (token-end (+ token-start (length (token-value token)))))
        (when (<= token-start cursor-point)
          (fset:push-last tokens-under-consideration token))
        (when (>= token-end cursor-point)
          (setf end-found t)
          (return))))
    (let* ((token-to-complete (if end-found (fset:pop-last tokens-under-consideration) *empty-token*))
           (token-start (if end-found
                            (position-record-offset (token-position token-to-complete))
                            (length input-text)))
           (token-end (if end-found
                          (+ token-start (length (token-value token-to-complete)))
                          token-start))
           (token-range (cons token-start token-end)))
      (completion-suggestions-for-tokens
       tokens-under-consideration
       token-to-complete
       (make-instance 'completion-context :cursor-point cursor-point
                      :readtable readtable :token-range token-range)))))
