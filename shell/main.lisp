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

(defpackage :shcl/shell/main
  (:use
   :common-lisp :trivial-gray-streams :shcl/core/lexer :shcl/core/shell-grammar
   :shcl/core/utility :shcl/core/evaluate :shcl/core/baking :shcl/core/thread
   :shcl/core/lisp-interpolation :shcl/core/shell-readtable :shcl/core/builtin
   :shcl/core/iterator)
  (:import-from :shcl/core/posix #:exit)
  (:import-from :shcl/shell/directory)
  (:import-from :shcl/shell/logs)
  (:import-from :shcl/shell/lisp-repl)
  (:import-from :shcl/core/parser #:abort-parse)
  (:import-from :shcl/shell/prompt
   #:with-history #:history-enter #:history-set-size #:make-editline-stream)
  (:import-from :cl-cli)
  (:import-from :uiop)
  (:import-from :swank)
  (:import-from :fset)
  (:export #:main #:run-shell-commands-in-stream))
(in-package :shcl/shell/main)

(optimization-settings)

(defparameter *shell-readtable* +standard-shell-readtable+
  "The shell readtable used by the main shell loop.

This only changes how `shcl/shell/main:main' reads shell commands.
Macros and functions which consume shell expressions are not impacted
in any way by this variable.")

(define-builtin -shcl-reset-readtable (args)
  (unless (equal 1 (fset:size args))
    (format *error-output* "Invalid number of arguments: ~A" (fset:size args))
    (return-from -shcl-reset-readtable 1))
  (setf *shell-readtable* +standard-shell-readtable+)
  0)

(defparameter *fresh-prompt* t
  "If non-nil, then the next time the user is prompted for input, the
standard prompt should be displayed.

When nil, a prompt indicating that the previous line is incomplete
should be displayed.")

(defun main-prompt ()
  "Return the string which should be displayed as the prompt for the
next line.

See `*fresh-prompt*'."
  (let ((result (if *fresh-prompt* "shcl> " "> ")))
    (setf *fresh-prompt* nil)
    result))

(defun main-token-iterator (stream form-queue)
  "Return an iterator that produces the tokens found in `stream'.

Tokens are read using `*shell-readtable*', and their bake forms (see
`shcl/core/baking:bake-tokens') are added to form-queue."
  (lookahead-iterator-wrapper
   (bake-tokens
    (map-iterator (token-iterator-symbolic-readtable stream '*shell-readtable*)
                  (lambda (token)
                    (debug-log status "TOKEN: ~A" token)
                    token))
    form-queue)))

(defun restartable-command-iterator (stream form-queue &key history)
  "Return an iterator that emits the fully parsed commands that
`stream' contains.

Tokens are read using `main-token-iterator', and bake forms are placed
in `form-queue'.  See `shcl/core/baking:bake-tokens'."
  (let* ((captured-input (when history (make-string-output-stream)))
         (wrapped-stream (if history (make-echo-stream stream captured-input) stream))
         (tokens (main-token-iterator wrapped-stream form-queue))
         (commands (command-iterator tokens)))
    (make-iterator ()
      (let ((*fresh-prompt* t))
        (labels
            ((record-history ()
               (when history
                 (history-enter history (get-output-stream-string captured-input)))
               (setf *fresh-prompt* t))
             (reset-token-iterator ()
               (loop :while (read-char-no-hang wrapped-stream nil nil))
               (setf tokens (main-token-iterator wrapped-stream form-queue)
                     commands (command-iterator tokens))
               (record-history)))
          (tagbody
           start
             (restart-case
                 (progn
                   (do-iterator (value commands)
                     (record-history)
                     (emit value))
                   (stop))
               (ignore ()
                 (reset-token-iterator)
                 (go start)))))))))

(defparameter *enable-lisp-splice* nil
  "SHCL should permit splicing in lisp forms.

Do not set this variable directly.  `main' will set this based on the
arguments it receives.")
(defparameter *debug* nil)
(defparameter *help* nil
  "SHCL should display some basic help information and then exit.

Do not set this variable directly.  `main' will set this based on the
arguments it receives.")

(defparameter *options*
  '((*enable-lisp-splice* nil "Extend shell language to support splicing lisp expressions")
    (*debug* nil "Run a SWANK server")
    (*help* nil "Show list of options"))
  "The set of options recognized by `main'.  See `cl-cli:parse-cli'.")

(defmacro with-options ((argv &key (options '*options*)) &body body)
  "Parse options and establish dynamic bindings for them.

See `cl-cli:parse-cli'."
  (let ((vars (gensym "VARS"))
        (values (gensym "VALUES"))
        (command (gensym "COMMAND"))
        (kwargs (gensym "KWARGS"))
        (rest (gensym "REST")))
    `(multiple-value-bind (,vars ,values ,command ,kwargs ,rest) (cl-cli:parse-cli ,argv ,options)
       (declare (ignore ,command ,kwargs ,rest))
       (progv ,vars ,values
         ,@body))))

(defun exit-info-iterator (stream &key history)
  (let* ((form-queue (make-queue))
         (commands (restartable-command-iterator stream form-queue :history history)))
    (make-iterator ()
      (restart-case
          (progn
            (do-iterator (tree commands)
              (loop
                 (let* ((stop '#:stop)
                        (form (dequeue-no-block form-queue stop)))
                   (when (eq stop form)
                     (return))
                   (debug-log status "EVAL ~A" form)
                   (eval form)))
              (debug-log status "TREE: ~A" tree)
              (restart-case
                  (let ((result (evaluate tree)))
                    (debug-log status "RESULT ~A" result)
                    (emit result))
                (skip ())))
            (stop))
        (die () (exit 1))))))

(define-builtin shcl-enable-lisp-syntax (args)
  "Permit the use of lisp splice forms in shell expressions."
  (unless (equal 1 (fset:size args))
    (return-from shcl-enable-lisp-syntax 1))
  (setf *shell-readtable* (use-table *shell-readtable* *splice-table*))
  0)

(define-builtin -shcl-start-swank (args)
  "Start a swank server.

Optional argument is the port to start the server on."
  (let ((port 4005))
    (case (fset:size args)
      (1)
      (2
       (setf port (parse-integer (fset:lookup args 1) :junk-allowed nil)))
      (otherwise
       (format *error-output* "-shcl-start-swank: Invalid arguments~%")
       (return-from -shcl-start-swank 2)))
    (swank:create-server :port port)
    0))

(defun main ()
  "Start running SHCL's shell.

This function assumes that SHCL \"owns\" the process.  That means that
the process environment won't change without SHCL's knowledge and that
SHCL has total control over the process environment.  This means, for
example, that...
* SHCL is soley responsible for all file descriptor manipulations
* SHCL may install or remove any signal handlers it wishes"
  ;; We assume that shcl/core/utility:observe-revival has already run
  (with-options ((uiop:raw-command-line-arguments))
    (when *help*
      (cl-cli:help *options* nil :prog-name "shcl")
      (return-from main))

    (when *debug*
      (-shcl-start-swank (fset:seq "-shcl-start-swank")))

    (when *enable-lisp-splice*
      (setf *shell-readtable* *splice-table*))

    (with-history (h)
      (history-set-size h 800)
      (let ((stream (make-editline-stream :prompt-fn 'main-prompt :history h))
            (*package* (find-package :shcl-user)))
        (handler-bind
            ((abort-parse
              (lambda (e)
                (when-let ((restart (find-restart 'ignore)))
                  (format *error-output* "Parse error: ~A~%" e)
                  (invoke-restart restart)))))
          (do-iterator (exit-info (exit-info-iterator stream :history h))
            (declare (ignore exit-info))))))))
