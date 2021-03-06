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
   :shcl/core/utility :shcl/core/translate
   :shcl/core/lisp-interpolation :shcl/core/dispatch-table)
  (:import-from :shcl/core/sequence
   #:lazy-map #:do-while-popf #:do-sequence #:wrap-with #:cache-impure)
  (:import-from :shcl/core/command #:define-builtin)
  (:import-from :shcl/core/shell-lambda
   #:handle-command-errors #:with-parsed-arguments)
  (:import-from :shcl/core/posix #:exit)
  (:import-from :shcl/core/command #:exit-condition #:exit-condition-exit-info)
  (:import-from :shcl/core/exit-info
   #:truthy-exit-info #:exit-info-code #:exit-info-p #:falsey-exit-info)
  (:import-from :shcl/core/environment #:env)
  (:import-from :shcl/shell/directory)
  (:import-from :shcl/shell/builtins)
  (:import-from :shcl/shell/lisp-repl)
  (:import-from :shcl/core/parser #:parse-failure)
  (:import-from :shcl/shell/prompt
   #:with-history #:history-enter #:history-set-size #:make-editline-stream
   #:interpret-prompt-string)
  (:import-from :shcl/shell/complete #:completion-suggestions-for-input)
  (:import-from :uiop)
  (:import-from :swank)
  (:import-from :fset)
  (:export
   #:main #:default-prompt #:*prompt-function* #:*fresh-prompt*))
(in-package :shcl/shell/main)

(optimization-settings)

(defun set-cl-fad-tmp ()
  ;; We use uiop:getenv instead of shcl/core/environment:env because
  ;; this is called during revival and SHCL's environment might not
  ;; have been brough up, yet.
  (let* ((env-tmpdir (uiop:getenv "TMPDIR"))
         (tmpdir (or (when env-tmpdir
                       (cl-fad:pathname-as-directory env-tmpdir))
                     #P"/tmp/")))
    (setf (logical-pathname-translations "temporary-files") `(("*.*.*" ,tmpdir)))))

(on-revival set-cl-fad-tmp)

(defun clear-cl-fad-tmp ()
  (setf (logical-pathname-translations "temporary-files") nil))

;; There's no need to retain the tmp directiory used during build
;; time.
(on-dump clear-cl-fad-tmp)

(defpackage :shcl-user
  (:use :common-lisp :shcl/shell/lisp-repl)
  (:import-from :shcl/core/environment #:env)
  (:import-from :shcl/core/command #:define-builtin)
  (:import-from :shcl/shell/main
    #:default-prompt #:*prompt-function* #:*fresh-prompt*))

(defparameter *shell-readtable* (standard-shell-readtable)
  "The shell readtable used by the main shell loop.

This only changes how `shcl/shell/main:main' reads shell commands.
Macros and functions which consume shell expressions are not impacted
in any way by this variable.")

(define-builtin -shcl-reset-readtable ()
  "Restore the standard shell readtable."
  (setf *shell-readtable* (standard-shell-readtable))
  0)

(defparameter *fresh-prompt* t
  "If non-nil, then the next time the user is prompted for input, the
standard prompt should be displayed.

When nil, a prompt indicating that the previous line is incomplete
should be displayed.")

(defun default-prompt ()
  "The default prompt function.

If PS1 and PS2 are defined, they will be used as the prompt.
Otherwise, this function uses a generic prompt."
  (if *fresh-prompt*
      (let ((ps1 (env "PS1" nil)))
        (if ps1
            (interpret-prompt-string ps1)
            "shcl> "))
      (let ((ps2 (env "PS2" nil)))
        (if ps2
            (interpret-prompt-string ps2)
            "> "))))

(defvar *prompt-function* 'default-prompt
  "A function which produces the shell's prompt.

This function should return a string.  See also: `*fresh-prompt*'.")

(defun main-prompt ()
  "Return the string which should be displayed as the prompt for the
next line.

See `*fresh-prompt*'."
  (let ((result (funcall *prompt-function*)))
    (setf *fresh-prompt* nil)
    result))

(defun main-complete (input-string cursor-position)
  (completion-suggestions-for-input input-string cursor-position *shell-readtable*))

(defun logging-token-sequence (stream)
  "Return a sequence that produces the tokens found in `stream'.

Tokens are read using `*shell-readtable*'."
  (lazy-map (tokens-in-stream-symbolic-readtable stream '*shell-readtable*)
            (lambda (token)
              (debug-log status "TOKEN: ~A" token)
              token)))

(defun logging-command-sequence (tokens)
  (lazy-map (commands-for-tokens tokens)
            (lambda (command)
              (debug-log status "COMMAND: ~A" command)
              command)))

(defun logging-evaluation-form-sequence (commands)
  (lazy-map (evaluation-forms-for-commands commands)
            (lambda (form)
              (debug-log status "EVAL: ~A" form)
              form)))

(define-condition shell-eof (error)
  ())

(defun shell-read (stream &key history (eof-error-p t) eof-value)
  (let* ((captured-input (when history (make-string-output-stream)))
         (wrapped-stream (if history (make-echo-stream stream captured-input) stream))
         results
         (*fresh-prompt* t))
    (labels
        ((record-history ()
           (when history
             (history-enter history (get-output-stream-string captured-input)))
           (setf *fresh-prompt* t))
         (initialize-sequence ()
           (setf results (as-> wrapped-stream x
                               (logging-token-sequence x)
                               (logging-command-sequence x)
                               (logging-evaluation-form-sequence x))))
         (reset-token-sequence ()
           (loop :while (read-char-no-hang wrapped-stream nil nil))
           (initialize-sequence)
           (record-history)))
      (initialize-sequence)
      (tagbody
       restart
         (restart-case
             (do-while-popf (value results)
               (record-history)
               (return-from shell-read value))
           (give-up-on-read ()
             (reset-token-sequence)
             (go restart))))
      (if eof-error-p
          (error 'shell-eof)
          eof-value))))

(defun %shell-repl (stream history)
  (let ((last-result (list (truthy-exit-info))))
    (labels
        ((handle-parse-failure (e)
           (when-let ((restart (find-restart 'ignore-command)))
             (format *error-output* "Parse error: ~A~%" e)
             (invoke-restart restart)))
         (handle-shell-eof (e)
           (declare (ignore e))
           (return-from %shell-repl (values-list last-result)))
         (read-with-handlers ()
           (handler-bind
               ((parse-failure #'handle-parse-failure)
                (shell-eof #'handle-shell-eof))
             (shell-read stream :history history)))
         (eval-with-restarts (form)
           (restart-case
               (eval form)
             (give-up-on-eval ()
               (falsey-exit-info)))))
      (declare (dynamic-extent #'handle-parse-failure)
               (dynamic-extent #'handle-shell-eof)
               (dynamic-extent #'read-with-handlers)
               (dynamic-extent #'eval-with-restarts))

      (loop
        (let* ((form (read-with-handlers))
               (result (multiple-value-list (eval-with-restarts form))))
          (debug-log status "RESULT: ~A" result)
          (setf last-result result))))))

(defun shell-repl (stream &key history)
  (loop
    (restart-case
        (return (%shell-repl stream history))
      (exit-repl ()
        (return (falsey-exit-info)))
      (restart-repl ()))))

(defparameter *debug* nil)
(defparameter *help* nil
  "SHCL should display some basic help information and then exit.

Do not set this variable directly.  `main' will set this based on the
arguments it receives.")

(defmacro with-options (argv &body body)
  "Parse options and establish dynamic bindings for them."
  `(with-parsed-arguments
       (&flag (*debug* "--swank")
              (*help* "--help"))
       ,argv
     ,@body))

(defun help ()
  (format t "shcl [options]
An unholy union of Common Lisp and POSIX shell.

Supported options:
--swank Start a swank server for debugging
--help Print this message
"))

(define-builtin shcl-enable-lisp-syntax ()
  "Permit the use of lisp splice forms in shell expressions."
  (setf *shell-readtable* (use-table *shell-readtable* *splice-table*))
  0)

(define-builtin -shcl-start-swank (&optional (port "4005"))
  "Start a swank server.

Optional argument is the port to start the server on."
  (let ((port (parse-integer port :junk-allowed nil)))
    (swank:create-server :port port)
    0))

(defun main (&optional (argv (uiop:raw-command-line-arguments)))
  "Start running SHCL's shell.

This function assumes that SHCL \"owns\" the process.  That means that
the process environment won't change without SHCL's knowledge and that
SHCL has total control over the process environment.  This means, for
example, that...
* SHCL is soley responsible for all file descriptor manipulations
* SHCL may install or remove any signal handlers it wishes"
  ;; We assume that shcl/core/utility:observe-revival has already run
  (handle-command-errors "shcl"
    (with-options argv
      (unless (zerop (length *help*))
        (help)
        (return-from main))

      (unless (zerop (length *debug*))
        (-shcl-start-swank (fset:seq "-shcl-start-swank")))

      (with-history (h)
        (history-set-size h 800)
        (let ((stream (make-editline-stream :prompt-fn 'main-prompt :history h
                                            :complete-fn 'main-complete))
              (*package* (find-package :shcl-user))
              (startup-file (probe-file "~/.shclrc.lisp")))
          (when startup-file
            (load startup-file))

          (handler-case
              (shell-repl stream :history h)
            (exit-condition (e)
              (exit-condition-exit-info e))))))))
