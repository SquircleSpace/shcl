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

(defpackage :shcl/core/command
  (:use :common-lisp :shcl/core/utility)
  (:import-from :fset)
  (:import-from :alexandria)
  (:import-from :closer-mop)
  (:import-from :shcl/core/shell-lambda
                #:command-error #:shell-lambda #:handle-command-errors)
  (:import-from :shcl/core/fd-table
                #:with-fd-streams #:with-private-fds #:linearize-fd-bindings
                #:fd-wrapper-value)
  (:import-from :shcl/core/exit-info #:exit-info #:make-exit-info)
  (:import-from :shcl/core/data #:define-data)
  (:import-from :shcl/core/shell-environment #:preserve-special-variable #:with-subshell
                #:extend-shell-environment)
  (:import-from :shcl/core/support
                #:wifexited #:wexitstatus #:wifsignaled #:wtermsig #:wifstopped
                #:wstopsig)
  (:import-from :shcl/core/posix-types #:wuntraced)
  (:import-from :shcl/core/posix #:waitpid)
  (:import-from :shcl/core/environment #:linearized-exported-environment
                #:do-environment-bindings #:environment-binding-exported-p
                #:environment-binding-value #:environment-binding-readonly-p
                #:deconstruct-environment-assignment-string)
  (:import-from :shcl/core/working-directory #:get-fd-current-working-directory)
  (:import-from :shcl/core/fork-exec #:run)
  (:export
   #:define-builtin #:define-special-builtin
   #:lookup-command :invoke-command :*command-namespace*
   #:make-command-namespace
   #:command-namespace-table :command-namespace-fallback #:command
   #:special-builtin #:builtin #:binary :invoke-command
   #:install-command #:command-priority :redefining-command
   #:confusing-command-name
   #:exit-condition :exit-condition-exit-info
   #:command-error #:handle-command-errors #:wrap-errors)
  (:documentation
   "This package contains functionality related to defining and
running shell commands."))
(in-package :shcl/core/command)

(optimization-settings)

(defgeneric invoke-command (command environment-modifier &rest args)
  (:documentation
   "Run the given command.

Think of this function like `funcall' for commands.

`environment-modifier' is a function that will, well, modify the
environment.  For example, it might set environment variables or
modify file descriptors.

`args' contains the strings that are passed to the shell command as
arguments."))

(defgeneric command-priority (command)
  (:documentation
   "Return the priority of the given command.

See `install-command' to learn more about priorities.

You do not need to specialize this function, but it is recommended."))

(define-data command-namespace ()
  ((table
    :initarg :table
    :initform (fset:empty-map)
    :updater command-namespace-table)
   (fallback
    :initarg :fallback
    :updater command-namespace-fallback)))

(defun make-command-namespace (&key fallback)
  "Produce a new, empty `command-namespace'.

`fallback' is the command that should"
  (make-instance 'command-namespace :fallback fallback))

(define-data binary ()
  ()
  (:documentation
   "An object representing a shell command backed by a file on disk."))

(defvar *command-namespace* (make-instance 'command-namespace :fallback (make-instance 'binary))
  "The namespace containing all known commands.")

(preserve-special-variable '*command-namespace*)

(defmethod documentation ((command-name string) (doc-type (eql 'command)))
  (documentation (lookup-command command-name) t))

(define-condition confusing-command-name (warning)
  ((name
    :initarg :name
    :reader confusing-command-name-name
    :initform (required)))
  (:report
   (lambda (c s)
     (format s "The given command name (~S) will produce confusing
results.  Users normally expect that commands containing a slash will
invoke a binary instead of a builtin."
             (confusing-command-name-name c)))))

(define-condition redefining-command (warning)
  ((old-command
    :initarg :old-command
    :reader redefining-command-old-command
    :initform (required))
   (new-command
    :initarg :new-command
    :reader redefining-command-new-command
    :initform (required))
   (name
    :initarg :name
    :reader redefining-command-name
    :initform (required))
   (priority
    :initarg :priority
    :reader redefining-command-priority
    :initform (required)))
  (:report
   (lambda (c s)
     (format s "Replacing existing command with name ~S"
             (redefining-command-name c)))))

(defun install-command (name command &optional (priority (command-priority command)))
  (when (find #\/ name)
    (warn 'confusing-command-name :name name))
  (let* ((table (command-namespace-table *command-namespace*))
         (priority-table (or (fset:lookup table name) (fset:empty-map))))
    (multiple-value-bind (existing-command found) (fset:lookup priority-table priority)
      (when found
        (warn 'redefining-command :name name :priority priority
              :old-command existing-command :new-command command))
      (setf (fset:lookup priority-table priority) command)
      (setf (fset:lookup (command-namespace-table *command-namespace*) name) priority-table)
      (values))))

(defun lookup-command (name)
  (multiple-value-bind (value found) (fset:lookup (command-namespace-table *command-namespace*) name)
    (if (and found (not (fset:empty? value)))
        (nth-value 1 (fset:at-rank value 0))
        (command-namespace-fallback *command-namespace*))))

(defun run-binary (args)
  (let ((bindings (linearize-fd-bindings)))
    (with-private-fds (fds)
      (run args
           :fd-alist bindings
           :managed-fds fds
           :environment (linearized-exported-environment)
           :working-directory-fd (fd-wrapper-value (get-fd-current-working-directory))))))

(defmethod invoke-command ((command binary) environment-modifier &rest args)
  (let ((pid (with-subshell
               (funcall environment-modifier)
               (run-binary args)))
        status)

    (debug-log status "PID ~A = ~A" pid args)
    (setf status (nth-value 1 (waitpid pid wuntraced)))
    (debug-log status "EXITED ~A" pid)
    (when (wifstopped status)
      (warn "Stopped jobs should get a job number, but they don't"))

    (make-exit-info :pid pid
                    :exit-status (when (wifexited status)
                                   (wexitstatus status))
                    :exit-signal (when (wifsignaled status)
                                   (wtermsig status))
                    :stop-signal (when (wifstopped status)
                                   (wstopsig status)))))

(define-data special-builtin ()
  ((handler
    :initarg :handler
    :initform (required)
    :updater special-builtin-handler)))

(defmethod documentation ((command special-builtin) (doc-type (eql t)))
  (documentation (special-builtin-handler command) doc-type))

(defun ensure-exit-info (exit-value)
  (etypecase exit-value
    ((integer 0 255)
     (make-exit-info :exit-status exit-value))
    (exit-info
     exit-value)))

(defmethod invoke-command ((command special-builtin) environment-modifier &rest args)
  (with-slots (handler) command
    (funcall environment-modifier)
    (with-fd-streams ()
      (handle-command-errors (first args)
        (ensure-exit-info (apply handler args))))))

(defmethod command-priority ((command special-builtin))
  0)

(defun register-special-builtin (name handler)
  (install-command name (make-instance 'special-builtin :handler handler)))

(define-data builtin ()
  ((handler
    :initarg :handler
    :initform (required)
    :updater builtin-handler)))

(defmethod documentation ((command builtin) (doc-type (eql t)))
  (documentation (builtin-handler command) doc-type))

(defmethod invoke-command ((command builtin) environment-modifier &rest args)
  (with-slots (handler) command
    (funcall environment-modifier)
    (with-fd-streams ()
      (handle-command-errors (first args)
        (ensure-exit-info (apply handler args))))))

(defmethod command-priority ((command builtin))
  1)

(defun register-builtin (name handler)
  (install-command name (make-instance 'builtin :handler handler)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-name (name)
    (check-type name (and (or symbol list) (not null)))
    (etypecase name
      (list
       (destructuring-bind (sym-name str-name) name
         (check-type sym-name (and symbol (not null)))
         (check-type str-name string)
         (values sym-name str-name)))
      (symbol
       (values name (string-downcase (symbol-name name)))))))

(defmacro %define-command (&whole whole class name shell-lambda-list &body body)
  (multiple-value-bind (function-sym string-form) (parse-name name)
    (when (find #\/ string-form)
      (warn 'confusing-command-name :name string-form))
    (let* ((args (gensym "ARGS")))
      (multiple-value-bind (real-body declarations documentation)
          (alexandria:parse-body body :documentation t :whole whole)
        `(progn
           (defun ,function-sym (&rest ,args)
             ,@(when documentation
                 `(,documentation))
             (apply (shell-lambda ,shell-lambda-list ,@declarations ,@real-body) ,args))
           (install-command ,string-form (make-instance ',class :handler #',function-sym))
           ',function-sym)))))

(defmacro define-builtin (name shell-lambda-list &body body)
  "Define a new shell builtin.

`name' should either be a symbol or a list of the
form (`function-name' `builtin-name') where `function-name' is a
symbol and `builtin-name' is a string.  If `name' is simply a symbol,
then the builtin name is the downcased symbol name."
  `(%define-command builtin ,name ,shell-lambda-list
                    ,@body))

(defmacro define-special-builtin (name shell-lambda-list &body body)
  "Define a new special shell builtin.

`name' should either be a symbol or a list of the
form (`function-name' `builtin-name') where `function-name' is a
symbol and `builtin-name' is a string.  If `name' is simply a symbol,
then the builtin name is the downcased symbol name."
  `(%define-command special-builtin ,name ,shell-lambda-list
                    ,@body))

(define-special-builtin (colon ":") (&rest args)
  "Ignores its arguments and exits successfully."
  (declare (ignore args))
  0)

(define-special-builtin (dot ".") (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "Sourcing files"))

(define-special-builtin exec (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "exec"))

(define-condition exit-condition (serious-condition)
  ((exit-info
    :initarg :exit-info
    :reader exit-condition-exit-info
    :documentation
    "The desired exit status"))
  (:documentation
   "A signal representing that the shell would like to exit.

Subshells (e.g. `with-subshell') handle this condition
automatically.")
  (:report
   (lambda (c s)
     (format s "exit ~A" (exit-condition-exit-info c)))))

(defun establish-exit-condition-handler (data continuation)
  (declare (ignore data))
  (handler-case
      (funcall continuation)
    (exit-condition (e)
      (return-from establish-exit-condition-handler
        (exit-condition-exit-info e)))))

(let ((the-void (constantly nil)))
  (extend-shell-environment
   'exit-condition-handler
   the-void
   'establish-exit-condition-handler
   the-void))

(define-special-builtin exit (&optional (exit-status "0"))
  (handler-case
      (setf exit-status (parse-integer exit-status :junk-allowed nil))
    (parse-error (e)
      (declare (ignore e))
      (format *error-output* "exit: ~A: numeric argument required~%" exit-status)
      (setf exit-status 2)))
  (let ((exit-info (ensure-exit-info exit-status)))
    (error 'exit-condition :exit-info exit-info)))

(defun print-escaped (str &optional (output *standard-output*))
  ;; We're supposed to escape the strings so that they can be read
  ;; again, but we don't know what readtable is in use.  We're
  ;; going to assume that its normal-ish.
  (format output "'")
  (loop :for char :across str :do
     (if (equal char #\')
         (format output "'\\''")
         (write-char char output)))
  (format output "'"))

(defun print-environment-assignment-for-binding (key binding &optional (output *standard-output*))
  (let* ((value (environment-binding-value binding)))
    (print-escaped key output)
    (when value
      (format output "=")
      (print-escaped value output)))
  (values))

(define-special-builtin (builtin-export "export") (&flag (print "-p") &rest args)
  (case (length print)
    (0
     (dolist (arg args)
       (multiple-value-bind (var value)
           (deconstruct-environment-assignment-string arg :if-no-assignment nil)
         (when value
           (setf (shcl/core/environment:env var) value))
         (setf (shcl/core/environment:env-exported-p var) t)))
     0)

    (1
     (when args
       (error 'command-error :message "-p argument cannot be followed by positional arguments"))

     (do-environment-bindings (key binding)
       (when (environment-binding-exported-p binding)
         (format *standard-output* "export ")
         (print-environment-assignment-for-binding key binding *standard-output*)
         (format *standard-output* "~%")))
     0)

    (otherwise
     (error 'command-error :message "-p argument cannot be specified multiple times"))))

(define-special-builtin readonly (&flag (print "-p") &rest args)
  (case (length print)
    (0
     (dolist (arg args)
       (multiple-value-bind (var value)
           (deconstruct-environment-assignment-string arg :if-no-assignment nil)
         (when value
           (setf (shcl/core/environment:env var) value))
         (setf (shcl/core/environment:env-readonly-p var) t)))
     0)

    (1
     (when args
       (error 'command-error :message "-p argument cannot be followed by positional arguments"))

     (do-environment-bindings (key binding)
       (when (environment-binding-readonly-p binding)
         (format *standard-output* "readonly ")
         (print-environment-assignment-for-binding key binding *standard-output*)
         (format *standard-output* "~%")))
     0)

    (otherwise
     (error 'command-error :message "-p argument cannot be specified multiple times"))))

(define-special-builtin (builtin-return "return") (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "return"))

(define-special-builtin (builtin-set "set") (&rest args)
  (when args
    (error 'not-implemented :feature "set with more than 0 arguments"))

  ;; POSIX says the output must be sorted
  (let ((bindings (make-extensible-vector)))
    (do-environment-bindings (key binding)
      (vector-push-extend (cons key binding) bindings))
    (setf bindings (sort bindings #'string< :key 'car))
    (loop :for pair :across bindings :do
       (destructuring-bind (key . binding) pair
         (print-environment-assignment-for-binding key binding *standard-output*)
         (format *standard-output* "~%"))))
  0)

(define-special-builtin shift (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "shift"))

(define-special-builtin times (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "times"))

(define-special-builtin trap (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "trap"))

(define-special-builtin unset (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "unset"))
