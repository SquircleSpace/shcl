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
  (:import-from :shcl/core/fd-table
                #:with-fd-streams #:with-living-fds #:simplify-fd-bindings)
  (:import-from :shcl/core/exit-info #:exit-info #:make-exit-info)
  (:import-from :shcl/core/data #:define-data)
  (:import-from :shcl/core/shell-environment #:preserve-special-variable #:with-subshell)
  (:import-from :shcl/core/support
                #:wifexited #:wexitstatus #:wifsignaled #:wtermsig #:wifstopped
                #:wstopsig)
  (:import-from :shcl/core/posix-types #:wuntraced)
  (:import-from :shcl/core/posix #:waitpid)
  (:import-from :shcl/core/environment #:*environment* #:linearized-exported-environment)
  (:import-from :shcl/core/working-directory #:current-working-directory-fd)
  (:import-from :shcl/core/fork-exec #:run)
  (:export
   #:define-builtin #:define-special-builtin #:lookup-command
   #:command-error #:wrap-errors #:invoke-command #:shell-lambda
   #:with-parsed-arguments #:handle-command-errors
   #:*command-namespace* #:make-command-namespace #:command-namespace-table
   #:command-namespace-fallback #:special-builtin #:builtin #:binary
   #:invoke-command #:install-command #:command-priority
   #:redefining-command #:confusing-command-name)
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

(define-condition command-error (error)
  ((message
    :initarg :message
    :initform nil
    :reader command-error-message
    :type (or string null)
    :documentation
    "A string describing the error that occured.")
   (error
    :initarg :error
    :initform nil
    :reader command-error-error
    :type (or error null)
    :documentation
    "Any `error'.")
   (return-code
    :initarg :return-code
    :initform 1
    :type (integer 0 255)
    :reader command-error-return-code
    :documentation
    "The desired return code for the command."))
  (:report
   (lambda (c s)
     (format s "~A"
             (or (command-error-message c)
                 (command-error-error c)
                 "Unknown error"))))
  (:documentation
   "A condition class that represents a failure while executing a
shell command implemented in lisp.

If you want to signal a pre-existing error, pass it in as the `error'
argument.  If you instead want to signal a simple error, pass in a
string message "))

(defmacro handle-command-errors (command-name &body body)
  "A convenience macro that handles `command-error' signals in a sane way.

The body is evaluted with a signal handler installed for
`command-error'.  Any `command-error's handled by this macro will be
printed to stderr along with the provided command name.  This macro
will then evaluate to the return code of the error.  See
`command-error-return-code'."
  (let ((name (gensym "NAME"))
        (e (gensym "E")))
    `(let ((,name ,command-name))
       (handler-case
           (progn ,@body)
         (command-error (,e)
                        (format *error-output* "~A: ~A~%" ,name ,e)
                        (ensure-exit-info (command-error-return-code ,e)))))))

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
  (let ((bindings (fset:convert 'list (simplify-fd-bindings))))
    (with-living-fds (fds)
      (run args
           :fd-alist bindings
           :managed-fds fds
           :environment (linearized-exported-environment *environment*)
           :working-directory-fd (current-working-directory-fd)))))

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
    (handle-command-errors (first args)
      (ensure-exit-info (apply handler args)))))

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

(defun wrap-error (error)
  "If the given error isn't already a `command-error', wrap it with
one and resignal."
  (unless (typep error 'command-error)
    (error 'command-error :error error)))

(defmacro wrap-errors (&body body)
  "All errors that would normally escape this form are wrapped in a
`command-error' first."
  `(handler-bind
       ((error 'wrap-error))
     ,@body))

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
       (values name (string-downcase (symbol-name name))))))

  (define-data shell-lambda-list ()
    ((whole
      :initarg :whole
      :initform nil
      :reader shell-lambda-list-whole)
     (argv0
      :initarg :argv0
      :initform nil
      :reader shell-lambda-list-argv0)
     (flags
      :initarg :flags
      :initform #()
      :reader shell-lambda-list-flags)
     (options
      :initarg :options
      :initform #()
      :reader shell-lambda-list-options)
     (required-args
      :initarg :required-args
      :initform #()
      :reader shell-lambda-list-required-args)
     (optional-args
      :initarg :optional-args
      :initform #()
      :reader shell-lambda-list-optional-args)
     (rest
      :initarg :rest
      :initform nil
      :reader shell-lamnbda-list-rest)))

  (defun shell-lambda-list-variables (sll)
    (with-slots (whole argv0 flags options required-args optional-args rest)
        sll
      (let (variables)
        (when whole
          (push whole variables))
        (when argv0
          (push argv0 variables))
        (loop :for spec :across flags :do
           (push (aref spec 0) variables))
        (loop :for spec :across options :do
           (push (aref spec 0) variables))
        (loop :for var :across required-args :do
           (push var variables))
        (loop :for spec :across optional-args :do
           (push (car spec) variables))
        (when rest
          (push rest variables))
        variables)))

  (defun parse-shell-lambda-list (shell-lambda-list)
    (let (state
          edge-table
          whole
          argv0
          (flag (make-extensible-vector))
          (option (make-extensible-vector))
          (required (make-extensible-vector))
          (optional (make-extensible-vector))
          rest
          (unique-variable-table (make-hash-table))
          (unique-flag-table (make-hash-table :test 'equal)))
      (labels
          ((edge-table-for-state (state)
             (loop :for entry :on edge-table :do
                (when (eq (cdr (car entry)) state)
                  (return-from edge-table-for-state (cdr entry)))))
           (jump (arg)
             (let ((entry (when (symbolp arg)
                            (assoc (symbol-name arg) edge-table :test 'equal))))
               (when entry
                 (setf state (cdr entry))
                 (setf edge-table (edge-table-for-state state))
                 t)))
           (&-p (arg)
             (and (symbolp arg)
                  (not (zerop (length (symbol-name arg))))
                  (equal #\& (aref (symbol-name arg) 0))))
           (valid-flag-sigil-p (arg assume-dash-prefix)
             (and (stringp arg)
                  (find-if (lambda (c) (not (equal #\- c))) arg)
                  (or assume-dash-prefix
                      (equal #\- (aref arg 0)))))
           (ensure-unique-variable (variable)
             (when (gethash variable unique-variable-table)
               (error "Variable ~A appears twice in shell lambda list" variable))
             (setf (gethash variable unique-variable-table) t)
             variable)
           (ensure-unique-flag (flag)
             (when (gethash flag unique-flag-table)
               (error "Flag ~A appears twice in shell lambda list" flag))
             (setf (gethash flag unique-flag-table) t)
             flag)
           (start (arg)
             (error "A shell lambda list must start with an & argument, got: ~A" arg))
           (&whole (arg)
             (cond
               (whole
                (error "&whole may only be specified once"))
               ((and (symbolp arg) arg)
                (setf whole (ensure-unique-variable arg)))
               (t
                (error "Malformed &whole argument, got: ~A" arg))))
           (&argv0 (arg)
             (cond
               (argv0
                (error "argv0 can only be specified once"))
               ((null arg)
                (error "nil cannot be used for argv0"))
               ((symbolp arg)
                (setf argv0 (ensure-unique-variable arg)))
               (t
                (error "argv0 must be a symbol, got: ~A" arg))))
           (&flag (arg)
             (cond
               ((and (symbolp arg)
                     (valid-flag-sigil-p (symbol-name arg) t))
                (vector-push-extend
                 (vector (ensure-unique-variable arg)
                         (ensure-unique-flag (concatenate 'string "--" (string-downcase (symbol-name arg)))))
                 flag))
               ((consp arg)
                (destructuring-bind (variable &rest flag-strings) arg
                  (check-type variable symbol)
                  (ensure-unique-variable variable)
                  (unless flag-strings
                    (error "At least one flag string must be provided"))
                  (dolist (flag-string flag-strings)
                    (unless (valid-flag-sigil-p flag-string nil)
                      (error "Flag argument sigils must be a string, start with -, and contain a character other than -"))
                    (ensure-unique-flag flag-string))
                  (vector-push-extend (coerce arg 'vector) flag)))
               (t
                (error "Malformed flag argument, got: ~A" arg))))
           (&option (arg)
             (cond
               ((and (symbolp arg)
                     (valid-flag-sigil-p (symbol-name arg) t))
                (vector-push-extend
                 (vector (ensure-unique-variable arg)
                         (ensure-unique-flag (concatenate 'string "--" (string-downcase (symbol-name arg)))))
                 option))
               ((consp arg)
                (destructuring-bind (variable &rest option-strings) arg
                  (check-type variable symbol)
                  (ensure-unique-variable variable)
                  (unless option-strings
                    (error "At least one option string must be provided"))
                  (dolist (option-string option-strings)
                    (unless (valid-flag-sigil-p option-string nil)
                      (error "Option argument sigils must be a string, start with -, and contain a character other than -"))
                    (ensure-unique-flag option-string))
                  (vector-push-extend (coerce arg 'vector) option)))
               (t
                (error "Malformed option argument, got: ~A" arg))))
           (&required (arg)
             (cond
               ((symbolp arg)
                (vector-push-extend (ensure-unique-variable arg) required))
               (t
                (error "Malformed required argument, got: ~A" arg))))
           (&optional (arg)
             (cond
               ((symbolp arg)
                (vector-push-extend (cons arg nil) optional))
               ((consp arg)
                (destructuring-bind (variable default-value) arg
                  (check-type variable symbol)
                  (vector-push-extend (cons (ensure-unique-variable variable) default-value) optional)))
               (t
                (error "Malformed optional argument, got: ~A" arg))))
           (&rest (arg)
             (cond
               (rest
                (error "rest can only be specified once"))
               ((and (symbolp arg) arg)
                (setf rest (ensure-unique-variable arg)))
               (t
                (error "Malformed rest argument, got: ~A" arg)))))
        (setf edge-table `(("&WHOLE" . ,#'&whole)
                           ("&ARGV0" . ,#'&argv0)
                           ("&FLAG" . ,#'&flag)
                           ("&OPTION" . ,#'&option)
                           ("&REQUIRED" . ,#'&required)
                           ("&OPTIONAL" . ,#'&optional)
                           ("&REST" . ,#'&rest)))
        (setf state #'start)
        (loop :while shell-lambda-list :do
           (let* ((arg (pop shell-lambda-list)))
             (cond
               ((&-p arg)
                (unless (jump arg)
                  (error "Unsupported & argument: ~A" arg)))

               (t
                (funcall state arg)))))

        (make-instance 'shell-lambda-list :whole whole :argv0 argv0 :flags flag
                       :options option :required-args required
                       :optional-args optional :rest rest)))))

(defun unparse-shell-lambda-list (sll)
  (with-slots (whole argv0 flags options required-args optional-args rest) sll
    (let (ll)
      (when whole
        (push '&whole ll)
        (push whole ll))
      (when argv0
        (push '&argv0 ll)
        (push argv0 ll))
      (unless (zerop (length flags))
        (push '&flag ll)
        (loop :for flag-spec :across flags :do
           (push (coerce flag-spec 'list) ll)))
      (unless (zerop (length options))
        (push '&option ll)
        (loop :for option-spec :across options :do
           (push (coerce option-spec 'list) ll)))
      (unless (zerop (length required-args))
        (push '&required ll)
        (loop :for arg :across required-args :do
           (push arg ll)))
      (unless (zerop (length optional-args))
        (push '&optional ll)
        (loop :for arg :across optional-args :do
           (push arg ll)))
      (when rest
        (push '&rest ll)
        (push rest ll))
      (nreverse ll))))

(defmethod print-object ((sll shell-lambda-list) stream)
  (print-unreadable-object (sll stream :type t)
    (format stream "~S" (unparse-shell-lambda-list sll))))

(defun parse-flags-and-options (flag-table option-table args)
  (loop :while args :do
     (let* ((next-arg (car args))
            (valid-option (and (not (zerop (length next-arg)))
                               (equal #\- (aref next-arg 0))))
            mini-arg
            setter)
       (cond
         ((equal "--" next-arg)
          (pop args)
          (return))

         ;; --flag
         ((and valid-option
               (setf setter (gethash next-arg flag-table)))
          (pop args)
          (funcall setter next-arg))

         ;; --option val
         ((and valid-option
               (setf setter (gethash next-arg option-table)))
          (pop args)
          (let ((option-arg (pop args)))
            (unless option-arg
              (error 'command-error :message (format nil "Option ~A requires an argument" next-arg)))
            (funcall setter option-arg)))

         ;; -PACK [maybe-opt-val]
         ((and (< 2 (length next-arg))
               (setf setter (gethash (setf mini-arg (subseq next-arg 0 2))
                                     flag-table)))
          (pop args)
          (funcall setter mini-arg)
          (loop :for index :from 2 :below (length next-arg) :do
             (let* ((mini-arg (format nil "-~C" (aref next-arg index)))
                    (flag-setter (gethash mini-arg flag-table))
                    (option-setter (when (equal index (1- (length next-arg)))
                                     (gethash mini-arg option-table))))
               (cond
                 (flag-setter
                  (funcall flag-setter mini-arg))
                 (option-setter
                  (let ((arg (pop args)))
                    (unless arg
                      (error 'command-error :message (format nil "Option ~A requires an argument" mini-arg)))
                    (funcall option-setter arg)))
                 (t
                  (error 'command-error "Unrecognized flag ~A in option pack ~A" mini-arg next-arg))))))

         ;; -Oval
         ((and (< 2 (length next-arg))
               (setf setter (gethash (setf mini-arg (subseq next-arg 0 2))
                                     option-table)))
          (pop args)
          (funcall setter (subseq next-arg 2)))

         (t
          (return)))))
  args)

(defmacro shell-lambda (shell-lambda-list &body body)
  "`lambda' with shell argument processing.

This macro produces a lambda form which parses the arguments it
receives according to the specification given in `shell-lambda-list'
and then evaluates `body'.

Like a normal lambda list, there are several type of arguments
supported (e.g. `&optional' and `&rest').  Unlike a normal lambda
list, the first argument isn't assumed to be a required argument.  If
your shell lambda list is non-empty, it must start with an argument
type specifier.

The following argument type specifiers are supported.  You can provide
any subset of them, but you must provide them in the order listed
below.  Each argument type specifier may appear only once in the shell
lambda list.

&whole [var]
Binds var to the full list of arguments received.  Note that the first
element of the list is the command name.  You must not mutate the list.

&argv0 [var]
Binds var to the first argument received (i.e. the command name).

&flag {(var sigil-str*) | var}*
&option {(var sigil-str*) | var}*
Flags and options model the common shell convetion of --arguments.
The difference between a flag and an option is how many parameters
they consume.  A flag consumes no command arguments.  An option
consumes one command argument.  Although you must place flags before
options in your shell lambda list, flags and options can be intermixed
when the lambda is invoked.

If you do not provide a sigil-str, it will default to the lower-case
name of the variable symbol.  If you provide multiple sigil-str
values, all of them will be associated with the flag/option.  This
allows you to achieve the common pattern of having long
names (e.g. --long-name) and short names (e.g. -L) for a single flag.
You could also use this to model flags that are mutually
exclusive (e.g. --foobar vs --no-foobar).

With &flag, var will be bound to an array containing the sigils that
were found.  So, if a single flag is provided more than once, the
array will have multiple elements.  With &option, var will be bound to
an array containing the values provided for that option.

&required {var}*
Vars listed in the required section correspond to positional arguments
that must be provided by the caller.  After proccessing argv0, flags,
and options, the required section works just like required arguments
in a normal lambda list.

&optional {(var default-form) | var}*
The optional section behaves just like the optional section of a
normal lambda list.

&rest [var]
The rest section behaves just like the rest section of a normal lambda
list."
  ;; I went back and forth on whether &flag arguments should be
  ;; array-valued or list-valued.  I ultimately decided that it was
  ;; better to default to the option with better performance.  Unlike
  ;; &whole and &rest, &flag and &option don't have analogs in normal
  ;; lambda lists and so there's no convention to follow.

  ;; The end result is that we use lists for some arguments and arrays
  ;; for others.  I bet this is going to cause some people a great
  ;; deal of annoyance...
  (let* ((sll (parse-shell-lambda-list shell-lambda-list))
         (args (gensym "ARGS"))
         (arg (gensym "ARG"))
         (option-table (gensym "OPTION-TABLE"))
         (flag-table (gensym "FLAG-TABLE"))
         (option-setter (gensym "OPTION-SETTER"))
         (flag-setter (gensym "FLAG-SETTER"))
         (variables (shell-lambda-list-variables sll)))
    (with-slots (whole argv0 flags options required-args optional-args rest)
        sll
      `(lambda (&rest ,args)
         (let ,variables
           ,@(when whole
               `((setf ,whole ,args)))
           (let ((,arg (pop ,args)))
             (unless ,arg
               (error 'command-error :message "All shell commands must receive the command name as an argument"))
             ,@(when argv0
                 `((setf ,argv0 ,arg))))
           ,@(unless (and (zerop (length flags))
                          (zerop (length options)))
               `((let ((,option-table (make-hash-table :test 'equal))
                       (,flag-table (make-hash-table :test 'equal)))
                   ,@(loop :for spec :across options
                        :for var = (aref spec 0)
                        :for sigils = (subseq spec 1) :collect
                        `(labels
                             ((,option-setter (,arg)
                                (vector-push-extend ,arg ,var)))
                           (setf ,var (make-extensible-vector))
                           (loop :for ,arg :across ,sigils :do
                              (setf (gethash ,arg ,option-table) #',option-setter))))
                   ,@(loop :for spec :across flags
                        :for var = (aref spec 0)
                        :for sigils = (subseq spec 1) :collect
                        `(labels
                             ((,flag-setter (,arg)
                                (vector-push-extend ,arg ,var)))
                           (setf ,var (make-extensible-vector))
                           (loop :for ,arg :across ,sigils :do
                              (setf (gethash ,arg ,flag-table) #',flag-setter))))
                   (setf ,args (parse-flags-and-options ,flag-table ,option-table ,args)))))
           ,@(loop :for var :across required-args :collect
                `(let ((,arg (pop ,args)))
                   (unless ,arg
                     (error 'command-error :message (format nil "Required argument ~A is missing" ',var)))
                   (setf ,var ,arg)))
           ,@(loop :for spec :across optional-args :collect
                `(let ((,arg (pop ,args)))
                   (setf ,(car spec) (if ,arg ,arg ,(cdr spec)))))
           ,(if rest
                `(setf ,rest ,args)
                `(when ,args
                   (error 'command-error :message "Too many arguments provided")))
           (let ,(mapcar (lambda (var) (list var var)) variables)
             ,@body))))))

(defmacro with-parsed-arguments (shell-lambda-list args &body body)
  "Parse the given arguments according to the given shell lambda list.

See `shell-lambda' for documentation on what a shell lambda list
consists of.

This macro is equivalent to creating a shell lambda and then invoking
it with the given args."
  `(apply (shell-lambda ,shell-lambda-list
            ,@body)
          ,args))

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
           (install-command ,string-form (make-instance ',class :handler #',function-sym)))))))

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

(define-special-builtin exit (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "exit"))

(define-special-builtin (builtin-export "export") (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "export"))

(define-special-builtin readonly (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "readonly"))

(define-special-builtin (builtin-return "return") (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "return"))

(define-special-builtin (builtin-set "set") (&rest args)
  (declare (ignore args))
  (error 'not-implemented :feature "set"))

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
