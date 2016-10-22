(defpackage :shcl/evaluate
  (:use :common-lisp :trivial-garbage :alexandria :bordeaux-threads
        :shcl/utility :shcl/shell-grammar :shcl/lexer :shcl/fork-exec
        :shcl/thread :shcl/expand :shcl/environment :shcl/builtin
        :shcl/posix :shcl/posix-types)
  (:shadowing-import-from :alexandria #:when-let #:when-let*)
  (:shadowing-import-from :shcl/posix #:pipe)
  (:export #:evaluate))
(in-package :shcl/evaluate)

(optimization-settings)

(defparameter *umask*
  (logior s-irusr s-iwusr s-irgrp s-iroth)
  "The umask that should be used when creating new files.")

(define-condition not-implemented (warning error)
  ((message
    :initarg :message
    :initform ""
    :accessor not-implemented-message
    :type string))
  (:report (lambda (c s) (format s "NOT-IMPLEMENTED ~A~%" (not-implemented-message c))))
  (:documentation
   "A condition indicating that a feature hasn't been implemented
yet."))

(define-once-global %fd-retain-count-table% (make-hash-table)
  (:documentation
   "This table tracks how many outstanding retains each file
descriptor has."))
(define-once-global %fd-retain-count-table-lock% (make-lock)
  (:documentation
   "This lock protects `%fd-retain-count-table-lock%'."))
(defparameter *autorelease-fd-scope* nil
  "The collection of file descriptors that should be released when the
current fd scope exits.

This is bound by `with-fd-scope'.")

(defun fd-managed-p (fd)
  "Returns t iff the given fd is being managed with retain/release."
  (with-lock-held (%fd-retain-count-table-lock%)
    (not (not (gethash fd %fd-retain-count-table%)))))

(define-condition fd-already-managed (error)
  ((fd
    :initarg :fd
    :initform (required)
    :accessor fd-already-managed-fd))
  (:report (lambda (c s) (format s "Can't enter FD ~A into retain/release management again" (fd-already-managed-fd c)))))

(defun %manage-new-fd (fd)
  "Enter the given fd into the retain/release table."
  (when (gethash fd %fd-retain-count-table%)
    (error 'fd-already-managed :fd fd))
  (setf (gethash fd %fd-retain-count-table%) 1)
  fd)

(define-condition fd-not-managed (error)
  ((fd
    :initarg :fd
    :initform (required)
    :accessor fd-not-managed-fd))
  (:report (lambda (c s) (format s "Can't retain unmanaged FD ~A" (fd-not-managed-fd c)))))

(defun fd-retain (fd)
  "Increment the retain count for the given fd.

The given fd will not be closed in the shell process until the retain
count reaches 0.  See `fd-release'."
  (with-lock-held (%fd-retain-count-table-lock%)
    (unless (gethash fd %fd-retain-count-table%)
      (error 'fd-not-managed :fd fd))
    (incf (gethash fd %fd-retain-count-table%))
    fd))

(define-condition fd-over-release (error)
  ((fd
    :initarg :fd
    :initform (required)
    :accessor fd-over-release-fd))
  (:report (lambda (c s) (format s "FD ~A was over released" (fd-over-release-fd c)))))

(defun %fd-release (fd)
  "Do the work of `fd-release', but assume the table is already
locked."
  (unless (gethash fd %fd-retain-count-table%)
    (error 'fd-over-release :fd fd))
  (let ((count (decf (gethash fd %fd-retain-count-table%))))
    (when (equal 0 count)
      (debug-log 'status "CLOSE ~A" fd)
      (posix-close fd)
      (remhash fd %fd-retain-count-table%)))
  nil)

(defun fd-release (fd)
  "Decrement the retain count for the given fd.

If the retain count reaches 0, then it will be closed immediately."
  (with-lock-held (%fd-retain-count-table-lock%)
    (%fd-release fd)))

(define-condition fd-autorelease-without-scope (error)
  ((fd
    :initarg :fd
    :initform (required)
    :accessor fd-autorelease-without-scope-fd))
  (:report (lambda (c s) (format s "FD ~A was autoreleased without an FD scope in place" (fd-autorelease-without-scope-fd c)))))

(defun fd-autorelease (fd)
  "Release the given fd when the current fd scope exits."
  (unless *autorelease-fd-scope*
    (error 'fd-autorelease-without-scope :fd fd))
  (vector-push-extend fd *autorelease-fd-scope*)
  fd)

(defmacro with-fd-scope (() &body body)
  "Introduce an fd scope.

Calls to `fd-autorelease' while this fd scope is active will not
decrement fd retain counts until control leaves this fd scope.

Additionally, any manipulations to the fd table `*fd-bindings*' will
be reverted when this scope exits."
  (let ((fd (gensym "FD")))
    `(let ((*fd-bindings* (or *fd-bindings* (fset:empty-map)))
           (*autorelease-fd-scope* (make-extensible-vector :element-type 'integer)))
       (unwind-protect (progn ,@body)
         (with-lock-held (%fd-retain-count-table-lock%)
           (loop :for ,fd :across *autorelease-fd-scope* :do
              (%fd-release ,fd)))))))

(defmacro with-living-fds ((fd-list-sym) &body body)
  "Lock the fd table and list all managed file descriptors.

Since this locks the fd table, it is very important to minimize the
amount of work done in the body of this macro.  Ideally, you would
do nothing exept spawn a new process."
  `(with-lock-held (%fd-retain-count-table-lock%)
     (let ((,fd-list-sym (hash-table-keys %fd-retain-count-table%)))
       (declare (dynamic-extent ,fd-list-sym))
       ,@body)))

(defun dup-retained (fd)
  (with-lock-held (%fd-retain-count-table-lock%)
    (let ((new-fd (dup fd)))
      (debug-log 'status "DUP ~A = ~A" new-fd fd)
      (%manage-new-fd new-fd))))

(defun open-retained (pathname flags mode)
  "This is a wrapper around the posix open function which
atomically adds the new fd to the fd table and gives it a +1 retain
count."
  (with-lock-held (%fd-retain-count-table-lock%)
    (let ((fd (posix-open pathname flags mode)))
      (debug-log 'status "OPEN ~A = ~A" fd pathname)
      (%manage-new-fd fd))))

(defun pipe-retained ()
  "This is a wrapper around the posix pipe function which atomically
adds the new fds to the fd table and gives them +1 retain counts.

Returns two values: the read-end of the pipe and the write end of the
pipe."
  (with-lock-held (%fd-retain-count-table-lock%)
    (multiple-value-bind (read-end write-end) (shcl/posix:pipe)
      (debug-log 'status "PIPE ~A -> ~A" write-end read-end)
      (values (%manage-new-fd read-end) (%manage-new-fd write-end)))))

(defmacro with-pipe ((read-end write-end) &body body)
  "Introduce a pipe into the retain table (as if with `pipe-retained')
with a +0 retain count.

That is, you do not need to release the file descriptors produced by
this macro.  You must balance any retains you perform on the given
file descriptors."
  (let ((raw-read-end (gensym "RAW-READ-END"))
        (raw-write-end (gensym "RAW-WRITE-END")))
    `(multiple-value-bind (,raw-read-end ,raw-write-end) (pipe-retained)
       (let ((,read-end (fd-autorelease ,raw-read-end))
             (,write-end (fd-autorelease ,raw-write-end)))
         ,@body))))

(defgeneric open-args-for-redirect (redirect)
  (:documentation
   "Returns the flags that should be passed to the posix open function
for the given redirect."))
(defmethod open-args-for-redirect ((r less))
  (declare (ignore r))
  (logior o-rdonly))
(defmethod open-args-for-redirect ((r great))
  (declare (ignore r))
  (logior o-wronly o-creat o-trunc))
(defmethod open-args-for-redirect ((r dgreat))
  (declare (ignore r))
  (logior o-wronly o-creat o-append))
(defmethod open-args-for-redirect ((r lessgreat))
  (declare (ignore r))
  (logior o-rdwr o-creat))

(defgeneric fd-from-description (description)
  (:documentation
   "Given a description of a place, produce a file descriptor for that place.

This function implements part of `bind-fd' and should not be called
directly."))
(defmethod fd-from-description ((fd integer))
  fd)
(defmethod fd-from-description ((io-file io-file))
  (with-slots (redirect filename) io-file
    (let ((fd (open-retained (coerce (expansion-for-word filename :split-fields nil :expand-pathname t)
                                     'simple-string)
                             (open-args-for-redirect redirect)
                             *umask*)))
      (fd-autorelease fd))))

(defparameter *fd-bindings* nil
  "This variable contains information about how fds should be bound
when a new process is spawned.

This variable should not be accessed directly.  You should add
bindings with `bind-fd' and query bindings with `get-fd'.")

(defun bind-fd (fd description)
  "Extend `*fd-bindings*' to include a binding for `fd' to the thing
implied by `description'.

Note: `description' is interpreted by the `fd-from-description'
generic function.

This function doesn't actually modify the given fd in the current
process.  Instead, it will simply store information about what the
named fd should be bound to in spawned processes.  This may involve
allocating a new fd in this process."
  (unless *fd-bindings*
    (error "Cannot bind without an fd-scope"))
  (let ((from-fd (fd-from-description description)))
    (fset:adjoinf *fd-bindings* fd from-fd)
    (debug-log 'status "BIND ~A = ~A (~A)" fd from-fd *fd-bindings*)))

(defun separator-par-p (separator)
  "Return non-nil iff the given separator non-terminal describes a
& (par) separator."
  (check-type separator separator)
  (with-slots (separator-op) separator
    (when (slot-boundp separator 'separator-op)
      (typep separator-op 'par))))

(define-condition invalid-fd (error)
  ((fd
    :type integer
    :initarg :fd
    :accessor invalid-fd-fd
    :initform (required)))
  (:report (lambda (c s) (format s "Redirect from invalid fd: ~A~%" (invalid-fd-fd c))))
  (:documentation
   "A condition that indicates that an invalid fd has been
requested.

This represents an error in the shell expression.

An fd is considered invalid if it hasn't been bound by the shell
expression."))

(defun get-fd (fd)
  "Return the fd (in this process) will be dup'd into to the given
fd (in a spawned subprocesses).

See `bind-fd'."
  (let ((binding (fset:lookup *fd-bindings* fd)))
    (when binding
      (return-from get-fd binding)))
  (when (fd-managed-p fd)
    (error 'invalid-fd :fd fd))
  (handler-case (fcntl fd f-getfd)
    (syscall-error ()
      (error 'invalid-fd :fd fd)))
  fd)

(defgeneric handle-redirect (redirect &optional fd-override)
  (:documentation
   "Bind fds (as necessary) to actualize the redirect requested."))

(defmethod handle-redirect ((r io-redirect) &optional fd-override)
  (when fd-override
    (error "You did bad.  This shouldn't be set here."))

  (assert (slot-boundp r 'io-number))

  (labels
      ((to-int (io-number)
         (parse-integer (token-value io-number))))
    (with-slots (io-number io-file io-here) r
      (cond
        ((slot-boundp r 'io-here)
         (error 'not-implemented :message "Here-documents are not implemented"))

        ((slot-boundp r 'io-file)
         (handle-redirect io-file (to-int io-number)))

        (t
         (error "Impossible"))))))

(defmethod handle-redirect ((r io-file) &optional fd-override)
  (labels
      ((to-int (filename)
         (let* ((fd-string (simple-word-text filename)))
           (parse-integer fd-string)))
       (fd (default) (or fd-override default)))
    (with-slots (redirect filename fd-description) r
      (etypecase redirect
        (less
         (bind-fd (fd 0) r))

        (lessand
         (bind-fd (fd 0) (get-fd (to-int fd-description))))

        (great
         (bind-fd (fd 1) r))

        (greatand
         (bind-fd (fd 1) (get-fd (to-int fd-description))))

        (dgreat
         (bind-fd (fd 1) r))

        (lessgreat
         (bind-fd (fd 0) r))

        (clobber
         (bind-fd (fd 1) r))))))

(defmethod handle-redirect ((r io-here) &optional fd-override)
  (declare (ignore fd-override))
  (error 'not-implemented :message "Here-documents are not implemented"))

(defmethod handle-redirect ((r redirect-list) &optional fd-override)
  (when fd-override
    (error "You did bad.  This shouldn't be set here."))

  (with-slots (io-redirect redirect-list-tail) r
    (handle-redirect io-redirect)
    (when redirect-list-tail
      (handle-redirect redirect-list-tail))))

(defmethod handle-redirect ((r redirect-list-tail) &optional fd-override)
  (when fd-override
    (error "You did bad.  This shouldn't be set here."))

  (with-slots (io-redirect redirect-list-tail) r
    (handle-redirect io-redirect)
    (when redirect-list-tail
      (handle-redirect redirect-list-tail))))

(defun simplify-fd-bindings-default-new-fd-fn (fd)
  (fd-autorelease (dup-retained fd)))

(defun simplify-fd-bindings (&key (fd-bindings *fd-bindings*) (new-fd-fn #'simplify-fd-bindings-default-new-fd-fn))
  "Transform the given bindings so that there is no overlap between
fds that will be bound in the child process and the source fds in this
process.

The goal is to make it so that we can naively use dup2 to create the
desired state in the child process.  The requirement specified
above (no overlap) is stricter than necessary, but also easier to
implement.

While the return value of this function is safe to bind to
`*fd-bindings*', there is very little reason to do that.

The new-fd-fn argument is only intended to be used for testing."
  ;; An alternative approach would be to identify cycles in the
  ;; dependency graph implied by fd-bindings.  Then, the dependency
  ;; cycles could be broken using dup.
  (debug-log 'status "SIMPLIFY ~A" fd-bindings)
  (let* ((ours (fset:empty-set))
         (theirs (fset:empty-set))
         (conflict (fset:empty-set)))
    (fset:do-map (key value fd-bindings)
      (fset:adjoinf theirs key)
      (fset:adjoinf ours value))
    (setf conflict (fset:intersection ours theirs))
    (when (zerop (fset:size conflict))
      (debug-log 'status "SIMPLIFIED")
      (return-from simplify-fd-bindings fd-bindings))

    (let ((translation (make-hash-table)))
      (fset:do-set (conflicted-fd conflict)
        (let (new-fd)
          (loop :do (setf new-fd (funcall new-fd-fn))
             :while (fset:lookup theirs new-fd))
          (setf (gethash conflicted-fd translation) new-fd)))
      (fset:do-map (key value fd-bindings)
        (let ((new-value (gethash value translation)))
          (when new-value
            (fset:adjoinf fd-bindings key new-value))))
      (debug-log 'status "SIMPLIFIED ~A" fd-bindings)
      fd-bindings)))

(defun evaluate-background-job (sy)
  (declare (ignore sy))
  (error 'not-implemented :message "Background jobs aren't implemented")
  (truthy-exit-status))

(defun evaluate-synchronous-job (sy)
  "Evaluate the given syntax tree synchronously.

This is a synonym for `evaluate'."
  (evaluate sy))

(defparameter *special-variables-to-preserve-during-async*
  '(*environment*)
  "The values of these variables will be preserved when switching
threads to evaluate a syntax tree asynchronously (both for
`evaluate-async-job' and `evaluate-background-job').")

(defun evaluate-async-job (sy completion-handler)
  "Evaluate the given syntax tree asynchronously.

This function does not create an entry in the job table."
  (let* ((symbols *special-variables-to-preserve-during-async*)
         (symbol-values (mapcar #'symbol-value symbols))
         (fd-bindings *fd-bindings*)
         (fds-to-retain (fset:empty-set)))
    (fset:do-map (theirs ours fd-bindings)
      (declare (ignore theirs))
      (fset:adjoinf fds-to-retain ours))

    (labels
        ((async-eval ()
           (progv symbols symbol-values
             (with-fd-scope ()
               (setf *fd-bindings* fd-bindings)
               (fset:do-set (fd fds-to-retain)
                 (fd-autorelease fd))

               (let* ((result (evaluate sy)))
                 (funcall completion-handler result))))
           (debug-log 'status "Thread exit ~A" sy)))
      (fset:do-set (fd fds-to-retain)
        (fd-retain fd))
      (make-thread #'async-eval))))

(defun exit-true-p (exit-thing)
  "Given an exit code, return t iff the program exited successfully."
  (zerop exit-thing))

(defun exit-false-p (exit-thing)
  "Given an exit code, return t iff the program didn't exit
sucesfully."
  (not (exit-true-p exit-thing)))

(defun invert-exit-status (exit-thing)
  "Given an exit code, produce a similar code that indicates failure."
  (if (exit-true-p exit-thing)
      1
      0))

(defun truthy-exit-status ()
  "Produce an exit code that indicates success."
  0)

(defun falsey-exit-status ()
  "Produce an exit code that indicates failure."
  1)

(defun exit-status (&key pid exit-code exit-signal stop-signal)
  "Produce an exit code that incorperates the given information."
  (declare (ignore pid))
  (+ (if exit-code exit-code 0) (if exit-signal 128 0) (if stop-signal 128 0)))

(defgeneric evaluate (syntax-tree)
  (:documentation
   "This is the main driver for evaluating shell expressions.

It is analogous to `eval' for Common Lisp.

The methods on this function are tightly coupled to the shell grammar."))

(defmethod evaluate (sy)
  (error 'not-implemented :message (format nil "Cannot eval ~A" (class-name (class-of sy)))))

(defmethod evaluate ((sy complete-command))
  (with-slots (newline-list complete-command) sy
    (if (slot-boundp sy 'complete-command)
        (return-from evaluate (evaluate-synchronous-job complete-command))
        (return-from evaluate (truthy-exit-status)))))

(defun evaluate-command-list (sy)
  (with-slots (and-or separator-op command-list-tail) sy
    (let ((no-wait (typep separator-op 'par)))

      (unless command-list-tail
        (if no-wait
            (return-from evaluate-command-list (evaluate-background-job and-or))
            (return-from evaluate-command-list (evaluate-synchronous-job and-or))))

      (if no-wait
          (evaluate-background-job sy)
          (evaluate-synchronous-job and-or))

      (return-from evaluate-command-list (evaluate-synchronous-job command-list-tail)))))

(defmethod evaluate ((sy command-list))
  (evaluate-command-list sy))
(defmethod evaluate ((sy command-list-tail))
  (evaluate-command-list sy))

(defun evaluate-and-or (previous-result sy)
  (unless sy
    (return-from evaluate-and-or previous-result))

  (with-slots (pipeline and-or-tail) sy
    (let ((result
           (cond
             ((and (slot-boundp sy 'and-if) (exit-false-p previous-result))
              (falsey-exit-status))
             ((and (slot-boundp sy 'or-if) (exit-true-p previous-result))
              previous-result)
             (t
              (evaluate-synchronous-job pipeline)))))

      (evaluate-and-or result and-or-tail))))

(defmethod evaluate ((sy and-or))
  (with-slots (pipeline and-or-tail) sy
    (let ((result (evaluate-synchronous-job pipeline)))
      (evaluate-and-or result and-or-tail))))

(defmethod evaluate ((sy pipeline))
  (with-slots (bang pipe-sequence) sy
    (let ((result (evaluate-synchronous-job pipe-sequence)))
      (return-from evaluate (invert-exit-status result)))))

(defconstant +pipe-read-fd+ 0)
(defconstant +pipe-write-fd+ 1)

(defun evaluate-pipe-sequence (sy)
  (let ((vector (make-extensible-vector))
        (results (make-extensible-vector))
        (semaphore (make-semaphore))
        write-fd)
    (labels
        ((visit (thing)
           (with-slots (command pipe-sequence-tail) thing
             (vector-push-extend command vector)
             (vector-push-extend nil results)
             (when pipe-sequence-tail
               (visit pipe-sequence-tail))))
         (store (index thing)
           (setf (aref results index) thing)
           (semaphore-signal semaphore))
         (run-command (index read-end write-end)
           (with-fd-scope ()
             (when read-end
               (bind-fd +pipe-read-fd+ read-end))
             (when write-end
               (bind-fd +pipe-write-fd+ write-end))
             (evaluate-async-job (aref vector index)
                                 (lambda (result) (store index result))))))
      ;; Produce a vector containing all the elements of the pipeline
      (visit sy)
      (assert (< 1 (length vector)))

      ;; Run each command in the pipeline
      (loop :for index :from (- (length vector) 1) :downto 1 :do
         (multiple-value-bind (read-end write-end) (pipe-retained)
           (run-command index read-end write-fd)
           (when write-fd
             (fd-release write-fd))
           (setf write-fd write-end)
           (fd-release read-end)))

      ;; Run the very first command
      (assert write-fd)
      (run-command 0 nil write-fd)
      (fd-release write-fd)

      ;; And wait
      (loop :for n :below (length vector) :do
         (semaphore-wait semaphore))

      (return-from evaluate-pipe-sequence (aref results (- (length results) 1))))))

(defmethod evaluate ((sy pipe-sequence))
  (with-slots (command pipe-sequence-tail) sy
    (unless pipe-sequence-tail
      (return-from evaluate (evaluate-synchronous-job command)))

    (return-from evaluate (evaluate-pipe-sequence sy))))

(defmethod evaluate ((sy command))
  (with-slots (compound-command redirect-list) sy
    (with-fd-scope ()
      (handle-redirect redirect-list)
      (return-from evaluate (evaluate-synchronous-job compound-command)))))

(defmethod evaluate ((sy subshell))
  (declare (ignore sy))
  (error 'not-implemented :message "Subshells not implemented"))

(defmethod evaluate ((sy compound-list))
  (with-slots (newline-list term) sy
    (return-from evaluate (evaluate-synchronous-job term))))

(defun evaluate-term (sy)
  (with-slots (and-or separator term-tail) sy
    (when (separator-par-p separator)
      (evaluate-background-job and-or))

    (if term-tail
        (return-from evaluate-term (evaluate-synchronous-job term-tail))
        (return-from evaluate-term (truthy-exit-status)))))

(defmethod evaluate ((sy term))
  (evaluate-term sy))
(defmethod evaluate ((sy term-tail))
  (evaluate-term sy))

(defun cmd-prefix-parts (prefix)
  "Given a cmd-prefix, separate it into the 2 things it
describes (variable assignments and io redirects)."
  (with-slots (io-redirect assignment-word cmd-prefix-tail) prefix
    (multiple-value-bind (assignments redirects)
        (when cmd-prefix-tail
          (cmd-prefix-parts cmd-prefix-tail))

      (when (slot-boundp prefix 'io-redirect)
        (push io-redirect redirects))

      (when (slot-boundp prefix 'assignment-word)
        (push assignment-word assignments))

      (values assignments redirects))))

(defun cmd-suffix-parts (suffix)
  "Given a cmd-suffix, separate it into the things id
describes (command arguments and io redirects)."
  (with-slots (io-redirect a-word cmd-suffix-tail) suffix
    (multiple-value-bind (arguments redirects)
        (when cmd-suffix-tail
          (cmd-suffix-parts cmd-suffix-tail))

      (when (slot-boundp suffix 'io-redirect)
        (push io-redirect redirects))

      (when (slot-boundp suffix 'a-word)
        (push a-word arguments))

      (values arguments redirects))))

(defun simple-command-parts (sy)
  "Given a simple-command, extract the assignments, command arguments,
and io redirects."
  (let (assignments
        arguments
        redirects)
    (with-slots (cmd-prefix cmd-word cmd-name cmd-suffix) sy
      (when (slot-boundp sy 'cmd-prefix)
        (multiple-value-bind (prefix-assignments prefix-redirects) (cmd-prefix-parts cmd-prefix)
          (dolist (a prefix-assignments)
            (push a assignments))
          (dolist (r prefix-redirects)
            (push r redirects))))

      (when (slot-boundp sy 'cmd-name)
        (push cmd-name arguments))

      (when (slot-boundp sy 'cmd-word)
        (push cmd-word arguments))

      (when (slot-boundp sy 'cmd-suffix)
        (multiple-value-bind (suffix-arguments suffix-redirects) (cmd-suffix-parts cmd-suffix)
          (dolist (a suffix-arguments)
            (push a arguments))
          (dolist (r suffix-redirects)
            (push r redirects))))

      (values (nreverse assignments) (nreverse arguments) (nreverse redirects)))))

(defun evaluate-assignment-word (assignment-word)
  "Modify the environment to include the given variable assignment."
  (with-accessors ((value assignment-word-value-word) (name assignment-word-name)) assignment-word
    (let ((expanded (expansion-for-word
                     value
                     :expand-aliases nil
                     :expand-pathname nil
                     :split-fields nil)))
      (setf (env (simple-word-text name)) expanded))))

(defun evaluate-command-free (assignments redirects)
  "Not all simple-commands have a command!"
  (dolist (assign assignments)
    (evaluate-assignment-word assign))
  (with-fd-scope ()
    (dolist (redirect redirects)
      (handle-redirect redirect)))
  (truthy-exit-status))

(defmethod evaluate ((sy simple-command))
  (with-slots (cmd-prefix cmd-word cmd-name cmd-suffix) sy
    (multiple-value-bind (assignments arguments redirects) (simple-command-parts sy)
      (debug-log 'status "EXEC: ~A ~A ~A~%" assignments arguments redirects)
      (when (zerop (length arguments))
        (return-from evaluate (evaluate-command-free assignments redirects)))

      (with-environment-scope ()
        (dolist (assign assignments)
          (evaluate-assignment-word assign))
        (setf arguments (expansion-for-words arguments :expand-aliases t :expand-pathname t))
        (with-fd-scope ()
          (dolist (r redirects)
            (handle-redirect r))
          (let* ((bindings (simplify-fd-bindings))
                 pid
                 status)
            (when-let ((builtin (lookup-builtin (fset:first arguments))))
              (return-from evaluate
                (exit-status :exit-code (funcall builtin arguments))))

            (with-living-fds (fds)
              (setf pid (run arguments
                             :fd-alist (fset:convert 'list bindings)
                             :managed-fds fds
                             :environment (linearized-exported-environment)))
              (debug-log 'status "PID ~A = ~A" pid arguments))
            (setf status (nth-value 1 (waitpid pid wuntraced)))
            (debug-log 'status "EXITED ~A" pid)
            (when (wifstopped status)
              (warn "Stopped jobs should get a job number, but they don't"))

            (exit-status :pid pid
                         :exit-code (when (wifexited status)
                                      (wexitstatus status))
                         :exit-signal (when (wifsignaled status)
                                        (wtermsig status))
                         :stop-signal (when (wifstopped status)
                                        (wstopsig status)))))))))

(define-condition not-an-exit-code (warning)
  ((actual-type
    :initarg :actual-type
    :accessor not-an-exit-code-actual-type
    :initform (required)
    :type symbol)
   (eval-target
    :initarg :eval-target
    :accessor not-an-exit-code-eval-target
    :initform (required)))
  (:report (lambda (c s) (format s "~A is not an exit code.  Given ~A~%"
                                 (not-an-exit-code-actual-type c) (not-an-exit-code-eval-target c)))))

(defmethod evaluate :around (sy)
  (let ((result (call-next-method)))
    (unless (typep result 'integer)
      (warn 'not-an-exit-code :actual-type (class-name (class-of result)) :eval-target sy))
    result))

(defmethod evaluate ((s string))
  (do-iterator (command (command-iterator (token-iterator (make-string-input-stream s))))
    (evaluate command)))
