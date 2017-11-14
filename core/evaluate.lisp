(defpackage :shcl/core/evaluate
  (:use
   :common-lisp :alexandria :bordeaux-threads
   :shcl/core/utility :shcl/core/shell-grammar :shcl/core/lexer :shcl/core/fork-exec
   :shcl/core/thread :shcl/core/expand :shcl/core/environment :shcl/core/builtin
   :shcl/core/posix :shcl/core/posix-types :shcl/core/exit-info :shcl/core/fd-table
   :shcl/core/working-directory :shcl/core/shell-environment :shcl/core/iterator)
  (:shadowing-import-from :alexandria #:when-let #:when-let*)
  (:shadowing-import-from :shcl/core/posix #:pipe)
  (:export #:evaluate))
(in-package :shcl/core/evaluate)

(optimization-settings)

(defparameter *umask*
  (logior s-irusr s-iwusr s-irgrp s-iroth)
  "The umask that should be used when creating new files.")

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

This function implements part of `bind-fd-description' and should not be called
directly."))
(defmethod fd-from-description ((fd integer))
  fd)
(defmethod fd-from-description ((io-file io-file))
  (with-slots (redirect filename) io-file
    (let ((expansion (expansion-for-words (fset:seq filename) :split-fields nil :expand-pathname t)))
      (unless (equal 1 (fset:size expansion))
        (error 'not-implemented :feature "file name expanded to multiple words"))
      (setf expansion (fset:first expansion))
      (fd-autorelease
       (openat-retained (current-working-directory-fd)
                        (coerce expansion 'simple-string)
                        (open-args-for-redirect redirect)
                        *umask*)))))

(defun bind-fd-description (fd description)
  "Bind `fd' to the fd implied by `description'."
  (let ((from-fd (fd-from-description description)))
    (bind-fd fd from-fd)))

(defun separator-par-p (separator)
  "Return non-nil iff the given separator non-terminal describes a
& (par) separator."
  (check-type separator separator)
  (with-slots (separator-op) separator
    (when (slot-boundp separator 'separator-op)
      (typep separator-op 'par))))

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
         (error 'not-implemented :feature "Here-documents"))

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
         (bind-fd-description (fd 0) r))

        (lessand
         (bind-fd-description (fd 0) (get-fd (to-int fd-description))))

        (great
         (bind-fd-description (fd 1) r))

        (greatand
         (bind-fd-description (fd 1) (get-fd (to-int fd-description))))

        (dgreat
         (bind-fd-description (fd 1) r))

        (lessgreat
         (bind-fd-description (fd 0) r))

        (clobber
         (bind-fd-description (fd 1) r))))))

(defmethod handle-redirect ((r io-here) &optional fd-override)
  (declare (ignore fd-override))
  (error 'not-implemented :feature "Here-documents"))

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

(defun evaluate-background-job (sy)
  (declare (ignore sy))
  (error 'not-implemented :feature "Background jobs")
  (truthy-exit-info))

(defun evaluate-synchronous-job (sy)
  "Evaluate the given syntax tree synchronously.

This is a synonym for `evaluate'."
  (evaluate sy))

(defun evaluate-async-job (sy completion-handler)
  "Evaluate the given syntax tree asynchronously.

This function does not create an entry in the job table."
  (let ((shell-environment (preserve-shell-environment)))
    (labels
        ((async-eval ()
           (unwind-protect
                (handler-bind
                    ((error (lambda (e)
                              (return-from async-eval (funcall completion-handler nil e)))))
                  (with-restored-shell-environment shell-environment
                    (destroy-preserved-shell-environment shell-environment)
                    (let* ((result (evaluate sy)))
                      ;; TODO: What if we encounter an error?  We
                      ;; still need to signal completion (one way or
                      ;; the other!)
                      (funcall completion-handler result nil))))
             ;; Just in case we never even made it into the body of
             ;; with-resotred-shell-environment, let's destroy the
             ;; environment again.
             (destroy-preserved-shell-environment shell-environment)
             (debug-log status "Worker thread exit ~A" sy))))
      ;; TODO: What if thread creation errors out?
      (make-thread #'async-eval))))

(defgeneric evaluate (syntax-tree)
  (:documentation
   "This is the main driver for evaluating shell expressions.

It is analogous to `eval' for Common Lisp.

The methods on this function are tightly coupled to the shell grammar."))

(defmethod evaluate (sy)
  (error 'not-implemented :feature (format nil "Cannot eval ~A" (class-name (class-of sy)))))

(defmethod evaluate ((sy complete-command))
  (with-slots (newline-list complete-command command-list) sy
    (cond
      ((slot-boundp sy 'complete-command)
       (return-from evaluate (evaluate-synchronous-job complete-command)))
      ((slot-boundp sy 'command-list)
       (return-from evaluate (evaluate-synchronous-job command-list)))
      (t
       (return-from evaluate (truthy-exit-info))))))

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
             ((and (slot-boundp sy 'and-if) (exit-info-false-p previous-result))
              (falsey-exit-info))
             ((and (slot-boundp sy 'or-if) (exit-info-true-p previous-result))
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
      (return-from evaluate (invert-exit-info result)))))

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
               (bind-fd-description +pipe-read-fd+ read-end))
             (when write-end
               (bind-fd-description +pipe-write-fd+ write-end))
             (evaluate-async-job (aref vector index)
                                 (lambda (result error)
                                   (when error
                                     (debug-log error "Error in pipeline ~A" error))
                                   (store index (or result (internal-error-exit-info))))))))
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
  (with-slots (compound-list) sy
    (with-restored-shell-environment (preserve-shell-environment)
      (return-from evaluate (evaluate-synchronous-job compound-list)))))

(defmethod evaluate ((sy compound-list))
  (with-slots (newline-list term) sy
    (return-from evaluate (evaluate-synchronous-job term))))

(defun evaluate-term (sy)
  (with-slots (and-or separator term-tail) sy
    (let ((result
           (if (separator-par-p separator)
               (evaluate-background-job and-or)
               (evaluate-synchronous-job and-or))))

      (if term-tail
        (return-from evaluate-term (evaluate-synchronous-job term-tail))
        (return-from evaluate-term result)))))

(defmethod evaluate ((sy term))
  (evaluate-term sy))
(defmethod evaluate ((sy term-tail))
  (evaluate-term sy))

(define-condition loop-jump ()
  ((count
    :initarg :count
    :initform 1
    :accessor loop-jump-count)
   (exit-info
    :initarg :exit-info
    :initform (truthy-exit-info)
    :reader loop-jump-exit-info)))

(defun stop-loop-jump (condition)
  (setf (loop-jump-count condition) 0))

(defmacro loop-jump-boundary (&body body)
  `(handler-bind
       ((loop-jump 'stop-loop-jump))
     ,@body))

(define-condition loop-break (loop-jump)
  ())

(define-builtin (builtin-break "break") (args)
  (wrap-errors
    (when (> (fset:size args) 2)
      (error "Invalid arguments"))
    (let ((count (if (equal 1 (fset:size args))
                     1
                     (parse-integer (fset:lookup args 1) :junk-allowed nil))))
      (unless (plusp count)
        (error "Count must be positive"))
      (signal 'loop-break :count count))
    (error "No loops detected")))

(define-condition loop-continue (loop-jump)
  ())

(define-builtin (builtin-continue "continue") (args)
  (wrap-errors
    (when (> (fset:size args) 2)
      (error "Invalid arguments"))
    (let ((count (if (equal 1 (fset:size args))
                     1
                     (parse-integer (fset:lookup args 1) :junk-allowed nil))))
      (unless (plusp count)
        (error "Count must be positive"))
      (signal 'loop-continue :count count))
    (error "No loops detected")))

(defun %shell-while-loop (condition-fn body-fn)
  (let (result)
    (macrolet
        ((%loop-jump-propagate (&body body)
           (let ((err (gensym "ERR")))
             `(lambda (,err)
                (when (plusp (loop-jump-count ,err))
                  (decf (loop-jump-count ,err))
                  (setf result (loop-jump-exit-info ,err))
                  (when (plusp (loop-jump-count ,err))
                    (signal ,err))
                  ,@body)))))
      (loop :while (funcall condition-fn)
         :do
         (block continue
           (restart-case
               (handler-bind
                   ((loop-break
                       (%loop-jump-propagate (return)))
                    (loop-continue
                       (%loop-jump-propagate (return-from continue))))
                 (setf result (funcall body-fn)))
             (break-loop ()
               (return))
             (continue-loop ()
               (return-from continue))))))
    (or result (truthy-exit-info))))

(defmacro shell-while-loop (condition &body body)
  `(%shell-while-loop (lambda () ,condition) (lambda () ,@body)))

(defun wordlist-words (wordlist)
  (let ((result (make-extensible-vector)))
    (labels
        ((handle (x)
           (with-slots (a-word wordlist-tail) x
             (vector-push-extend a-word result)
             (when wordlist-tail
               (handle wordlist-tail)))))
      (handle wordlist)
      result)))

(defmethod evaluate ((sy for-clause))
  (with-slots (name-nt in-nt wordlist sequential-sep do-group) sy
    (let* ((wordlist
            (cond
              ((not (slot-boundp sy 'sequential-sep))
               `#(,(make-instance 'double-quote :parts `#(,(make-instance 'variable-expansion-word :variable "@")))))
              ((slot-boundp sy 'wordlist)
               (wordlist-words wordlist))
              (t
               #())))
           (words (expansion-for-words wordlist :expand-pathname t))
           (name (simple-word-text (slot-value name-nt 'name)))
           (iter (iterator words))
           current-word)
      (shell-while-loop
          (multiple-value-bind (value valid) (next iter)
            (setf current-word value)
            valid)
        (setf (env name) current-word)
        (evaluate-synchronous-job do-group)))))

(defun evaluate-if-clause (sy)
  (check-type sy (or if-clause else-part))
  (with-slots (condition body else-part) sy
    (unless (slot-boundp sy 'condition)
      (return-from evaluate-if-clause (evaluate body)))

    (let ((condition-result (evaluate-synchronous-job condition)))
      (when (exit-info-true-p condition-result)
        (return-from evaluate-if-clause (evaluate-synchronous-job body)))

      (if (slot-boundp sy 'else-part)
          (return-from evaluate-if-clause (evaluate-if-clause else-part))
          (return-from evaluate-if-clause (truthy-exit-info))))))

(defmethod evaluate ((sy if-clause))
  (evaluate-if-clause sy))

(defmethod evaluate ((sy while-clause))
  (with-slots (compound-list do-group) sy
    (shell-while-loop
        (exit-info-true-p (evaluate-synchronous-job compound-list))
      (evaluate-synchronous-job do-group))))

(defmethod evaluate ((sy do-group))
  (with-slots (compound-list) sy
    (return-from evaluate (evaluate-synchronous-job compound-list))))

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
    (let ((expanded (expansion-for-words
                     (list value)
                     :expand-aliases nil
                     :expand-pathname nil
                     :split-fields nil)))
      (unless (equal 1 (fset:size expanded))
        (error 'not-implemented :feature "Variables with multiple fields"))
      (setf (env (simple-word-text name)) (fset:first expanded)))))

(defun evaluate-command-free (assignments redirects)
  "Not all simple-commands have a command!"
  (dolist (assign assignments)
    (evaluate-assignment-word assign))
  (with-fd-scope ()
    (dolist (redirect redirects)
      (handle-redirect redirect)))
  (truthy-exit-info))

(defmethod evaluate ((sy simple-command))
  (with-slots (cmd-prefix cmd-word cmd-name cmd-suffix) sy
    (multiple-value-bind (assignments arguments redirects) (simple-command-parts sy)
      (debug-log status "EXEC: ~A ~A ~A" assignments arguments redirects)
      (when (zerop (length arguments))
        (return-from evaluate (evaluate-command-free assignments redirects)))

      (with-fd-scope ()
        (dolist (r redirects)
          (handle-redirect r))
        (let* ((bindings (simplify-fd-bindings))
               (new-environment (with-environment-scope ()
                                  (dolist (assign assignments)
                                    (evaluate-assignment-word assign))
                                  *environment*))
               pid
               status)
          (setf arguments (with-environment-scope (new-environment)
                            (with-fd-streams ()
                              (expansion-for-words arguments :expand-aliases t :expand-pathname t))))

          (when-let* ((command (fset:first arguments))
                      (builtin (and
                                (not (find #\/ command))
                                (lookup-builtin command))))
            (return-from evaluate
              (funcall builtin arguments)))

          (with-living-fds (fds)
            (setf pid (run arguments
                           :fd-alist (fset:convert 'list bindings)
                           :managed-fds fds
                           :environment (linearized-exported-environment new-environment)
                           :working-directory-fd (current-working-directory-fd)))
            (debug-log status "PID ~A = ~A" pid arguments))
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
                                         (wstopsig status))))))))

(define-condition not-an-exit-info (warning)
  ((actual-type
    :initarg :actual-type
    :accessor not-an-exit-info-actual-type
    :initform (required)
    :type symbol)
   (eval-target
    :initarg :eval-target
    :accessor not-an-exit-info-eval-target
    :initform (required)))
  (:report (lambda (c s) (format s "~A is not an exit info.  Given ~A~%"
                                 (not-an-exit-info-actual-type c) (not-an-exit-info-eval-target c)))))

(defmethod evaluate :around (sy)
  (let ((result (call-next-method)))
    (if (exit-info-p result)
        (setf (env "?") (format nil "~A" (exit-info-code result)))
        (warn 'not-an-exit-info :actual-type (class-name (class-of result)) :eval-target sy))
    result))
