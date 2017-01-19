(defpackage :shcl/core/working-directory
  (:use :common-lisp :bordeaux-threads :shcl/core/utility
        :shcl/core/fd-table :shcl/core/posix-types :shcl/core/posix
        :shcl/core/support :shcl/core/shell-environment)
  (:export #:directory-p #:path-invalid #:path-invalid-message #:cd
           #:push-working-directory #:pop-working-directory
           #:current-working-directory-fd))
(in-package :shcl/core/working-directory)

(defun process-working-directory-retained ()
  "Return a retained fd representing the current working directory of
the process."
  (open-retained "." O-RDONLY))

(defvar *working-directory-fds* nil
  "The current stack of working directories")
(defvar *working-directory-lock* (make-lock)
  "The lock protecting access to `*working-directory-fds*'.")
(defun initialize-working-directory-fds ()
  "Set `*working-directory-fds*' to its initial value.

This should not be called directly.  It is intended to be called by
`shcl/core/utility:observe-revival' when the process starts."
  (setf *working-directory-fds* (list (process-working-directory-retained))))
(on-revival initialize-working-directory-fds)

(defun %current-working-directory-fd ()
  "The non-locking version of `current-working-directory-fd'."
  (or (first *working-directory-fds*)
      (fd-autorelease (process-working-directory-retained))))

(defun current-working-directory-fd ()
  "Return a fd representing the current working directory as seen by
the current thread."
  (with-lock-held (*working-directory-lock*)
    (%current-working-directory-fd)))

(defun release-history (history)
  "Release the file descriptors in the given working directory history."
  (dolist (item history)
    (fd-release item)))

(defun retain-history (history)
  "Retain the file descriptors in the given working directory history."
  (dolist (item history)
    (fd-retain item)))

(defun preserve-working-directory-history ()
  "Create a copy of the current working directory history.

This copy must be destroyed with
`destroy-preserved-working-directory-history'."
  (with-lock-held (*working-directory-lock*)
    (let* ((fds *working-directory-fds*))
      (retain-history fds)
      fds)))

(defun destroy-preserved-working-directory-history (history)
  "Reclaim resources associated with the given working directory history.

After calling this function, the given history must not be used
again."
  (release-history history)
  (values))

(defun call-with-alternate-working-directory-history (history fn)
  "Call the given `fn' in a context where the working directory has been
switched to `history'.

The `history' argument must be a value returned by
`preserve-working-directory-history'."
  (let ((*working-directory-lock* (make-lock))
        (*working-directory-fds* history))
    ;; The history might change once we start executing body.  So,
    ;; we're going to retain it in its current state and release it
    ;; in whatever state it is in later.
    (retain-history *working-directory-fds*)
    (unwind-protect (funcall fn)
      (release-history *working-directory-fds*))))

(extend-shell-environment
 'working-directory
 'preserve-working-directory-history
 'call-with-alternate-working-directory-history
 'destroy-preserved-working-directory-history)

(defun %try-opening-path (path)
  "Open the given path if it exists and return the fd associated with the file.

Returns nil if the given path doesn't exist."
  (handler-bind
      ((syscall-error
        (lambda (e)
          (when (equal enoent (syscall-errno e))
            (return-from %try-opening-path nil)))))
    (fd-autorelease (openat-retained (%current-working-directory-fd) path o-rdonly))))

(defun directory-p (path)
  "Returns non-nil if the given path represents a directory."
  (with-lock-held (*working-directory-lock*)
    (with-fd-scope ()
      (let ((fd (%try-opening-path path)))
        (when fd
          (directory-fd-p fd))))))

(defun directory-fd-p (fd)
  "Returns non-nil if the given fd points at a directory."
  (s-isdir (slot-value (fstat fd) 'st-mode)))

(defgeneric path-invalid-message (condition)
  (:documentation
   "Access the human-readable description of of what is wrong"))

(define-condition path-invalid (error)
  ((message
    :type string
    :initarg :message
    :reader path-invalid-message
    :documentation
    "The human-readable description of what is wrong"))
  (:report (lambda (c s) (format s "Path is invalid.  ~A" (path-invalid-message c))))
  (:documentation
   "A condition that is signaled when a given path string isn't suitable."))

(defun %pop-working-directory ()
  "The non-locking version of `pop-working-directory'."
  (let ((fd (pop *working-directory-fds*)))
    (fd-release fd)
    (values)))

(defun pop-working-directory ()
  "Pop a directory off of the working directory history."
  (with-lock-held (*working-directory-lock*)
    (%pop-working-directory)))

(defun %push-working-directory-fd (fd)
  "The non-locking, fd-centric version of `push-working-directory'."
  (fd-retain fd)
  (push fd *working-directory-fds*))

(defun push-working-directory (path)
  "Push a new directory onto the working directory history."
  (with-lock-held (*working-directory-lock*)
    (with-fd-scope ()
      (let ((dir-fd (%try-opening-path path)))
        (unless (and dir-fd (directory-fd-p dir-fd))
          (error 'path-invalid :message (format nil "~A is not a directory" path)))
        (%push-working-directory-fd dir-fd)))))

(defun cd (path)
  "Change the current working directory.

This is more or less equivalent to calling `pop-working-directory' and
then `push-working-directory'."
  (with-lock-held (*working-directory-lock*)
    (with-fd-scope ()
      (let ((dir-fd (%try-opening-path path)))
        (unless (and dir-fd (directory-fd-p dir-fd))
          (error 'path-invalid :message (format nil "~A is not a directory" path)))
        (%pop-working-directory)
        (%push-working-directory-fd dir-fd)
        (values)))))
