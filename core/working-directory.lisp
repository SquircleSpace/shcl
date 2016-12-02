(defpackage :shcl/core/working-directory
  (:use :common-lisp :bordeaux-threads :shcl/core/utility
        :shcl/core/fd-table :shcl/core/posix-types :shcl/core/posix
        :shcl/core/support :shcl/core/shell-environment)
  (:export #:directory-p #:path-invalid #:path-invalid-message #:cd
           #:push-working-directory #:pop-working-directory
           #:current-working-directory-fd))
(in-package :shcl/core/working-directory)

(defvar *process-working-directory-lock* (make-recursive-lock))

(defun process-working-directory-retained ()
  (with-recursive-lock-held (*process-working-directory-lock*)
    (open-retained "." O-RDONLY)))

(defvar *working-directory-fds* nil)
(defvar *working-directory-lock* (make-lock))
(defun initialize-working-directory-fds ()
  (setf *working-directory-fds* (list (process-working-directory-retained))))
(on-revival initialize-working-directory-fds)

(defun %current-working-directory-fd ()
  (or (first *working-directory-fds*)
      (fd-autorelease (process-working-directory-retained))))

(defun current-working-directory-fd ()
  (with-lock-held (*working-directory-lock*)
    (%current-working-directory-fd)))

(defun release-history (history)
  (dolist (item history)
    (fd-release item)))

(defun retain-history (history)
  (dolist (item history)
    (fd-retain item)))

(defun preserve-working-directory-history ()
  (with-lock-held (*working-directory-lock*)
    (let* ((fds *working-directory-fds*))
      (retain-history fds)
      fds)))

(defun destroy-preserved-working-directory-history (history)
  (release-history history)
  (values))

(defmacro with-alternate-working-directory-history (preserved-history &body body)
  (let ((history (gensym "HISTORY")))
    `(let* ((,history ,preserved-history)
            (*working-directory-lock* (make-lock))
            (*working-directory-fds* ,history))
       (retain-history *working-directory-fds*)
       (unwind-protect (progn ,@body)
         (release-history *working-directory-fds*)))))

(defun call-with-alternate-working-directory-history (history fn)
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
  (handler-bind
      ((syscall-error
        (lambda (e)
          (when (equal enoent (syscall-errno e))
            (return-from %try-opening-path nil)))))
    (fd-autorelease (openat-retained (%current-working-directory-fd) path o-rdonly))))

(defun directory-p (path)
  (with-lock-held (*working-directory-lock*)
    (with-fd-scope ()
      (let ((fd (%try-opening-path path)))
        (when fd
          (directory-fd-p fd))))))

(defun directory-fd-p (fd)
  (s-isdir (slot-value (fstat fd) 'st-mode)))

(define-condition path-invalid (error)
  ((message
    :type string
    :initarg :message
    :reader path-invalid-message))
  (:report (lambda (c s) (format s "Path is invalid.  ~A" (path-invalid-message c)))))

(defun %pop-working-directory ()
  (let ((fd (pop *working-directory-fds*)))
    (fd-release fd)
    (values)))

(defun pop-working-directory ()
  (with-lock-held (*working-directory-lock*)
    (%pop-working-directory)))

(defun %push-working-directory-fd (fd)
  (fd-retain fd)
  (push fd *working-directory-fds*))

(defun push-working-directory (path)
  (with-lock-held (*working-directory-lock*)
    (with-fd-scope ()
      (let ((dir-fd (%try-opening-path path)))
        (unless (and dir-fd (directory-fd-p dir-fd))
          (error 'path-invalid :message (format nil "~A is not a directory" path)))
        (%push-working-directory-fd dir-fd)))))

(defun cd (path)
  (with-lock-held (*working-directory-lock*)
    (with-fd-scope ()
      (let ((dir-fd (%try-opening-path path)))
        (unless (and dir-fd (directory-fd-p dir-fd))
          (error 'path-invalid :message (format nil "~A is not a directory" path)))
        (%pop-working-directory)
        (%push-working-directory-fd dir-fd)
        (values)))))
