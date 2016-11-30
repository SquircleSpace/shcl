(defpackage :shcl/core/working-directory
  (:use :common-lisp :bordeaux-threads :shcl/core/utility
        :shcl/core/fd-table :shcl/core/posix-types :shcl/core/posix
        :shcl/core/shell-environment)
  (:export #:cd #:push-working-directory #:pop-working-directory
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
      (let ((dir-fd (openat-retained (%current-working-directory-fd) path O-RDONLY)))
        (unwind-protect
             (progn
               (%push-working-directory-fd dir-fd))
          (fd-release dir-fd))))))

(defun cd (path)
  (with-lock-held (*working-directory-lock*)
    (with-fd-scope ()
      (let ((dir-fd (openat-retained (%current-working-directory-fd) path O-RDONLY)))
        (unwind-protect
             (progn
               (%pop-working-directory)
               (%push-working-directory-fd dir-fd)
               (values))
          (fd-release dir-fd))))))
