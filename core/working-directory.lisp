(defpackage :shcl/core/working-directory
  (:use :common-lisp :trivial-garbage :bordeaux-threads :shcl/core/utility
        :shcl/core/fd-table :shcl/core/posix-types :shcl/core/posix)
  (:export #:cd #:push-working-directory #:pop-working-directory
           #:current-working-directory-fd #:preserve-working-directory-history
           #:with-alternate-working-directory-history))
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

(defstruct wrapper
  object)

(defstruct preserved-working-directory-history
  fds)

(defun release-history (history)
  (dolist (item history)
    (fd-release item)))

(defun retain-history (history)
  (dolist (item history)
    (fd-retain item)))

(defun preserve-working-directory-history ()
  (with-lock-held (*working-directory-lock*)
    (let* ((fds *working-directory-fds*)
           (wrapper (make-wrapper :object fds))
           (wrapped-history (make-preserved-working-directory-history :fds wrapper)))
      (labels ((finalize-preserved-working-directory-history ()
                 (release-history fds)))
        (retain-history fds)
        (finalize wrapper #'finalize-preserved-working-directory-history)
        wrapped-history))))

(defun destroy-preserved-working-directory-history (history)
  (cancel-finalization (preserved-working-directory-history-fds history))
  (setf (wrapper-object (preserved-working-directory-history-fds history)) nil)
  (setf (preserved-working-directory-history-fds history) nil)
  (values))

(defun rehydrate-working-directory-history-fds (history)
  (wrapper-object (preserved-working-directory-history-fds history)))

(defmacro with-alternate-working-directory-history (preserved-history (&key (destroy t)) &body body)
  (let ((history (gensym "HISTORY")))
    `(let* ((,history ,preserved-history)
            (*working-directory-lock* (make-lock))
            (*working-directory-fds* (rehydrate-working-directory-history-fds ,history)))
       ;; The history might change once we start executing body.  So,
       ;; we're going to retain it in its current state and release it
       ;; in whatever state it is in later.
       (retain-history *working-directory-fds*)
       ,@(when destroy `((destroy-preserved-working-directory-history ,history)))
       (unwind-protect (progn ,@body)
         (release-history *working-directory-fds*)))))

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
    (let ((dir-fd (openat-retained (%current-working-directory-fd) path O-RDONLY)))
      (unwind-protect
           (progn
             (%push-working-directory-fd dir-fd))
        (fd-release dir-fd)))))

(defun cd (path)
  (with-lock-held (*working-directory-lock*)
    (let ((dir-fd (openat-retained (%current-working-directory-fd) path O-RDONLY)))
      (unwind-protect
           (progn
             (%pop-working-directory)
             (%push-working-directory-fd dir-fd)
             (values))
        (fd-release dir-fd)))))
