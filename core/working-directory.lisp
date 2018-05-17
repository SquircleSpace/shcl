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

(defpackage :shcl/core/working-directory
  (:use :common-lisp :bordeaux-threads :shcl/core/utility
        :shcl/core/fd-table :shcl/core/posix-types :shcl/core/posix
        :shcl/core/support :shcl/core/shell-environment)
  (:export #:directory-p #:path-invalid #:path-invalid-message #:cd
           #:push-working-directory #:pop-working-directory
           #:with-local-working-directory #:get-fd-current-working-directory))
(in-package :shcl/core/working-directory)

(defun retained-fd-process-working-directory ()
  "Return a retained fd representing the current working directory of
the process."
  (retained-fd-open "." O-RDONLY))

(defvar *working-directory-fds* nil
  "The current stack of working directories")
(defun working-directory-fds ()
  "Set `*working-directory-fds*' to its initial value."
  (cond
    (*working-directory-fds*
     *working-directory-fds*)
    (t
     (setf *working-directory-fds* (list (retained-fd-process-working-directory))))))
(on-revival working-directory-fds)

(defun get-fd-current-working-directory ()
  "Return a fd representing the current working directory as seen by
the current thread."
  (first (working-directory-fds)))

(defun release-history (history)
  "Release the file descriptors in the given working directory history."
  (dolist (item history)
    (fd-wrapper-release item)))

(defun retain-history (history)
  "Retain the file descriptors in the given working directory history."
  (dolist (item history)
    (fd-wrapper-retain item))
  history)

(defun preserve-working-directory-history ()
  "Create a copy of the current working directory history.

This copy must be destroyed with
`destroy-preserved-working-directory-history'."
  (retain-history (working-directory-fds)))

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
  (let ((*working-directory-fds* history))
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

(defun retained-fd-open-dir (path)
  (labels
      ((retained-fd-open-it ()
         (handler-case
             (retained-fd-openat (get-fd-current-working-directory) path o-rdonly)
           (syscall-error ()
             (return-from retained-fd-open-dir nil)))))
    (declare (dynamic-extent #'retained-fd-open-it))
    (receive-ref-counted-fd (fd (retained-fd-open-it))
      (when (directory-fd-p fd)
        (fd-wrapper-retain fd)))))

(defun directory-p (path)
  "Returns non-nil if the given path represents a directory."
  (receive-ref-counted-fd (fd (retained-fd-open-dir path))
    (when fd
      t)))

(defun directory-fd-p (fd)
  "Returns non-nil if the given fd points at a directory."
  (s-isdir (slot-value (fstat (fd-wrapper-value fd)) 'st-mode)))

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

(defun pop-working-directory ()
  "Pop a directory off of the working directory history."
  (fd-wrapper-release (pop *working-directory-fds*))
  (values))

(defun push-working-directory-fd (fd)
  "The non-locking, fd-centric version of `push-working-directory'."
  (push (fd-wrapper-retain fd) *working-directory-fds*)
  (values))

(defun push-working-directory (path)
  "Push a new directory onto the working directory history."
  (receive-ref-counted-fd (dir-fd (retained-fd-open-dir path))
    (unless dir-fd
      (error 'path-invalid :message (format nil "~A is not a directory" path)))
    (push-working-directory-fd dir-fd)))

(defun cd (path)
  "Change the current working directory.

This is more or less equivalent to calling `pop-working-directory' and
then `push-working-directory'."
  (receive-ref-counted-fd (dir-fd (retained-fd-open-dir path))
    (unless dir-fd
      (error 'path-invalid :message (format nil "~A is not a directory" path)))
    (pop-working-directory)
    (push-working-directory-fd dir-fd)
    (values)))

(defmacro with-local-working-directory ((place) &body body)
  "Within the body of this macro, the current working directory is
changed to be `place'.

You can think of this as being roughly equivalent to
     (unwind-protect
          (progn
            (push-working-directory ,place)
            ,@body)
       (pop-working-directory))

You may not assume anything about the contents of the working
directory stack within the body of this form.  There may or may not be
any file descriptors (other than the current one) on the stack.

The behavior is undefined if you push directories onto the stack which
are not popped off before control leaves the body of this macro."
  (let ((pop-needed (gensym "POP-NEEDED")))
    `(let (,pop-needed)
       (unwind-protect
            (progn
              (push-working-directory ,place)
              (setf ,pop-needed t)
              ,@body)
         (when ,pop-needed
           (pop-working-directory))))))
