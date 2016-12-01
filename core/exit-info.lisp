(defpackage :shcl/core/exit-info
  (:use :common-lisp :shcl/core/utility :shcl/core/posix)
  (:export
   #:exit-info #:exit-info-p #:exit-info-true-p #:exit-info-false-p
   #:invert-exit-info #:make-exit-info #:truthy-exit-info #:falsey-exit-info
   #:internal-error-exit-info))
(in-package :shcl/core/exit-info)

(optimization-settings)

(defclass exit-info ()
  ((pid
    :reader exit-info-pid
    :initform nil
    :initarg :pid)
   (exit-status
    :reader exit-info-exit-status
    :initform nil
    :initarg :exit-status)
   (exit-signal
    :reader exit-info-exit-signal
    :initform nil
    :initarg :exit-signal)
   (stop-signal
    :reader exit-info-stop-signal
    :initform nil
    :initarg :stop-signal)))

(defun exit-info-p (thing)
  (typep thing 'exit-info))

(defmethod print-object ((exit-info exit-info) stream)
  (print-unreadable-object (exit-info stream :type t)
    (with-slots (pid exit-status exit-signal stop-signal) exit-info
      (let ((things (make-extensible-vector)))
        (when pid
          (vector-push-extend (cons :pid pid) things))
        (when exit-status
          (vector-push-extend (cons :exit-status exit-status) things))
        (when exit-signal
          (vector-push-extend (cons :exit-signal exit-signal) things))
        (when stop-signal
          (vector-push-extend (cons :stop-signal stop-signal) things))
        (loop :while (< 1 (length things)) :do
           (let ((pair (vector-pop things)))
             (format stream "~A ~A " (car pair) (cdr pair))))
        (let ((pair (vector-pop things)))
          (format stream "~A ~A" (car pair) (cdr pair)))))))

(defun exit-info-true-p (exit-info)
  "Given an exit info, return t iff the program exited successfully."
  (with-slots (exit-status exit-signal stop-signal) exit-info
    (labels ((okay (value) (or (null value) (zerop value))))
      (and (okay exit-status)
           (okay exit-signal)
           (okay stop-signal)))))

(defun exit-info-false-p (exit-info)
  "Given an exit info, return t iff the program didn't exit
sucesfully."
  (not (exit-info-true-p exit-info)))

(defun invert-exit-info (exit-info)
  "Given an exit info, produce a similar info that indicates the logical inverse."
  (if (exit-info-true-p exit-info)
      (make-exit-info :exit-status 1)
      (make-exit-info :exit-status 0)))

(defun exit-info-code (exit-info)
  "Turn an exit-info into an integer.  This probably isn't something you want."
  (with-slots (exit-code exit-signal stop-signal) exit-info
    (+ (if exit-code exit-code 0)
       (if exit-signal (ash exit-signal 7) 0)
       (if stop-signal (ash stop-signal 8) 0))))

(defun make-exit-info (&key pid exit-status exit-signal stop-signal)
  "Produce an exit info that incorperates the given information."
  (make-instance 'exit-info :pid pid :exit-status exit-status :exit-signal exit-signal :stop-signal stop-signal))

(defun truthy-exit-info ()
  "Produce an exit-info that indicates success"
  (make-exit-info :exit-status 0))

(defun falsey-exit-info ()
  "Produce an exit-info that indicates failure"
  (make-exit-info :exit-status 1))

(defun internal-error-exit-info ()
  "Produce an exit-info that indicates shcl has failed in some way"
  (make-exit-info :exit-status 128))
