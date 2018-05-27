(defpackage :shcl/test/posix
  (:use :common-lisp :prove :cffi :shcl/core/utility :shcl/core/posix
        :shcl/test/foundation)
  (:import-from :shcl/core/posix-types #:dirent #:d-name)
  (:import-from :shcl/core/fd-table #:with-private-fds))
(in-package :shcl/test/posix)

(optimization-settings)

(defcfun (opendir "opendir") dir-ptr
  (name :string))

(defun fork ()
  #+sbcl (sb-posix:fork)
  #-sbcl (error "Cannot fork on this compiler"))

(defcfun (_exit "_exit") :void
  (status :int))

(defmacro forked (&body body)
  (let ((pid (gensym "PID"))
        (e (gensym "E")))
    `(let ((,pid (fork)))
       (cond
         ((plusp ,pid)
          ,pid)
         ((zerop ,pid)
          (unwind-protect
               (handler-case (progn ,@body)
                 (error (,e)
                   (format *error-output* "ERROR: ~A~%" ,e)
                   (finish-output *error-output*)
                   (_exit 1)))
            (_exit 0)))
         ((minusp ,pid)
          ;; The wrapper around posix fork should have taken care of this
          ;; for us
          (assert nil nil "This is impossible"))))))

(defun open-fds ()
  (let ((result (make-extensible-vector :element-type 'integer))
        dir-fd
        dir)
    (unwind-protect
         (progn
           (setf dir (opendir "/dev/fd"))
           (setf dir-fd (dirfd dir))
           (loop
              (block again
                (let ((dirent (readdir dir))
                      name-ptr)
                  (when (null-pointer-p dirent)
                    (return))
                  (setf name-ptr (foreign-slot-pointer dirent '(:struct dirent) 'd-name))
                  (let ((s (foreign-string-to-lisp name-ptr)))
                    (when (equal #\. (aref s 0))
                      (return-from again))
                    (vector-push-extend (parse-integer s)
                                        result))))))
      (when dir
        (closedir dir)))
    (remove dir-fd result)))

(defun verify-fds ()
  (with-private-fds (shcl-exceptions)
    (let* ((compiler-exceptions (shcl/core/fd-table::compiler-owned-fds))
           (exceptions (fset:convert 'fset:set (nconc (list 0 1 2) compiler-exceptions shcl-exceptions)))
           (open-fds (fset:convert 'fset:set (open-fds)))
           (bad (fset:set-difference open-fds exceptions)))
      (cond
        ((zerop (fset:size bad))
         (_exit 0))

        (t
         (format *standard-output* "Unexpected fds: ~A~%" (fset:convert 'list bad))
         (format *standard-output* "Exceptions: ~A~%" (fset:convert 'list exceptions))
         (format *standard-output* "Open: ~A~%" (fset:convert 'list open-fds))
         (finish-output *standard-output*)
         (_exit 1))))))

(define-test fds
  (block fds
    (cl-fad:with-open-temporary-file (s)
      (let ((path (pathname s)))
        (let* ((pid (handler-case
                        (forked
                          (setf uiop:*image-entry-point* #'verify-fds)
                          (uiop:dump-image path :executable t))
                      (error (e)
                        (skip 1 "~A" e)
                        (return-from fds))))
               (exit-status (nth-value 1 (waitpid pid 0))))
          (if (zerop exit-status)
              (pass "Was able to create test executable")
              (progn
                (fail (format nil "Exit status was non-zero ~A" exit-status))
                (return-from fds))))
        (multiple-value-bind (output error-output exit-status) (uiop:run-program (list path) :output :string :error-output :output :ignore-error-status t)
          (declare (ignore error-output))
          (if (zerop exit-status)
              (pass "No unaccounted fds")
              (fail output)))))))
