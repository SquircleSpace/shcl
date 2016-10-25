(defpackage :shcl-test/posix
  (:use :common-lisp :shcl/posix :prove))
(in-package :shcl-test/posix)

(defun verify-fds ()
  (let* ((exceptions (fset:union (fset:set 0 1 2)
                                 (fset:convert 'fset:set (compiler-owned-fds))))
         (open-fds (fset:convert 'fset:set (open-fds)))
         (bad (fset:set-difference open-fds exceptions)))
    (cond
      ((zerop (fset:size bad))
       (_exit 0))

      (t
       (format *standard-output* "Unexpected fds: ~A~%" (fset:convert 'list bad))
       (finish-output *standard-output*)
       (_exit 1)))))

(deftest fds
  (block fds
    (cl-fad:with-open-temporary-file (s)
      (let ((path (pathname s)))
        (let* ((pid (forked
                      (setf uiop:*image-entry-point* #'verify-fds)
                      (uiop:dump-image path :executable t)))
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
