(defpackage :shcl/shell/logs
  (:use :common-lisp :shcl/core/utility :shcl/core/builtin))
(in-package :shcl/shell/logs)

(define-builtin -shcl-dump-logs (args)
  (declare (ignore args))
  (dump-logs)
  0)
