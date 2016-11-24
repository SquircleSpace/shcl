(defpackage :shcl-test/environment
  (:use :common-lisp :prove :shcl/core/utility :shcl/core/environment))
(in-package :shcl-test/environment)

(optimization-settings)

(plan 1)

(defun contains-p (key)
  (nth-value 1 (env key)))

(deftest basics
  (let ((*environment* *environment*))
    (clear-environment)
    (ok (not (contains-p "foo")))

    (setf (env "foo") "bar")
    (ok (equal (env "foo") "bar"))
    (ok (not (exported-p "foo")))

    (export-variable "foo")
    (ok (exported-p "foo"))

    (unexport-variable "foo")
    (ok (not (exported-p "foo")))

    (unset-env "foo")
    (ok (not (contains-p "foo")))

    (setf (env "not_exported") "value")
    (with-environment-scope ()
      (setf (env "bar") "baz")
      (ok (equal (env "bar") "baz"))
      (export-variable "not_exported"))
    (ok (not (contains-p "bar")))
    (ok (not (exported-p "not_exported")))

    (setf $path "path")
    (ok (equal $path "path"))))
