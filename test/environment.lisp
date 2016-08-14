(in-package :shcl-test.environment)
(in-suite environment)

(defun contains-p (key)
  (nth-value 1 (env key)))

(def-test basics (:compile-at :definition-time)
  (let ((*environment* *environment*))
    (clear-environment)
    (is (not (contains-p "foo")))

    (setf (env "foo") "bar")
    (is (equal (env "foo") "bar"))
    (is (not (exported-p "foo")))

    (export-variable "foo")
    (is (exported-p "foo"))

    (unexport-variable "foo")
    (is (not (exported-p "foo")))

    (unset-env "foo")
    (is (not (contains-p "foo")))

    (setf (env "not_exported") "value")
    (with-environment-scope ()
      (setf (env "bar") "baz")
      (is (equal (env "bar") "baz"))
      (export-variable "not_exported"))
    (is (not (contains-p "bar")))
    (is (not (exported-p "not_exported")))

    (setf $path "path")
    (is (equal $path "path"))))
