;; Copyright 2018 Bradley Jensen
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

(defpackage :shcl/deps
  (:use :common-lisp)
  (:import-from :shcl/core/debug #:traverse-dependencies #:shcl-system-name-p)
  (:export #:generate-deps-file #:main))
(in-package :shcl/deps)

(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3) (compilation-speed 0)))

(defun relative-source-file (child)
  (let ((relative-namestring (asdf:component-relative-pathname child))
        (type (asdf:file-type child)))
    (format nil "~A.~A" relative-namestring type)))

(defun collect-dependencies (system)
  (let ((files (make-hash-table :test 'equal)))
    (traverse-dependencies system (depender depended)
      (dolist (child (asdf:component-children (asdf:find-system depender)))
        (setf (gethash (asdf:component-relative-pathname child) files) t))
      (shcl-system-name-p depended))
    files))

(defun generate-deps-file (output-path)
  (let ((shcl-files (collect-dependencies "shcl"))
        (deps-files (collect-dependencies "shcl/deps")))

    (with-open-file (stream #P"dependencies" :direction :output :if-exists :supersede)
      (format stream "shcl:")
      (loop :for file :being :the :hash-keys :of shcl-files :do
         (format stream " ~A" file))
      (format stream "~%")

      (format stream "~A:" output-path)
      (loop :for file :being :the :hash-keys :of deps-files :do
         (format stream " ~A" file))
      (format stream "~%"))))

(defun main ()
  (generate-deps-file "dependencies")
  (uiop:quit))
