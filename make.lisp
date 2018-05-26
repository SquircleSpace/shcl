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

(defpackage :shcl/bootstrap
  (:use :common-lisp))
(in-package :shcl/bootstrap)

(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3) (compilation-speed 0)))

(defun shcl-load (package)
  (if (find-package :ql)
      (funcall (intern "QUICKLOAD" :ql) package)
      (asdf:load-system package)))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (shcl-load :trivial-gray-streams))

#+sbcl
(progn
  (defclass filtered (trivial-gray-streams:fundamental-character-output-stream)
    ((line-buffer
      :initform (make-string-output-stream))
     (real-output
      :initarg :output)))
  (defmethod trivial-gray-streams:stream-write-char ((stream filtered) char)
    (write-char char (slot-value stream 'line-buffer))
    (unless (or (equal char #\return)
                (equal char #\linefeed))
      (return-from trivial-gray-streams:stream-write-char))

    (let ((line (get-output-stream-string (slot-value stream 'line-buffer)))
          (sign "; compiling"))
      (unless (and (>= (length line) (length sign))
                   (equal (subseq line 0 (length sign)) sign))
        (write-string line (slot-value stream 'real-output))))))

#+sbcl
(defmacro reduce-compile-noise (&body body)
  `(let ((*standard-output* (make-instance 'filtered :output *standard-output*)))
     ,@body))

#-sbcl
(defmacro reduce-compile-noise (&body body)
  `(progn ,@body))

(let ((asdf:*central-registry* (cons (directory-namestring (truename *load-pathname*))
                                     asdf:*central-registry*)))
  (handler-bind
      ((error
        (lambda (c)
          (format *error-output* "~%Fatal error: ~A~%" c)
          (uiop:quit 1))))
    #-ecl
    (progn
      (reduce-compile-noise
        (shcl-load :shcl))
      (when (uiop:getenv "SHCL_DEBUG")
        (push (make-pathname :directory (concatenate 'list (pathname-directory *load-truename*) '("test"))
                               :name nil :type nil
                               :defaults *load-truename*)
              asdf:*central-registry*)
        (reduce-compile-noise
          (shcl-load :shcl/core/debug)
          (shcl-load :shcl-test)))

      (asdf:oos 'asdf:program-op :shcl))
    #+ecl
    (progn
      (asdf:register-immutable-system :asdf)
      (asdf:register-immutable-system :uiop)

      (defmethod asdf:output-files ((op asdf:monolithic-lib-op) (sys (eql (asdf:find-system "shcl"))))
        (values (list "libshcl.a") t))

      (asdf:operate
       'asdf:monolithic-lib-op "shcl"))
    (uiop:quit 0)))
