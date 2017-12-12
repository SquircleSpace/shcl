(defpackage :shcl/bootstrap
  (:use :common-lisp))
(in-package :shcl/bootstrap)

(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3) (compilation-speed 0)))

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :trivial-gray-streams))

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
        (asdf:compile-system :shcl))
      (asdf:load-system :shcl)
      (when (uiop:getenv "SHCL_DEBUG")
        (push (make-pathname :directory (concatenate 'list (pathname-directory *load-truename*) '("test"))
                               :name nil :type nil
                               :defaults *load-truename*)
              asdf:*central-registry*)
        (reduce-compile-noise
          (asdf:load-system :shcl/core/debug)
          (asdf:load-system :shcl-test)))

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
