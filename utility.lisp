(in-package :shcl.utility)

(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3) (compilation-speed 0)))

(defmacro define-once-global (name &body initform)
  (let ((value (gensym "VALUE"))
        (set (gensym "SET")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let (,value ,set)
         (defun ,name ()
           (unless ,set
             (setf ,value (progn ,@initform)
                   ,set t))
           ,value)
         (define-symbol-macro ,name (,name))))))
