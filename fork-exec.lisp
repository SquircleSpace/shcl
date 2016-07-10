(in-package :shcl.fork-exec)

(defparameter *pid* nil)

(defmacro fork-and-die (&body body)
  (let ((result (gensym "RESULT")))
    `(let ((,result (sb-posix:fork)))
       (cond
         ((equal ,result 0)
          (unwind-protect
               (progn ,@body)
            (sb-ext:exit)))

         ((< ,result 0)
          (error "Impossible.  SBCL should already catch this"))

         (t
          ,result)))))

(defun print-fd-map (map stream)
  (let ((alist (sort (hash-table-alist map) #'< :key #'car)))
    (format stream "~A MAP: ~A~%" *pid* alist)))

(defun clean-fd-map (map)
  ;; First, we are going to move all the fd -> fd mappings to unused
  ;; fds
  (let ((fds-to-remap (make-hash-table))
        (map-copy (copy-hash-table map)))
    (with-hash-table-iterator (iter map-copy)
      (loop
         (block continue
           (multiple-value-bind (valid key value) (iter)
             (unless valid
               (return))

             (unless (typep value 'integer)
               (return-from continue))

             (unless (gethash value map-copy)
               (return-from continue))

             (unless (gethash value fds-to-remap)
               (let ((new-home (sb-posix:dup value)))
                 (format *error-output* "~A DUP ~A -> ~A~%" *pid* key new-home)
                 (setf (gethash value fds-to-remap) new-home)))

             (setf (gethash key map) (gethash value fds-to-remap))))))

    map-copy))

(defun determine-open-fds ()
  (labels
      ((extract-fd (path)
         (when (directory-pathname-p path)
           (setf path (pathname-as-file path)))
         (parse-integer (pathname-name path))))
    (let* ((paths (list-directory "/dev/fd/" :follow-symlinks nil))
           (numbers (map 'vector #'extract-fd paths)))
      numbers)))

(defun take-fd-map (map managed-fds)
  (with-hash-table-iterator (iter map)
    (loop
       (block continue
         (multiple-value-bind (valid key value) (iter)
           (unless valid
             (return))

           (format *error-output* "~A DUP2 ~A -> ~A~%" *pid* value key)
           (sb-posix:dup2 value key)))))

  (loop :for fd :in (hash-table-keys managed-fds) :do
     (format *error-output* "~A CLOSE ~A~%" *pid* fd)
     (sb-posix:close fd)))

(defcfun (%execvp "execvp") :int
  (file :string)
  (argv (:pointer :string)))

(defun execvp (file argv)
  (with-foreign-object
      (c-argv :string (+ 1 (length argv)))
    (loop :for index :below (length argv) :do
       (setf (mem-aref c-argv :string index) (aref argv index)))
    (setf (mem-aref c-argv :string (length argv)) (null-pointer))
    (%execvp file c-argv)))

(defun fork-exec (command &key fd-map managed-fds)
  (fork-and-die
   (handler-case
       (progn
         (setf *pid* (sb-posix:getpid))
         (format *error-output* "FORK ~A~%" *pid*)
         (print-fd-map fd-map *error-output*)
         (setf fd-map (clean-fd-map fd-map))
         (print-fd-map fd-map *error-output*)
         (take-fd-map fd-map managed-fds)
         (execvp (aref command 0) command))
     (error (c) (format *error-output* "~A~%" c)))))
