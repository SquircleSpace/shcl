(in-package :shcl.fork-exec)

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
               (setf (gethash value fds-to-remap) (sb-posix:dup key)))

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

(defun take-fd-map (map)
  (let ((fds-to-close (make-array 0 :adjustable t :fill-pointer t)))
    (with-hash-table-iterator (iter map)
      (loop
         (block continue
           (multiple-value-bind (valid key value) (iter)
             (unless valid
               (return))

             (sb-posix:dup2 value key)
             (format *error-output* "dup2 ~A overwrites ~A~%" value key)
             (vector-push-extend value fds-to-close)))))

    (setf fds-to-close (delete-duplicates fds-to-close))
    (loop :for fd :across fds-to-close :do
       (format *error-output* "Closing ~A~%" fd)
       (sb-posix:close fd))))

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

(defun fork-exec (command &key fd-map)
  (fork-and-die
    (format *error-output* "Fork~%")
    (setf fd-map (clean-fd-map fd-map))
    (take-fd-map fd-map)
    (execvp (aref command 0) command)))
