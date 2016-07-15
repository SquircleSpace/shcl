(in-package :shcl.fork-exec)

(defun determine-open-fds ()
  (labels
      ((extract-fd (path)
         (when (directory-pathname-p path)
           (setf path (pathname-as-file path)))
         (parse-integer (pathname-name path))))
    (let* ((paths (list-directory "/dev/fd/" :follow-symlinks nil))
           (numbers (map 'vector #'extract-fd paths)))
      numbers)))

(defun take-fd-map (alist managed-fd-list file-actions)
  (format *error-output* "FETAKE ~A~%" alist)

  (let ((managed-fds (make-hash-table))
        (fd-table (make-hash-table))
        (already-set-fds (make-hash-table)))
    (dolist (fd managed-fd-list)
      (setf (gethash fd managed-fds) t))

    (dolist (pair alist)
      (destructuring-bind (target-fd . value-fd) pair
        (setf (gethash target-fd fd-table) (gethash value-fd fd-table value-fd))))

    (format *error-output* "FEMAP ~A~%" (hash-table-alist fd-table))

    (dolist (pair alist)
      (block continue
        (destructuring-bind (target-fd . value-fd) pair
          (when (gethash target-fd already-set-fds)
            (return-from continue))
          (setf (gethash target-fd already-set-fds) t)
          (setf value-fd (gethash target-fd fd-table))

          (remhash target-fd managed-fds)
          (format *error-output* "FEDUP2 ~A -> ~A (~A = ~A)~%" value-fd target-fd target-fd value-fd)
          (posix-spawn-file-actions-adddup2 file-actions value-fd target-fd))))

    (loop :for fd :in (hash-table-keys managed-fds) :do
       (format *error-output* "FECLOSE ~A~%" fd)
       (posix-spawn-file-actions-addclose file-actions fd))))

(defun run (command &key fd-alist managed-fds)
  (with-posix-spawn-file-actions (file-actions)
    (take-fd-map fd-alist managed-fds file-actions)
    (with-posix-spawnattr (attr)
      (with-foreign-object (pid 'pid-t)
        (with-foreign-object
            (c-argv :string (+ 1 (length command)))
          (loop :for index :below (length command) :do
             (setf (mem-aref c-argv :string index) (aref command index)))
          (setf (mem-aref c-argv :string (length command)) (null-pointer))

          (let ((envp (null-pointer)))
            (posix-spawnp pid (aref command 0) file-actions attr c-argv envp)
            (return-from run (mem-ref pid 'pid-t))))))))
