(defpackage :shcl/fork-exec
  (:use :common-lisp :alexandria :cffi :shcl/utility :shcl/shell-grammar
        :shcl/posix-types :shcl/posix)
  (:shadowing-import-from :alexandria #:when-let #:when-let*)
  (:import-from :cl-fad #:list-directory #:directory-pathname-p #:pathname-as-file)
  (:export #:run))
(in-package :shcl/fork-exec)

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
  (debug-log status "FETAKE ~A" alist)

  (let ((managed-fds (make-hash-table)))
    (dolist (fd managed-fd-list)
      (setf (gethash fd managed-fds) t))

    (dolist (pair alist)
      (destructuring-bind (target-fd . value-fd) pair
        (remhash target-fd managed-fds)
        (debug-log status "FEDUP2 ~A -> ~A (~A = ~A)" value-fd target-fd target-fd value-fd)
        (posix-spawn-file-actions-adddup2 file-actions value-fd target-fd)))

    (loop :for fd :in (hash-table-keys managed-fds) :do
       (debug-log status "FECLOSE ~A" fd)
       (posix-spawn-file-actions-addclose file-actions fd))))

(defun run (command &key fd-alist managed-fds (environment (fset:empty-seq)))
  (setf command (fset:convert 'fset:seq command))
  (with-posix-spawn-file-actions (file-actions)
    (take-fd-map fd-alist managed-fds file-actions)
    (with-posix-spawnattr (attr)
      (with-foreign-object (pid 'pid-t)
        (posix-spawnp pid (fset:first command) file-actions attr command environment)
        (return-from run (mem-ref pid 'pid-t))))))
