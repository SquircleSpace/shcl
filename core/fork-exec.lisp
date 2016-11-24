(defpackage :shcl/core/fork-exec
  (:use :common-lisp :alexandria :cffi :shcl/core/utility :shcl/core/shell-grammar
        :shcl/core/posix-types :shcl/core/posix :shcl/core/support)
  (:shadowing-import-from :alexandria #:when-let #:when-let*)
  (:import-from :cl-fad #:list-directory #:directory-pathname-p #:pathname-as-file)
  (:export #:run))
(in-package :shcl/core/fork-exec)

(defun determine-open-fds ()
  (labels
      ((extract-fd (path)
         (when (directory-pathname-p path)
           (setf path (pathname-as-file path)))
         (parse-integer (pathname-name path))))
    (let* ((paths (list-directory "/dev/fd/" :follow-symlinks nil))
           (numbers (map 'vector #'extract-fd paths)))
      numbers)))

(defun take-fd-map (alist managed-fd-list fd-actions)
  (debug-log status "FETAKE ~A" alist)

  (let ((managed-fds (make-hash-table)))
    (dolist (fd managed-fd-list)
      (setf (gethash fd managed-fds) t))

    (dolist (pair alist)
      (destructuring-bind (target-fd . value-fd) pair
        (remhash target-fd managed-fds)
        (debug-log status "FEDUP2 ~A -> ~A (~A = ~A)" value-fd target-fd target-fd value-fd)
        (fd-actions-add-dup2 fd-actions value-fd target-fd)))

    (loop :for fd :in (hash-table-keys managed-fds) :do
       (debug-log status "FECLOSE ~A" fd)
       (fd-actions-add-close fd-actions fd))))

(defun run (command &key fd-alist managed-fds (environment (fset:empty-seq)) working-directory-fd)
  (setf command (fset:convert 'fset:seq command))
  (let ((fd-actions (make-fd-actions)))
    (take-fd-map fd-alist managed-fds fd-actions)
    (shcl-spawn (fset:first command) t working-directory-fd fd-actions command environment)))
