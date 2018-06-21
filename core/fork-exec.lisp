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

(defpackage :shcl/core/fork-exec
  (:use :common-lisp :alexandria :cffi :shcl/core/utility :shcl/core/shell-grammar
        :shcl/core/posix-types :shcl/core/posix :shcl/core/support)
  (:import-from :shcl/core/fd-table #:fd-wrapper-release #:fd-wrapper-value
                #:retained-fd-dup)
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

  (let ((managed-fds (make-hash-table))
        temp-fd)
    (unwind-protect
         (labels
             ((get-temp-fd (&key donor-fd)
                (unless temp-fd
                  (assert donor-fd)
                  (setf temp-fd (retained-fd-dup donor-fd)))
                (fd-wrapper-value temp-fd)))

           (dolist (fd managed-fd-list)
             (setf (gethash fd managed-fds) t))

           (dolist (pair alist)
             (destructuring-bind (target-fd . value-fd) pair
               (remhash target-fd managed-fds)
               (when (eq :temp value-fd)
                 (setf value-fd (get-temp-fd)))
               (when (eq :temp target-fd)
                 (setf target-fd (get-temp-fd :donor-fd value-fd)))

               (cond
                 (value-fd
                  (debug-log status "FEDUP2 ~A -> ~A (~A = ~A)" value-fd target-fd target-fd value-fd)
                  (fd-actions-add-dup2 fd-actions value-fd target-fd))
                 (t
                  (debug-log status "FDCLOSE ~A" target-fd)
                  (fd-actions-add-close fd-actions target-fd)))))

           (loop :for fd :in (hash-table-keys managed-fds) :do
              (debug-log status "FECLOSE ~A" fd)
              (fd-actions-add-close fd-actions fd)))
      (fd-wrapper-release temp-fd))))

(defun run (command &key fd-alist managed-fds environment working-directory-fd)
  "Run a program.

`command' is the list of arguments for the binary to receive.  The
first element of the list is the name of the binary to run.  The PATH
environment variable specified in the `environment' argument controls
where this function will search for binaries.

`fd-alist' is a list of file descriptor dup2 actions to perform.  The
car of each cell represents the file descriptor to act upon and the
cdr represents the value to place into that file descriptor.  If the
value is nil, this function will close the file descriptor.  If you
need to use a temporary file descriptor to break a cycle, you can use
`:temp' in place of a specific file descriptor.  It is unspecified
which file descriptor `:temp' represents.  The only guarantee is that
it will be a file descriptor that is unused in the current process.

Here's an example value for `fd-alist' that rotates file descriptors 1
through 3.

    ((:temp . 1) (1 . 2) (2 . 3) (3 . :temp) (:temp . nil))

`managed-fds' is a list of file descriptors in the current process
that should not be shared with the child process.  Note that
`fd-alist' overrides this.  So, for example, if `managed-fds' contains
fd 1 and `fd-alist' sets fd 1 to some value, the child process will
have fd 1 bound to the value specified by `fd-alist'.

`environment' is a sequence containing the environment bindings the
child process should receive.  The elements of this sequence should be
strings suitable for passing to the putenv POSIX C function.

`working-directory-fd' is a file descriptor representing the desired
working directory for the child process.  This file descriptor must be
suitable for passing to the fchdir POSIX C function."
  (let ((fd-actions (make-fd-actions)))
    (take-fd-map fd-alist managed-fds fd-actions)
    (shcl-spawn (fset:first command) t working-directory-fd fd-actions command environment)))
