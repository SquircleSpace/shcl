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

(defpackage :shcl/core/support
  (:use :common-lisp :cffi :shcl/core/utility :shcl/core/posix-types)
  (:export
   #:wifexited #:wifstopped #:wifsignaled #:wexitstatus #:wtermsig #:wstopsig
   #:string-table #:fd-actions #:make-fd-actions #:fd-actions-add-close
   #:fd-actions-add-dup2 #:shcl-spawn #:s-isreg #:s-isdir #:s-ischr #:s-isblk
   #:s-isfifo #:s-islnk #:s-issock #:define-c-wrapper))
(in-package :shcl/core/support)

(optimization-settings)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library shcl-support
    (:darwin (:default "libshcl-support") :search-path ".")
    (:linux (:default "libshcl-support") :search-path ".")))

(use-foreign-library shcl-support)

(defun pass (value)
  "Returns t."
  (declare (ignore value))
  t)

(defmacro define-c-wrapper ((lisp-name c-name &key library) (return-type &optional (error-checker ''pass))
                            &body arg-descriptions)
  "Define a CFFI binding and wrapper function (or macro) which checks
for errors."
  (let ((lisp-impl-name (intern (concatenate 'string "%" (symbol-name lisp-name))))
        (result (gensym "RESULT")))
    (labels
        ((defun-based ()
           (let ((args (mapcar 'first arg-descriptions)))
             `(defun ,lisp-name (,@args)
                ,(format nil "This function is a wrapper around the ~A C function.

It will signal a `syscall-error' if the following predicate returns nil.
~S" c-name error-checker)
                (let ((,result (,lisp-impl-name ,@args)))
                  (unless (funcall ,error-checker ,result)
                    (error 'syscall-error :function ',lisp-name))
                  ,result))))
         (macro-argify (thing)
           (if (typep thing 'cons)
               (list (first thing))
               (list '&rest '#:rest)))
         (macro-based ()
           (let* ((whole (gensym "WHOLE"))
                  (args (apply 'concatenate 'list (mapcar #'macro-argify arg-descriptions))))
             `(defmacro ,lisp-name (&whole ,whole ,@args)
                ,(format nil "This macro is a wrapper around the ~A C function.

It will signal a `syscall-error' if the following predicate returns nil.
~S" c-name error-checker)
                (declare (ignore ,@(remove '&rest args)))
                `(let ((,',result (,',lisp-impl-name ,@(cdr ,whole))))
                   (unless (funcall ,',error-checker ,',result)
                     (error 'syscall-error :function ',',lisp-name))
                   ,',result))))
         (wrapper ()
           (if (find '&rest arg-descriptions)
               (macro-based)
               (defun-based))))
      `(progn
         (defcfun (,lisp-impl-name ,c-name ,@(when library
                                               `(:library ,library)))
             ,return-type
           ,@arg-descriptions)
         ,(wrapper)))))

(define-c-wrapper (wifexited "wifexited" :library shcl-support) ((:boolean :int))
  (status :int))

(define-c-wrapper (wifstopped "wifstopped" :library shcl-support) ((:boolean :int))
  (status :int))

(define-c-wrapper (wifsignaled "wifsignaled" :library shcl-support) ((:boolean :int))
  (status :int))

(define-c-wrapper (wexitstatus "wexitstatus" :library shcl-support) (:int)
  (status :int))

(define-c-wrapper (wtermsig "wtermsig" :library shcl-support) (:int)
  (status :int))

(define-c-wrapper (wstopsig "wstopsig" :library shcl-support) (:int)
  (status :int))

(define-c-wrapper (s-isreg "s_isreg" :library shcl-support) ((:boolean :int))
  (mode mode-t))

(define-c-wrapper (s-isdir "s_isdir" :library shcl-support) ((:boolean :int))
  (mode mode-t))

(define-c-wrapper (s-ischr "s_ischr" :library shcl-support) ((:boolean :int))
  (mode mode-t))

(define-c-wrapper (s-isblk "s_isblk" :library shcl-support) ((:boolean :int))
  (mode mode-t))

(define-c-wrapper (s-isfifo "s_isfifo" :library shcl-support) ((:boolean :int))
  (mode mode-t))

(define-c-wrapper (s-islnk "s_islnk" :library shcl-support) ((:boolean :int))
  (mode mode-t))

(define-c-wrapper (s-issock "s_issock" :library shcl-support) ((:boolean :int))
  (mode mode-t))

(define-foreign-type string-table ()
  ((size
    :initarg :size
    :initform nil))
  (:actual-type :pointer)
  (:simple-parser string-table)
  (:documentation
   "A CFFI foreign type to represent an array of strings."))

(defmethod translate-to-foreign ((sequence fset:seq) (type string-table))
  (with-slots (size) type
    (let ((seq-size (fset:size sequence))
          table
          side-table)
      (when size
        (assert (equal seq-size size)))
      (let (success)
        (unwind-protect
             (let ((index 0))
               (setf table (foreign-alloc '(:pointer :char) :initial-element (null-pointer) :count (1+ seq-size)))
               (setf side-table (make-array seq-size :initial-element nil))
               (fset:do-seq (thing sequence)
                 (multiple-value-bind (converted-value param) (convert-to-foreign thing :string)
                   (setf (mem-aref table '(:pointer :char) index) converted-value)
                   (setf (aref side-table index) param)
                   (incf index)))
               (setf success t)
               (values table side-table))
          (unless success
            (if side-table
                (free-translated-object table type side-table)
                (foreign-free table))))))))

(defmethod translate-to-foreign ((sequence list) (type string-table))
  (translate-to-foreign (fset:convert 'fset:seq sequence) type))

(defmethod translate-to-foreign ((sequence vector) (type string-table))
  (translate-to-foreign (fset:convert 'fset:seq sequence) type))

(defmethod free-translated-object (translated (type string-table) param)
  (loop :for index :below (length param) :do
     (unless (null-pointer-p (mem-aref translated '(:pointer :char) index))
       (free-converted-object (mem-aref translated '(:pointer :char) index) :string (aref param index))))
  (foreign-free translated))

(define-c-wrapper (%%make-fd-actions "make_shcl_fd_actions") (:pointer))
(define-c-wrapper (%%destroy-fd-actions "destroy_shcl_fd_actions") (:void)
  (actions :pointer))
(define-c-wrapper (%%fd-actions-add-close "shcl_fd_actions_add_close") (:void)
  (actions :pointer)
  (fd :int))
(define-c-wrapper (%%fd-actions-add-dup2 "shcl_fd_actions_add_dup2") (:void)
  (actions :pointer)
  (fd1 :int)
  (fd2 :int))

(defstruct (fd-actions
             (:constructor %make-fd-actions))
  "A struct representing a sequence of actions to perform on file
descriptors."
  (actions (make-extensible-vector)))

(defun make-fd-actions ()
  "Create an empty instance of the `fd-actions' struct type."
  (%make-fd-actions))

(defstruct fd-action-close
  fd)

(defun fd-actions-add-close (actions fd)
  "Add a close action to the given `fd-actions'.

`fd' is the file descriptor to close."
  (vector-push-extend (make-fd-action-close :fd fd) (fd-actions-actions actions)))

(defstruct fd-action-dup2
  fd1
  fd2)

(defun fd-actions-add-dup2 (actions fd1 fd2)
  "Add a dup2 action to the given `fd-actions'.

`fd1' is the first argument to dup2.  `fd2' is the second argument."
  (vector-push-extend (make-fd-action-dup2 :fd1 fd1 :fd2 fd2) (fd-actions-actions actions)))

(define-foreign-type fd-actions-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser fd-actions))

(defmethod translate-to-foreign ((lisp-actions fd-actions) (type fd-actions-type))
  (let ((actions (%%make-fd-actions)))
    (loop :for action :across (fd-actions-actions lisp-actions) :do
       (etypecase action
         (fd-action-close
          (%%fd-actions-add-close actions (fd-action-close-fd action)))
         (fd-action-dup2
          (%%fd-actions-add-dup2 actions (fd-action-dup2-fd1 action) (fd-action-dup2-fd2 action)))))
    actions))

(defmethod free-translated-object (translated (type fd-actions-type) param)
  (declare (ignore param type))
  (%%destroy-fd-actions translated))

(defun non-zero-p (int)
  (not (zerop int)))

(define-c-wrapper (%shcl-spawn "shcl_spawn" :library shcl-support) (:int 'non-zero-p)
  (pid (:pointer pid-t))
  (path :string)
  (search :int)
  (working-directory-fd :int)
  (fd-actions fd-actions)
  (argv string-table)
  (envp string-table))

(defun shcl-spawn (path search-p working-directory-fd fd-actions argv envp)
  "Spawn a new process.

`path' is the path to the binary to run.  If `search-p' is non-nil,
then the value of the PATH environment variable in `envp' will be used
to search for the binary.

`working-directory-fd' is an integer representing the file descriptor
for the desired working directory of the child process.

`fd-actions' is an instance of the `fd-actions' struct type.  All the
actions contained within the struct will be executed in order.

`argv' is a sequence of strings that represent the arguments the
program should receive.  Note that you are responsible for including
the program name as the first element in the sequence.

`envp' is a sequence of strings describing the desired process
environment.  Each string should be of the form \"VAR=VALUE\"."
  (with-foreign-object (pid 'pid-t)
    (%shcl-spawn pid path (if search-p 1 0) working-directory-fd fd-actions argv envp)
    (mem-ref pid 'pid-t)))
