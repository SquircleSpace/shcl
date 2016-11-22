(defpackage :shcl/support/support
  (:use :common-lisp :cffi :shcl/utility :shcl/posix-types)
  (:export
   #:wifexited #:wifstopped #:wifsignaled #:wexitstatus #:wtermsig #:wstopsig
   #:string-table #:fd-actions #:make-fd-actions #:fd-actions-add-close
   #:fd-actions-add-dup2 #:shcl-spawn))
(in-package :shcl/support/support)

(optimization-settings)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library shcl-support
    (:linux (:default "libshcl-support") :search-path ".")))

(use-foreign-library shcl-support)

(defcfun (%wifexited "wifexited" :library shcl-support) :int
  (status :int))
(defun wifexited (status)
  (not (zerop (%wifexited status))))

(defcfun (%wifstopped "wifstopped" :library shcl-support) :int
  (status :int))
(defun wifstopped (status)
  (not (zerop (%wifstopped status))))

(defcfun (%wifsignaled "wifsignaled" :library shcl-support) :int
  (status :int))
(defun wifsignaled (status)
  (not (zerop (%wifsignaled status))))

(defcfun (wexitstatus "wexitstatus" :library shcl-support) :int
  (status :int))

(defcfun (wtermsig "wtermsig" :library shcl-support) :int
  (status :int))

(defcfun (wstopsig "wstopsig" :library shcl-support) :int
  (status :int))

(define-foreign-type string-table-type ()
  ((size
    :initarg :size
    :initform nil))
  (:actual-type :pointer)
  (:simple-parser string-table))

(defmethod translate-to-foreign ((sequence fset:seq) (type string-table-type))
  (with-slots (size) type
    (let ((seq-size (fset:size sequence))
          table
          side-table)
      (when size
        (assert (equal seq-size size)))
      (let (success)
        (unwind-protect
             (let ((index 0))
               (setf table (foreign-alloc :string :initial-element (null-pointer) :count seq-size :null-terminated-p t))
               (setf side-table (make-array seq-size :initial-element nil))
               (fset:do-seq (thing sequence)
                 (multiple-value-bind (converted-value param) (convert-to-foreign thing :string)
                   (setf (mem-aref table :string index) converted-value)
                   (setf (aref side-table index) param)
                   (incf index)))
               (setf success t)
               (values table side-table))
          (unless success
            (if side-table
                (free-translated-object table type side-table)
                (foreign-free table))))))))

(defmethod free-translated-object (translated (type string-table-type) param)
  (loop :for index :below (length param) :do
     (free-converted-object (mem-aref translated :pointer index) :string (aref param index)))
  (foreign-free translated))

(defcfun (%%make-fd-actions "make_shcl_fd_actions") :pointer)
(defcfun (%%destroy-fd-actions "destroy_shcl_fd_actions") :void
  (actions :pointer))
(defcfun (%%fd-actions-add-close "shcl_fd_actions_add_close") :void
  (actions :pointer)
  (fd :int))
(defcfun (%%fd-actions-add-dup2 "shcl_fd_actions_add_dup2") :void
  (actions :pointer)
  (fd1 :int)
  (fd2 :int))

(defstruct (fd-actions
             (:constructor %make-fd-actions))
  (actions (make-extensible-vector)))

(defun make-fd-actions ()
  (%make-fd-actions))

(defstruct fd-action-close
  fd)

(defun fd-actions-add-close (actions fd)
  (vector-push-extend (make-fd-action-close :fd fd) (fd-actions-actions actions)))

(defstruct fd-action-dup2
  fd1
  fd2)

(defun fd-actions-add-dup2 (actions fd1 fd2)
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

(defcfun (%shcl-spawn "shcl_spawn" :library shcl-support) :int
  (pid (:pointer pid-t))
  (path :string)
  (search :int)
  (working-directory-fd :int)
  (fd-actions fd-actions)
  (argv string-table)
  (envp string-table))

(defun shcl-spawn (path search-p working-directory-fd fd-actions argv envp)
  (with-foreign-object (pid 'pid-t)
    (let ((result (%shcl-spawn pid path (if search-p 1 0) working-directory-fd fd-actions argv envp)))
      (when (zerop result)
        (error "spawn failed"))
      (mem-ref pid 'pid-t))))
