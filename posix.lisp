(defpackage :shcl/posix
  (:use :common-lisp :cffi :trivial-garbage :shcl/posix-types :shcl/utility
        :shcl/support/support :bordeaux-threads)
  (:import-from :fset)
  (:export
   #:posix-spawn-file-actions-init #:posix-spawn-file-actions-destroy
   #:with-posix-spawn-file-actions #:posix-spawn-file-actions-addclose
   #:posix-spawn-file-actions-addopen #:posix-spawn-file-actions-adddup2
   #:posix-spawnp #:posix-spawnattr-init #:posix-spawnattr-destroy
   #:with-posix-spawnattr #:environment-iterator #:fchdir #:open-fds
   #:compiler-owned-fds #:posix-read #:posix-write #:fork #:_exit #:exit
   #:waitpid #:forked #:dup #:getpid #:posix-open #:openat #:fcntl #:posix-close
   #:pipe #:syscall-error #:wifexited #:wifstopped #:wifsignaled #:wexitstatus
   #:wtermsig #:wstopsig))
(in-package :shcl/posix)

(optimization-settings)

(define-condition syscall-error (error)
  ((errno
    :initform errno
    :accessor syscall-error-errno
    :type integer)
   (function
    :initform nil
    :initarg :function
    :accessor syscall-error-function))
  (:report (lambda (c s)
             (format s "Encountered an error (~A) in ~A.  ~A"
                     (syscall-error-errno c)
                     (syscall-error-function c)
                     (strerror (syscall-error-errno c))))))

(defstruct gc-wrapper
  pointer)

(defun wrapped-foreign-alloc (type &rest args &key initial-element initial-contents count null-terminated-p)
  (declare (ignore initial-element initial-contents count null-terminated-p))
  (let* ((the-pointer (apply #'foreign-alloc type args))
         (the-wrapper (make-gc-wrapper :pointer the-pointer)))
    (finalize the-wrapper (lambda () (foreign-free the-pointer)))
    the-wrapper))

(defun wrapped-foreign-free (pointer)
  (foreign-free (gc-wrapper-pointer pointer))
  (setf (gc-wrapper-pointer pointer) (null-pointer))
  (cancel-finalization pointer)
  nil)

(defun wrap (pointer &optional extra-finalizer)
  (let ((struct (make-gc-wrapper :pointer pointer)))
    (when extra-finalizer
      (finalize struct extra-finalizer))
    struct))

(defun unwrap (pointer)
  (gc-wrapper-pointer pointer))

(defun pass (value)
  (declare (ignore value))
  t)

(defun not-negative-p (number)
  (not (minusp number)))

(defun not-negative-1-p (number)
  (not (equal -1 number)))

(defmacro define-c-wrapper ((lisp-name c-name) (return-type &optional (error-checker ''pass)) &body arg-descriptions)
  (let ((lisp-impl-name (intern (concatenate 'string "%" (symbol-name lisp-name))))
        (result (gensym "RESULT")))
    (labels
        ((defun-based ()
           (let ((args (mapcar 'first arg-descriptions)))
             `(defun ,lisp-name (,@args)
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
         (defcfun (,lisp-impl-name ,c-name) ,return-type
           ,@arg-descriptions)
         ,(wrapper)))))

(define-c-wrapper (posix-spawnp "posix_spawnp") (:int #'zerop)
  (pid (:pointer pid-t))
  (file :string)
  (file-actions (:pointer (:struct posix-spawn-file-actions-t)))
  (attrp (:pointer (:struct posix-spawnattr-t)))
  (argv string-table)
  (envp string-table))

(define-c-wrapper (posix-spawn-file-actions-init "posix_spawn_file_actions_init") (:int #'zerop)
  (file-actions (:pointer (:struct posix-spawn-file-actions-t))))

(define-c-wrapper (posix-spawn-file-actions-destroy "posix_spawn_file_actions_destroy") (:int #'zerop)
  (file-actions (:pointer (:struct posix-spawn-file-actions-t))))

(defmacro with-posix-spawn-file-actions ((symbol) &body body)
  `(with-foreign-object (,symbol '(:struct posix-spawn-file-actions-t))
     (posix-spawn-file-actions-init ,symbol)
     (unwind-protect (progn ,@body)
       (posix-spawn-file-actions-destroy ,symbol))))

(define-c-wrapper (posix-spawn-file-actions-addclose "posix_spawn_file_actions_addclose") (:int #'zerop)
  (file-actions (:pointer (:struct posix-spawn-file-actions-t)))
  (fildes :int))

(define-c-wrapper (posix-spawn-file-actions-addopen "posix_spawn_file_actions_addopen") (:int #'zerop)
  (file-actions (:pointer (:struct posix-spawn-file-actions-t)))
  (fildes :int)
  (path :string)
  (oflag :int)
  (mode mode-t))

(define-c-wrapper (posix-spawn-file-actions-adddup2 "posix_spawn_file_actions_adddup2") (:int #'zerop)
  (file-actions (:pointer (:struct posix-spawn-file-actions-t)))
  (fildes :int)
  (newfildes :int))


(define-c-wrapper (posix-spawnattr-init "posix_spawnattr_init") (:int #'zerop)
  (attr (:pointer (:struct posix-spawnattr-t))))

(define-c-wrapper (posix-spawnattr-destroy "posix_spawnattr_destroy") (:int #'zerop)
  (attr (:pointer (:struct posix-spawnattr-t))))

(defmacro with-posix-spawnattr ((symbol) &body body)
  `(with-foreign-object (,symbol '(:struct posix-spawnattr-t))
     (posix-spawnattr-init ,symbol)
     (unwind-protect (progn ,@body)
       (posix-spawnattr-destroy ,symbol))))

(defun environment-iterator ()
  (let ((environment-pointer environ)
        (index 0))
    (make-iterator ()
      (when (null-pointer-p (mem-aref environment-pointer :pointer index))
        (stop))

      (let ((result (mem-aref environment-pointer :string index)))
        (incf index)
        (emit result)))))

(define-c-wrapper (opendir "opendir") (:pointer (lambda (x) (not (null-pointer-p x))))
  (name :string))

(define-c-wrapper (closedir "closedir") (:int #'zerop)
  (dirp :pointer))

(define-c-wrapper (dirfd "dirfd") (:int #'not-negative-p)
  (dirp :pointer))

(define-c-wrapper (%readdir "readdir") ((:pointer (:struct dirent))
                                        (lambda (x)
                                          (or (not (null-pointer-p x))
                                              (zerop errno))))
  (dirp :pointer))

(defun readdir (dirp)
  (setf errno 0)
  (%readdir dirp))

(define-c-wrapper (%fchdir "fchdir") (:int #'not-negative-1-p)
  (fildes :int))

(defun fchdir (fildes)
  (tagbody
   retry
     (handler-bind
         ((syscall-error
           (lambda (e)
             (when (equal eintr (syscall-error-errno e))
               (go retry)))))
       (%fchdir fildes))))

(defun open-fds ()
  (let ((result (make-extensible-vector :element-type 'integer))
        dir-fd
        dir)
    (unwind-protect
         (progn
           (setf dir (opendir "/dev/fd"))
           (setf dir-fd (dirfd dir))
           (loop
              (block again
                (let ((dirent (readdir dir))
                      name-ptr)
                  (when (null-pointer-p dirent)
                    (return))
                  (setf name-ptr (foreign-slot-pointer dirent '(:struct dirent) 'd-name))
                  (let ((s (foreign-string-to-lisp name-ptr)))
                    (when (equal #\. (aref s 0))
                      (return-from again))
                    (vector-push-extend (parse-integer s)
                                        result))))))
      (when dir
        (closedir dir)))
    (remove dir-fd result)))

(defun compiler-owned-fds ()
  #+sbcl
  (vector (sb-sys:fd-stream-fd sb-sys:*tty*))
  #-sbcl
  (progn
    (warn "Unsupported compiler.  Can't determine which fds the compiler owns.")
    #()))

(define-c-wrapper (%posix-read "read") (ssize-t #'not-negative-1-p)
  (fd :int)
  (buf :pointer)
  (count size-t))

(defun posix-read (fd count &key binary)
  (with-foreign-object (buf :char count)
    (tagbody
     retry
       (handler-bind
           ((syscall-error
             (lambda (e)
               (when (equal eintr (syscall-error-errno e))
                 (go retry)))))
         (let ((length (%posix-read fd buf count)))
           (unless binary
             (return-from posix-read
               (if (zerop length)
                   ""
                   (nth-value 0 (foreign-string-to-lisp buf :max-chars length)))))
           (let ((array (make-array length :element-type 'unsigned-byte)))
             (loop :for index :below length :do
                (setf (aref array index) (mem-aref buf :unsigned-char index)))
             array))))))

(define-c-wrapper (strlen "strlen") (size-t)
  (s :string))

(define-c-wrapper (%posix-write "write") (ssize-t #'not-negative-1-p)
  (fd :int)
  (buf :pointer)
  (count size-t))

(defun posix-write-foreign (fd ptr count)
  (let ((original-count count))
    (tagbody
     retry
       (handler-bind
           ((syscall-error
             (lambda (e)
               (when (equal eintr (syscall-error-errno e))
                 (go retry)))))
         (let* ((written (%posix-write fd ptr count)))
           (decf count written)
           (incf-pointer ptr written)
           (when (zerop count)
             (return-from posix-write-foreign original-count))
           (assert (plusp count)))))))

(defun posix-write-string (fd string)
  (with-foreign-string (ptr string)
    (posix-write-foreign fd ptr (strlen ptr))))

(defun posix-write-bytes (fd bytes)
  (with-foreign-object (ptr :unsigned-char (length bytes))
    (loop :for index :below (length bytes) :do
       (setf (mem-aref ptr :unsigned-char index) (aref bytes index)))
    (posix-write-foreign fd ptr (length bytes))))

(defun posix-write (fd buffer)
  (etypecase buffer
    (string
     (posix-write-string fd buffer))
    ((array (unsigned-byte 8))
     (posix-write-bytes fd buffer))))

(defun fork ()
  #+sbcl (sb-posix:fork)
  #-sbcl (error "Cannot fork on this compiler"))

(define-c-wrapper (_exit "_exit") (:void)
  (status :int))

(define-c-wrapper (exit "exit") (:void)
  (status :int))

(define-c-wrapper (%waitpid "waitpid") (pid-t #'not-negative-1-p)
  (pid pid-t)
  (wstatus (:pointer :int))
  (options :int))

(defun waitpid (pid options)
  (with-foreign-object (status :int)
    (let ((pid-output (%waitpid pid status options)))
      (values pid-output (mem-ref status :int)))))

(defmacro forked (&body body)
  (let ((pid (gensym "PID"))
        (e (gensym "E")))
    `(let ((,pid (fork)))
       (cond
         ((plusp ,pid)
          ,pid)
         ((zerop ,pid)
          (unwind-protect
               (handler-case (progn ,@body)
                 (error (,e)
                   (format *error-output* "ERROR: ~A~%" ,e)
                   (finish-output *error-output*)
                   (_exit 1)))
            (_exit 0)))
         ((minusp ,pid)
          ;; The wrapper around posix fork should have taken care of this
          ;; for us
          (assert nil nil "This is impossible"))))))

(define-c-wrapper (dup "dup") (:int #'not-negative-1-p)
  (fd :int))

(define-c-wrapper (getpid "getpid") (pid-t))

(define-c-wrapper (%open "open") (:int #'not-negative-1-p)
  (pathname :string)
  (flags :int)
  &rest)

(defun posix-open (pathname flags &optional mode)
  (if mode
      (%open pathname flags mode-t mode)
      (%open pathname flags)))

(define-c-wrapper (%openat "openat") (:int #'not-negative-1-p)
  (dirfd :int)
  (pathname :string)
  (flags :int)
  &rest)

(defun openat (dirfd pathname flags &optional mode)
  (if mode
      (%openat dirfd pathname flags mode-t mode)
      (%openat dirfd pathname flags)))

(define-c-wrapper (fcntl "fcntl") (:int #'not-negative-1-p)
  (fildes :int)
  (cmd :int)
  &rest)

(define-c-wrapper (%close "close") (:int #'not-negative-1-p)
  (fd :int))

(defun posix-close (fd)
  (%close fd))

(define-c-wrapper (%pipe "pipe") (:int #'not-negative-1-p)
  (fildes (:pointer :int)))

(defun pipe ()
  (with-foreign-object (fildes :int 2)
    (%pipe fildes)
    (values
     (mem-aref fildes :int 0)
     (mem-aref fildes :int 1))))

(define-c-wrapper (%strerror "strerror") (:string)
  (err :int))

(defvar *errno-lock* (make-lock))

(defun strerror (err)
  (with-lock-held (*errno-lock*)
    (%strerror err)))
