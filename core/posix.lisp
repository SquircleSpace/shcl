(defpackage :shcl/core/posix
  (:use :common-lisp :cffi :trivial-garbage :shcl/core/posix-types :shcl/core/utility
        :shcl/core/support :bordeaux-threads)
  (:import-from :fset)
  (:export
   #:environment-iterator #:dir-ptr #:fdopendir
   #:closedir #:dirfd #:readdir #:open-fds #:compiler-owned-fds
   #:posix-read #:strlen #:posix-write #:exit
   #:waitpid #:dup #:getpid #:posix-open #:openat #:fcntl #:posix-close
   #:pipe #:fstat #:syscall-error #:syscall-errno #:file-ptr #:fdopen #:fclose
   #:fileno #:wifexited #:wifstopped #:wifsignaled #:wexitstatus #:wtermsig
   #:wstopsig))
(in-package :shcl/core/posix)

(optimization-settings)

(define-condition syscall-error (error)
  ((errno
    :initform errno
    :reader syscall-errno
    :type integer)
   (function
    :initform nil
    :initarg :function
    :accessor syscall-error-function))
  (:report (lambda (c s)
             (format s "Encountered an error (~A) in ~A.  ~A"
                     (syscall-errno c)
                     (syscall-error-function c)
                     (strerror (syscall-errno c))))))

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

(defun environment-iterator ()
  (let ((environment-pointer environ)
        (index 0))
    (make-iterator ()
      (when (null-pointer-p (mem-aref environment-pointer :pointer index))
        (stop))

      (let ((result (mem-aref environment-pointer :string index)))
        (incf index)
        (emit result)))))

(define-foreign-type dir-ptr ()
  ()
  (:actual-type :pointer)
  (:simple-parser dir-ptr)
  (:documentation
   "The POSIX DIR * type."))

(define-c-wrapper (opendir "opendir") (dir-ptr (lambda (x) (not (null-pointer-p x))))
  (name :string))

(define-c-wrapper (fdopendir "fdopendir") (dir-ptr (lambda (x) (not (null-pointer-p x))))
  (fd :int))

(define-c-wrapper (closedir "closedir") (:int #'zerop)
  (dirp dir-ptr))

(define-c-wrapper (dirfd "dirfd") (:int #'not-negative-p)
  (dirp dir-ptr))

(define-c-wrapper (%readdir "readdir") ((:pointer (:struct dirent))
                                        (lambda (x)
                                          (or (not (null-pointer-p x))
                                              (zerop errno))))
  (dirp dir-ptr))

(defun readdir (dirp)
  (setf errno 0)
  (%readdir dirp))

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
               (when (equal eintr (syscall-errno e))
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
             (return-from posix-read array)))))))

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
               (when (equal eintr (syscall-errno e))
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

(defmacro define-simple-struct-class (name-and-type)
  (unless (consp name-and-type)
    (setf name-and-type `(,name-and-type (:struct ,name-and-type))))

  (destructuring-bind (lisp-name type) name-and-type
    (labels
        ((slot-definition (name) `(,name)))
      (let* ((structure-slot-names (foreign-slot-names type))
             (slots (mapcar #'slot-definition structure-slot-names))
             (instance (gensym "INSTANCE"))
             (slot-names (gensym "SLOT-NAMES"))
             (pointer (gensym "POINTER")))
        `(progn
           (defclass ,lisp-name ()
             ,slots)
           (defmethod shared-initialize :after ((,instance ,lisp-name) ,slot-names &key ((:pointer ,pointer)) &allow-other-keys)
             (declare (ignore ,slot-names))
             (when ,pointer
               ,@(loop :for slot-name :in structure-slot-names :collect
                    `(setf (slot-value ,instance ',slot-name) (foreign-slot-value ,pointer ',type ',slot-name)))))
           ',lisp-name)))))

(define-simple-struct-class stat)

(define-c-wrapper (%fstat "fstat") (:int #'not-negative-1-p)
  (fd :int)
  (buf (:pointer (:struct stat))))

(defun fstat (fd)
  (with-foreign-object (buf '(:struct stat))
    (%fstat fd buf)
    (make-instance 'stat :pointer buf)))

(define-c-wrapper (%strerror "strerror") (:string)
  (err :int))

(defvar *errno-lock* (make-lock))

(defun strerror (err)
  (with-lock-held (*errno-lock*)
    (%strerror err)))

(define-foreign-type file-ptr ()
  ()
  (:actual-type :pointer)
  (:simple-parser file-ptr)
  (:documentation
   "The C standard library FILE * type."))

(defcfun (fdopen "fdopen") file-ptr
  (fd :int)
  (mode :string))

(defcfun (fclose "fclose") :int
  (stream file-ptr))

(defcfun (fileno "fileno") :int
  (stream file-ptr))
