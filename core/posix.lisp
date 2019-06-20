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

(defpackage :shcl/core/posix
  (:use
   :common-lisp :cffi :trivial-garbage :shcl/core/posix-types
   :shcl/core/utility :shcl/core/support)
  (:import-from :fset)
  (:export
   #:do-environment #:copy-environment #:do-directory-contents #:dir-ptr
   #:fdopendir #:closedir #:dirfd #:readdir
   #:posix-read #:strlen #:posix-write #:exit
   #:waitpid #:dup #:getpid #:posix-open #:openat #:fcntl #:posix-close
   #:pipe #:fstat #:syscall-error #:syscall-errno #:file-ptr #:fdopen
   #:fclose #:fileno #:wifexited #:wifstopped #:wifsignaled #:wexitstatus
   #:wtermsig #:wstopsig #:faccessat #:gethostname #:sysconf #:get-passwd-for-uid
   #:getuid))
(in-package :shcl/core/posix)

(optimization-settings)

(defun not-negative-p (number)
  "Returns non-nil if `number' is non-negative."
  (not (minusp number)))

(defun not-negative-1-p (number)
  "Returns non-nil iff `number' is not -1."
  (not (equal -1 number)))

(defmacro do-environment ((var &optional result) &body body)
  "Iterate through the process environment, repeatedly binding `var'
to the environment binding strings."
  (let ((environment-pointer (gensym "ENVIRONMENT-POINTER"))
        (index (gensym "INDEX")))
    `(loop :with ,environment-pointer = environ
           :with ,index = 0
           :until (null-pointer-p (mem-aref ,environment-pointer :pointer ,index))
           :do
              (let ((,var (mem-aref ,environment-pointer :string ,index)))
                ,@body)
           :do (incf ,index)
           :finally (return ,result))))

(defun copy-environment ()
  "Return a sequence containing all the strings in the process
environment."
  (let ((result (make-extensible-vector :element-type 'string)))
    (do-environment (string)
      (vector-push-extend string result))
    result))

(define-foreign-type dir-ptr ()
  ()
  (:actual-type :pointer)
  (:simple-parser dir-ptr)
  (:documentation
   "The POSIX DIR * type."))

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
  "This function is a wrapper around the readdir C function."
  (setf errno 0)
  (%readdir dirp))

(defmacro do-directory-contents ((file-name dir-ptr &optional result) &body body)
  "Use `readdir' to iterate across the files contained within `dir-ptr'.

`body' is repeatedly evaluated with `file-name' bound to each
successive file found in `dir-ptr'.  When no more files are found,
`result' is evaluated and returned.  Iteration can be aborted early by
returning from the nil block (i.e. using `return').

Note that both \".\" and \"..\" are included in the directory
traversal.  You should be prepared to handle them accordingly."
  (let ((dir (gensym "DIR"))
        (dirent (gensym "DIRENT")))
    `(let ((,dir ,dir-ptr))
       (loop
          (let ((,dirent (readdir ,dir)))
            (when (null-pointer-p ,dirent)
              (return ,result))
            (let ((,file-name (foreign-string-to-lisp (foreign-slot-pointer ,dirent '(:struct dirent) 'd-name))))
              ,@body))))))

(define-c-wrapper (%posix-read "read") (ssize-t #'not-negative-1-p)
  (fd :int)
  (buf :pointer)
  (count size-t))

(defun posix-read (fd count &key binary)
  "Read a specific number of bytes from the given fd.

This function doesn't try to prevent multi-byte characters from being
split.  If you are using an encoding with characters requiring
multiple bytes, you shouldn't use this function in non-binary mode."
  ;; TODO: Stop using non-binary posix-read
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
  "Write `count' bytes starting at `ptr' to `fd'"
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
  "Write a string to a file descriptor"
  (with-foreign-string (ptr string)
    (posix-write-foreign fd ptr (strlen ptr))))

(defun posix-write-bytes (fd bytes)
  "Write bytes to a file descriptor"
  (with-foreign-object (ptr :unsigned-char (length bytes))
    (loop :for index :below (length bytes) :do
       (setf (mem-aref ptr :unsigned-char index) (aref bytes index)))
    (posix-write-foreign fd ptr (length bytes))))

(defun posix-write (fd buffer)
  "Write to a file descriptor.

Strings or unsigned-char arrays are supported."
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
  "This is a wrapper around the waitpid C function.

The output parameters are returned as multiuple values."
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
  "This is a wrapper around the open C function."
  (if mode
      (%open pathname flags mode-t mode)
      (%open pathname flags)))

(define-c-wrapper (%openat "openat") (:int #'not-negative-1-p)
  (dirfd :int)
  (pathname :string)
  (flags :int)
  &rest)

(defun openat (dirfd pathname flags &optional mode)
  "This is a wrapper around the openat C function."
  (if mode
      (%openat dirfd pathname flags mode-t mode)
      (%openat dirfd pathname flags)))

(define-c-wrapper (fcntl "fcntl") (:int #'not-negative-1-p)
  (fildes :int)
  (cmd :int)
  &rest)

(define-c-wrapper (posix-close "close") (:int #'not-negative-1-p)
  (fd :int))

(define-c-wrapper (%pipe "pipe") (:int #'not-negative-1-p)
  (fildes (:pointer :int)))

(defun pipe ()
  "This is a wrapper around the pipe C function.

The pipe file descriptors are returned as multiple values."
  (with-foreign-object (fildes :int 2)
    (%pipe fildes)
    (values
     (mem-aref fildes :int 0)
     (mem-aref fildes :int 1))))

(defmacro define-simple-struct-class (name-and-type)
  "This macro defines a class which represents the given struct type."
  (unless (consp name-and-type)
    (setf name-and-type `(,name-and-type (:struct ,name-and-type))))

  (destructuring-bind (lisp-name type) name-and-type
    (labels
        ((slot-definition (name)
           `(,name
             :documentation
             ,(format nil "This slot contains the value stored in the ~A slot of the foreign struct." name))))
      (let* ((structure-slot-names (foreign-slot-names type))
             (slots (mapcar #'slot-definition structure-slot-names))
             (instance (gensym "INSTANCE"))
             (slot-names (gensym "SLOT-NAMES"))
             (pointer (gensym "POINTER")))
        `(progn
           (defclass ,lisp-name ()
             ,slots
             (:documentation
              ,(format nil "Instances of this class represent instances of the ~A CFFI type." type)))
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
  "This is a wrapper around the fstat C function.

The output parameter is returned as an instance of the `stat' class."
  (with-foreign-object (buf '(:struct stat))
    (%fstat fd buf)
    (make-instance 'stat :pointer buf)))

(define-foreign-type file-ptr ()
  ()
  (:actual-type :pointer)
  (:simple-parser file-ptr)
  (:documentation
   "The C standard library FILE * type."))

(define-c-wrapper (fdopen "fdopen") (file-ptr (lambda (p) (not (null-pointer-p p))))
  (fd :int)
  (mode :string))

(define-c-wrapper (fclose "fclose") (:int #'zerop)
  (stream file-ptr))

(define-c-wrapper (fileno "fileno") (:int #'not-negative-1-p)
  (stream file-ptr))

(define-c-wrapper (faccessat "faccessat") (:int #'not-negative-1-p)
  (fd :int)
  (path :string)
  (amode :int)
  (flag :int))

(define-c-wrapper (%gethostname "gethostname") (:int #'not-negative-1-p)
  (name (:pointer :char))
  (len size-t))

(defun gethostname ()
  "This is a wrapper around the POSIX gethostname function.

The hostname is returned as a lisp string."
  (with-foreign-object (name :char (1+ host-name-max))
    (%gethostname name host-name-max)
    (foreign-string-to-lisp name :max-chars host-name-max)))

(define-c-wrapper (%sysconf "sysconf") (:long (lambda (val)
                                                (or (not (equal -1 val))
                                                    (zerop errno))))
  (name :int))

(defun sysconf (name)
  "This is a wrapper around the POSIX sysconf function."
  (setf errno 0)
  (%sysconf name))

(define-c-wrapper (%getpwuid-r "getpwuid_r") (:int)
  (uid uid-t)
  (pwd (:pointer (:struct passwd)))
  (buffer (:pointer :char))
  (bufsize size-t)
  (result (:pointer (:pointer (:struct passwd)))))

(defun getpwuid-r (uid pwd buffer bufsize result)
  (loop
    (let ((return-code (%getpwuid-r uid pwd buffer bufsize result)))
      (case return-code
        (0
         (return-from getpwuid-r return-code))
        (eintr) ;; Let's try that again
        (otherwise
         (error 'syscall-error :errno return-code :function 'getpwuid-r))))))

(defun call-with-getpwuid-r (uid function)
  (let* ((suggested-size (sysconf -sc-getpw-r-size-max))
         (buffer-size (if (equal -1 suggested-size)
                          1024
                          suggested-size)))
    (with-foreign-object (passwd '(:struct passwd))
      (with-foreign-object (passwd-pointer '(:pointer (:struct passwd)))
        (tagbody
         again
           (with-foreign-object (buffer :char buffer-size)
             (handler-bind
                 ((syscall-error
                    (lambda (e)
                      (when (equal (syscall-errno e) erange)
                        (setf buffer-size (* buffer-size 2))
                        (go again)))))
               (getpwuid-r uid passwd buffer buffer-size passwd-pointer))
             (let ((returned-passwd (mem-ref passwd-pointer '(:pointer (:struct passwd)))))
               (assert (or (null-pointer-p returned-passwd)
                           (pointer-eq passwd returned-passwd)))
               (return-from call-with-getpwuid-r
                 (funcall function returned-passwd)))))))))

(defmacro with-getpwuid-r ((passwd-pointer uid) &body body)
  `(call-with-getpwuid-r ,uid (lambda (,passwd-pointer) ,@body)))

(defclass passwd ()
  ((pw-name
    :initarg :pw-name
    :initform nil
    :type (or null string)
    :reader pw-name)
   (pw-uid
    :initarg :pw-uid
    :initform 0
    :type integer
    :reader pw-uid)
   (pw-gid
    :initarg :pw-gid
    :initform 0
    :type integer
    :reader pw-gid)
   (pw-dir
    :initarg :pw-dir
    :initform nil
    :type (or null string)
    :reader pw-dir)
   (pw-shell
    :initarg :pw-shell
    :initform nil
    :type (or null string)
    :reader pw-shell)))

(defun get-passwd-for-uid (uid)
  "Retrieve an instance of the lisp `passwd' class for the given uid.

This function uses POSIX's getpwuid_r function to retrieve a passwd
struct and then turns it into a lisp object for your consumption."
  (with-getpwuid-r (passwd-pointer uid)
    (macrolet
        ((slot (foreign-slot-name)
           `(foreign-slot-value passwd-pointer '(:struct passwd) ',foreign-slot-name))
         (stringify (foreign-slot-name)
           (let ((pointer (gensym "POINTER")))
             `(let ((,pointer (foreign-slot-value passwd-pointer '(:struct passwd) ',foreign-slot-name)))
                (if (null-pointer-p ,pointer)
                    nil
                    (foreign-string-to-lisp ,pointer))))))
      (make-instance 'passwd
                     :pw-name (stringify pw-name)
                     :pw-uid (slot pw-uid)
                     :pw-gid (slot pw-gid)
                     :pw-dir (stringify pw-dir)
                     :pw-shell (stringify pw-shell)))))

(define-c-wrapper (getuid "getuid") (uid-t))
