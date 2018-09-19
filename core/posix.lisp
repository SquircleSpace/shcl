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
   :shcl/core/utility :shcl/core/iterator :shcl/core/support)
  (:import-from :fset)
  (:export
   #:environment-iterator #:do-directory-contents #:dir-ptr #:fdopendir
   #:closedir #:dirfd #:readdir
   #:posix-read #:strlen #:posix-write #:exit
   #:waitpid #:dup #:getpid #:posix-open #:openat #:fcntl #:posix-close
   #:pipe #:fstat #:fstatat #:syscall-error #:syscall-errno #:file-ptr #:fdopen
   #:fclose #:fileno #:wifexited #:wifstopped #:wifsignaled #:wexitstatus
   #:wtermsig #:wstopsig))
(in-package :shcl/core/posix)

(optimization-settings)

(defun not-negative-p (number)
  "Returns non-nil if `number' is non-negative."
  (not (minusp number)))

(defun not-negative-1-p (number)
  "Returns non-nil iff `number' is not -1."
  (not (equal -1 number)))

(defun environment-iterator ()
  "Returns an iterator that emits the bindings in the current process
environment.

This function assumes that the process environment will not change
during iteration."
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

(define-c-wrapper (%fstatat "fstatat") (:int #'not-negative-1-p)
  (fd :int)
  (path :string)
  (buf (:pointer (:struct stat)))
  (flag :int))

(defun fstatat (fd path flag)
  "This is a wrapper around the fstatat C function.

The output parameter is returned as an instance of the `stat' class."
  (with-foreign-object (buf '(:struct stat))
    (%fstatat fd path buf flag)
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
