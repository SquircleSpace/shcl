(in-package :shcl.posix)

(optimization-settings)

(define-condition syscall-error (error)
  ((errno
    :initform errno
    :accessor syscall-error-errno
    :type integer)
   (function
    :initform nil
    :accessor syscall-error-function)))

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

(defmacro define-c-wrapper ((lisp-name c-name) (return-type &optional (error-checker 'pass)) &body arg-descriptions)
  (let ((lisp-impl-name (intern (concatenate 'string "%" (symbol-name lisp-name))))
        (result (gensym "RESULT"))
        (args (mapcar #'first arg-descriptions)))
    `(progn
       (defcfun (,lisp-impl-name ,c-name) ,return-type
         ,@arg-descriptions)
       (defun ,lisp-name (,@args)
         (let ((,result (,lisp-impl-name ,@args)))
           (unless (funcall ,error-checker ,result)
             (error 'syscall-error :function ',lisp-name))
           ,result)))))

(define-foreign-type string-table-type ()
  ((size
    :initarg :size
    :initform nil))
  (:actual-type :pointer)
  (:simple-parser string-table))

(defmethod translate-to-foreign ((sequence fset:seq) (type string-table-type))
  (with-slots (size inner-type) type
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
