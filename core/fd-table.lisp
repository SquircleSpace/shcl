(defpackage :shcl/core/fd-table
  (:use :common-lisp :alexandria :bordeaux-threads :trivial-gray-streams
        :shcl/core/utility :shcl/core/posix :shcl/core/posix-types
        :shcl/core/shell-environment)
  (:shadowing-import-from :alexandria #:when-let #:when-let*)
  (:export
   #:with-safe-fd-manipulation #:track #:forget
   #:fd-retain #:fd-release #:fd-autorelease
   #:with-fd-scope #:with-living-fds #:dup-retained #:open-retained
   #:openat-retained #:pipe-retained #:with-pipe #:bind-fd
   #:get-fd #:simplify-fd-bindings #:with-fd-streams #:fd-stream
   #:make-fd-stream))
(in-package :shcl/core/fd-table)

(optimization-settings)

(define-once-global %extant-fds% (make-hash-table)
  (:documentation
   "The set of fds that are currently alive."))
(define-once-global %extant-fds-lock% (make-recursive-lock)
  (:documentation
   "The lock protecting `%extant-fds%'."))

(defmacro with-safe-fd-manipulation (&body body)
  (let ((fd (gensym "FD")))
    `(with-recursive-lock-held (%extant-fds-lock%)
       (labels
           ((track (,fd)
              (debug-log status "TRACK ~A" ,fd)
              (setf (gethash ,fd %extant-fds%) t)
              ,fd)
            (forget (,fd)
              (debug-log status "FORGET ~A" ,fd)
              (remhash ,fd %extant-fds%)
              (values)))
         (declare (dynamic-extent #'track #'forget))
         (declare (ignorable #'track #'forget))
         ,@body))))

(define-once-global %fd-retain-count-table% (make-hash-table)
  (:documentation
   "This table tracks how many outstanding retains each file
descriptor has."))

(defparameter *autorelease-fd-scope* nil
  "The collection of file descriptors that should be released when the
current fd scope exits.

This is bound by `with-fd-scope'.")

(defparameter *fd-bindings* nil
  "This variable contains information about how fds should be bound
when a new process is spawned.

This variable should not be accessed directly.  You should add
bindings with `bind-fd' and query bindings with `get-fd'.")

(defstruct floating-fd-bindings-inner
  bindings)

(defstruct floating-fd-bindings
  inner)

(defun %fd-managed-p (fd)
  "The non-locking version of `fd-managed-p'"
  (not (not (gethash fd %fd-retain-count-table%))))

(defun fd-managed-p (fd)
  "Returns t iff the given fd is being managed with retain/release."
  (with-safe-fd-manipulation
    (%fd-managed-p fd)))

(define-condition fd-already-managed (error)
  ((fd
    :initarg :fd
    :initform (required)
    :accessor fd-already-managed-fd))
  (:report (lambda (c s) (format s "Can't enter FD ~A into retain/release management again" (fd-already-managed-fd c)))))

(defun manage-new-fd (fd)
  "Enter the given fd into the retain/release table.

To safely use this function, you must
1. Inside the body of a `with-safe-fd-manipulation' form, create and track a new fd
2. Call this function

Any other sequence of events has the potential to introduce a race.
In particular, once an unmanaged fd is added to *fd-bindings*, it must
not become a managed fd."
  (with-safe-fd-manipulation
    (when (gethash fd %fd-retain-count-table%)
      (error 'fd-already-managed :fd fd))
    (setf (gethash fd %fd-retain-count-table%) 1)
    fd))

(define-condition fd-not-managed (error)
  ((fd
    :initarg :fd
    :initform (required)
    :accessor fd-not-managed-fd))
  (:report (lambda (c s) (format s "Can't retain unmanaged FD ~A" (fd-not-managed-fd c)))))

(defun %fd-retain (fd)
  (unless (gethash fd %fd-retain-count-table%)
    (error 'fd-not-managed :fd fd))
  (incf (gethash fd %fd-retain-count-table%))
  fd)

(defun fd-retain (fd)
  "Increment the retain count for the given fd.

The given fd will not be closed in the shell process until the retain
count reaches 0.  See `fd-release'."
  (with-safe-fd-manipulation
    (%fd-retain fd)))

(defun retain-fd-bindings (fd-bindings)
  (with-safe-fd-manipulation
    (fset:do-map (key-fd value-fd fd-bindings)
      (declare (ignore key-fd))
      (when (%fd-managed-p value-fd)
        (%fd-retain value-fd)))))

(defun preserve-fd-bindings ()
  (retain-fd-bindings *fd-bindings*)
  *fd-bindings*)

(defun destroy-fd-bindings (bindings)
  (release-fd-bindings bindings)
  (values))

(defun call-with-fd-bindings (bindings continuation)
  (%with-fd-scope continuation bindings))

(extend-shell-environment
 'fd-table
 'preserve-fd-bindings
 'call-with-fd-bindings
 'destroy-fd-bindings)

(define-condition fd-over-release (error)
  ((fd
    :initarg :fd
    :initform (required)
    :accessor fd-over-release-fd))
  (:report (lambda (c s) (format s "FD ~A was over released" (fd-over-release-fd c)))))

(defun %fd-release (fd)
  "Do the work of `fd-release', but assume the table is already
locked."
  (unless (gethash fd %fd-retain-count-table%)
    (error 'fd-over-release :fd fd))
  (let ((count (decf (gethash fd %fd-retain-count-table%))))
    (when (equal 0 count)
      (debug-log status "CLOSE ~A" fd)
      (with-safe-fd-manipulation
        (posix-close fd)
        (forget fd))
      (remhash fd %fd-retain-count-table%)))
  nil)

(defun fd-release (fd)
  "Decrement the retain count for the given fd.

If the retain count reaches 0, then it will be closed immediately."
  (with-safe-fd-manipulation
    (%fd-release fd)))

(defun release-fd-bindings (fd-bindings)
  (with-safe-fd-manipulation
    (fset:do-map (key-fd value-fd fd-bindings)
      (declare (ignore key-fd))
      (when (%fd-managed-p value-fd)
        (%fd-release value-fd))))
  (values))

(define-condition fd-autorelease-without-scope (error)
  ((fd
    :initarg :fd
    :initform (required)
    :accessor fd-autorelease-without-scope-fd))
  (:report (lambda (c s) (format s "FD ~A was autoreleased without an FD scope in place" (fd-autorelease-without-scope-fd c)))))

(defun fd-autorelease (fd)
  "Release the given fd when the current fd scope exits."
  (unless *autorelease-fd-scope*
    (error 'fd-autorelease-without-scope :fd fd))
  (vector-push-extend fd *autorelease-fd-scope*)
  fd)

(defun %with-fd-scope (fn &optional bindings)
  (let* ((*fd-bindings* (or bindings *fd-bindings* (fset:empty-map)))
         (*autorelease-fd-scope* (make-extensible-vector :element-type 'integer)))
    ;; Retain the new bindings
    (retain-fd-bindings *fd-bindings*)

    (unwind-protect (funcall fn)
      (with-safe-fd-manipulation
        (loop :for fd :across *autorelease-fd-scope* :do
           (%fd-release fd)))
      (release-fd-bindings *fd-bindings*))))

(defmacro with-fd-scope (() &body body)
  "Introduce an fd scope.

Calls to `fd-autorelease' while this fd scope is active will not
decrement fd retain counts until control leaves this fd scope.
Additionally, any manipulations to the fd table `*fd-bindings*' will
be reverted when this scope exits."
  `(%with-fd-scope (lambda () ,@body)))

(defmacro with-living-fds ((fd-list-sym) &body body)
  "Lock the fd table and list all managed file descriptors.

Since this locks the fd table, it is very important to minimize the
amount of work done in the body of this macro.  Ideally, you would
do nothing exept spawn a new process."
  `(with-safe-fd-manipulation
     (let ((,fd-list-sym (hash-table-keys %extant-fds%)))
       (declare (dynamic-extent ,fd-list-sym))
       ,@body)))

(defun dup-retained (fd)
  (with-safe-fd-manipulation
    (let ((new-fd (track (dup fd))))
      (debug-log status "DUP ~A = ~A" new-fd fd)
      (manage-new-fd new-fd))))

(defun open-retained (pathname flags &optional mode)
  "This is a wrapper around the posix open function which
atomically adds the new fd to the fd table and gives it a +1 retain
count."
  (with-safe-fd-manipulation
    (let ((fd (track (posix-open pathname flags mode))))
      (debug-log status "OPEN ~A = ~A" fd pathname)
      (manage-new-fd fd))))

(defun openat-retained (dir-fd pathname flags &optional mode)
  (with-safe-fd-manipulation
    (let ((fd (track (openat dir-fd pathname flags mode))))
      (debug-log status "OPENAT ~A, ~A = ~A" dir-fd fd pathname)
      (manage-new-fd fd))))

(defun pipe-retained ()
  "This is a wrapper around the posix pipe function which atomically
adds the new fds to the fd table and gives them +1 retain counts.

Returns two values: the read-end of the pipe and the write end of the
pipe."
  (with-safe-fd-manipulation
    (multiple-value-bind (read-end write-end) (shcl/core/posix:pipe)
      (track read-end)
      (track write-end)
      (debug-log status "PIPE ~A -> ~A" write-end read-end)
      (values (manage-new-fd read-end) (manage-new-fd write-end)))))

(defmacro with-pipe ((read-end write-end) &body body)
  "Introduce a pipe into the retain table (as if with `pipe-retained')
with a +0 retain count.

That is, you do not need to release the file descriptors produced by
this macro.  You must balance any retains you perform on the given
file descriptors."
  (let ((raw-read-end (gensym "RAW-READ-END"))
        (raw-write-end (gensym "RAW-WRITE-END")))
    `(multiple-value-bind (,raw-read-end ,raw-write-end) (pipe-retained)
       (unwind-protect
            (let ((,read-end ,raw-read-end)
                  (,write-end ,raw-write-end))
              ,@body)
         (fd-release ,raw-read-end)
         (fd-release ,raw-write-end)))))

(defun bind-fd (fd fd-value)
  "Extend `*fd-bindings*' to include a binding for `fd' to `fd-value'

This function doesn't actually modify the given fd in the current
process.  Instead, it will simply store information about what the
named fd should be bound to in spawned processes."
  (check-type fd integer)
  (check-type fd-value integer)
  (unless *fd-bindings*
    (error "Cannot bind without an fd-scope"))
  (debug-log status "BIND ~A = ~A (~A)" fd fd-value *fd-bindings*)
  (with-safe-fd-manipulation
    (let ((old-value (fset:lookup *fd-bindings* fd)))
      (setf (fset:lookup *fd-bindings* fd) fd-value)
      (when (%fd-managed-p fd-value)
        (%fd-retain fd-value))
      (when (and old-value (%fd-managed-p old-value))
        (%fd-release old-value))))
  (values))

(define-condition invalid-fd (error)
  ((fd
    :type integer
    :initarg :fd
    :accessor invalid-fd-fd
    :initform (required)))
  (:report (lambda (c s) (format s "Redirect from invalid fd: ~A~%" (invalid-fd-fd c))))
  (:documentation
   "A condition that indicates that an invalid fd has been
requested.

This represents an error in the shell expression.

An fd is considered invalid if it hasn't been bound by the shell
expression."))

(defun get-fd (fd &key (error-on-closed-fd t))
  "Return the fd (in this process) will be dup'd into to the given
fd (in a spawned subprocesses).

See `bind-fd'."
  (when *fd-bindings*
    (let ((binding (fset:lookup *fd-bindings* fd)))
      (when binding
        (return-from get-fd binding))))
  (when (fd-managed-p fd)
    (error 'invalid-fd :fd fd))
  (when error-on-closed-fd
    (handler-case (fcntl fd f-getfd)
      (syscall-error ()
        (error 'invalid-fd :fd fd))))
  fd)

(defun simplify-fd-bindings-default-new-fd-fn (fd)
  (fd-autorelease (dup-retained fd)))

(defun simplify-fd-bindings (&key (fd-bindings *fd-bindings*) (new-fd-fn #'simplify-fd-bindings-default-new-fd-fn))
  "Transform the given bindings so that there is no overlap between
fds that will be bound in the child process and the source fds in this
process.

The goal is to make it so that we can naively use dup2 to create the
desired state in the child process.  The requirement specified
above (no overlap) is stricter than necessary, but also easier to
implement.

While the return value of this function is safe to bind to
`*fd-bindings*', there is very little reason to do that.

The new-fd-fn argument is only intended to be used for testing."
  ;; An alternative approach would be to identify cycles in the
  ;; dependency graph implied by fd-bindings.  Then, the dependency
  ;; cycles could be broken using dup.
  (debug-log status "SIMPLIFY ~A" fd-bindings)
  (let* ((ours (fset:empty-set))
         (theirs (fset:empty-set))
         (conflict (fset:empty-set)))
    (fset:do-map (key value fd-bindings)
      (fset:adjoinf theirs key)
      (fset:adjoinf ours value))
    (setf conflict (fset:intersection ours theirs))
    (when (zerop (fset:size conflict))
      (debug-log status "SIMPLIFIED")
      (return-from simplify-fd-bindings fd-bindings))

    (let ((translation (make-hash-table)))
      (fset:do-set (conflicted-fd conflict)
        (let (new-fd)
          (loop :do (setf new-fd (funcall new-fd-fn))
             :while (fset:lookup theirs new-fd))
          (setf (gethash conflicted-fd translation) new-fd)))
      (fset:do-map (key value fd-bindings)
        (let ((new-value (gethash value translation)))
          (when new-value
            (fset:adjoinf fd-bindings key new-value))))
      (debug-log status "SIMPLIFIED ~A" fd-bindings)
      fd-bindings)))

(defclass fd-stream (fundamental-stream)
  ((fd
    :initarg :fd
    :type (or integer null)
    :initform (required))
   (symbolic
    :initarg :symbolic
    :initform nil)))

(defgeneric fd-stream-fd (stream))
(defmethod fd-stream-fd ((stream fd-stream))
  (with-slots (symbolic fd) stream
    (if symbolic
        (get-fd fd :error-on-closed-fd nil)
        fd)))

(defclass fd-input-stream (fd-stream fundamental-input-stream)
  ((buffer
    :initform (make-array 0))
   (buffer-offset
    :initform 0)
   (buffer-maximum-size
    :accessor fd-input-stream-buffer-maximum-size
    :initform 1)))

(defclass fd-output-stream (fd-stream fundamental-output-stream)
  ())

(defclass fd-character-input-stream (fd-input-stream fundamental-character-input-stream)
  ())

(defclass fd-character-output-stream (fd-output-stream fundamental-character-input-stream)
  ())

(defclass fd-character-input-output-stream (fd-character-input-stream fd-character-output-stream)
  ())

(defclass fd-binary-input-stream (fd-input-stream fundamental-binary-input-stream)
  ())

(defclass fd-binary-output-stream (fd-output-stream fundamental-binary-input-stream)
  ())

(defclass fd-binary-input-output-stream (fd-binary-input-stream fd-binary-output-stream)
  ())

(defun fd-stream-read (stream binary-p)
  (with-slots (buffer buffer-offset buffer-maximum-size) stream
    (with-accessors ((fd fd-stream-fd)) stream
      (let ((last-char (unless (zerop (length buffer))
                         (aref buffer (- (length buffer) 1)))))
        (when (>= buffer-offset (length buffer))
          (setf buffer (posix-read fd buffer-maximum-size :binary binary-p))
          (setf buffer-offset 0))

        (when (>= buffer-offset (length buffer))
          (setf buffer (string last-char))
          (setf buffer-offset 1)
          (return-from fd-stream-read :eof)))

      (let ((result (aref buffer buffer-offset)))
        (incf buffer-offset)
        result))))

(defun fd-stream-unread (stream thing)
  (with-slots (buffer buffer-offset) stream
    (when (zerop buffer-offset)
      (error "Can't unread without reading first"))
    (unless (equal (aref buffer (- buffer-offset 1)) thing)
      (error "Can't unread a different thing from what was read"))
    (decf buffer-offset)
    nil))

(defmethod stream-read-char ((stream fd-character-input-stream))
  (fd-stream-read stream nil))

(defmethod stream-unread-char ((stream fd-character-input-stream) character)
  (fd-stream-unread stream character))

(defmethod stream-read-char-no-hang ((stream fd-character-input-stream))
  ;; This is perfectly valid, but not ideal.  We just can't know in
  ;; general whether we can read without blocking.
  nil)

(defmethod stream-read-byte ((stream fd-binary-input-stream))
  (fd-stream-read stream t))

(defmethod stream-write-char ((stream fd-character-output-stream) character)
  (with-accessors ((fd fd-stream-fd)) stream
    (posix-write fd (string character))))

(defmethod stream-line-column ((stream fd-character-output-stream))
  nil)

(defmethod stream-start-line-p ((stream fd-character-output-stream))
  nil)

(defmethod stream-fresh-line ((stream fd-character-output-stream))
  nil)

(defmethod stream-write-string ((stream fd-character-output-stream) string &optional start end)
  (with-accessors ((fd fd-stream-fd)) stream
    (when (or (not (or start end))
              (and (equal 0 start)
                   (equal (length string) end)))
      (return-from stream-write-string (posix-write fd string)))

    (unless start
      (setf start 0))
    (unless end
      (setf end (length string)))

    (posix-write fd (make-array (- end start) :element-type (array-element-type string) :displaced-to string :displaced-index-offset start))))

(defmethod stream-write-byte ((stream fd-binary-output-stream) byte)
  (with-accessors ((fd fd-stream-fd)) stream
    (posix-write fd (make-array 1 :initial-element byte :element-type '(unsigned-byte 8)))))

(defmacro with-fd-streams (() &body body)
  `(let ((*standard-input* (make-instance 'fd-character-input-stream :fd 0 :symbolic t))
         (*standard-output* (make-instance 'fd-character-output-stream :fd 1 :symbolic t))
         (*error-output* (make-instance 'fd-character-output-stream :fd 2 :symbolic t)))
     ,@body))

(defun make-binary-fd-stream (fd direction symbolic)
  (macrolet
      ((make (type)
         `(make-instance ',type :symbolic symbolic :fd fd)))
    (ecase direction
      (:input
       (make fd-binary-input-stream))
      (:output
       (make fd-binary-output-stream))
      (:io
       (make fd-binary-input-output-stream)))))

(defun make-character-fd-stream (fd direction symbolic)
  (macrolet
      ((make (type)
         `(make-instance ',type :symbolic symbolic :fd fd)))
    (ecase direction
      (:input
       (make fd-character-input-stream))
      (:output
       (make fd-character-output-stream))
      (:io
       (make fd-character-input-output-stream)))))

(defun make-fd-stream (fd &key (direction :input) binary symbolic)
  (if binary
      (make-binary-fd-stream fd direction symbolic)
      (make-character-fd-stream fd direction symbolic)))
