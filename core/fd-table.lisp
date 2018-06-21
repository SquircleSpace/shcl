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

(defpackage :shcl/core/fd-table
  (:use
   :common-lisp :alexandria :bordeaux-threads :trivial-gray-streams
   :shcl/core/utility :shcl/core/posix :shcl/core/posix-types
   :shcl/core/shell-environment)
  (:shadowing-import-from :alexandria #:when-let #:when-let*)
  (:import-from :cffi #:translate-to-foreign)
  (:import-from :trivial-garbage #:finalize #:cancel-finalization)
  (:export
   #:fd-wrapper #:fd-wrapper-close #:ref-counted-fd #:unmanaged-fd
   #:fd-wrapper-retain #:fd-wrapper-release #:fd-wrapper-value
   #:with-fd-scope #:with-private-fds #:retained-fd-dup #:retained-fd-open
   #:retained-fd-openat #:retained-fds-pipe #:with-pipe #:set-fd-binding
   #:unset-fd-binding #:get-fd-binding #:fd-bind* #:receive-ref-counted-fd
   #:linearize-fd-bindings #:with-fd-streams #:fd-stream
   #:make-fd-stream #:dup-fd-into-file-ptr #:close-file-ptr
   #:dup-fd-into-dir-ptr #:close-dir-ptr #:with-dir-ptr-for-fd))
(in-package :shcl/core/fd-table)

(optimization-settings)

(defstruct (gc-token (:constructor %make-gc-token))
  "This struct exists only to be garbage collected.")

(defun make-gc-token (&key warning cleanup-fn)
  (let ((token (%make-gc-token)))
    (finalize token (lambda ()
                      (when warning
                        (warn warning))
                      (when cleanup-fn
                        (funcall cleanup-fn))))
    token))

(defun invalidate-gc-token (gc-token)
  (cancel-finalization gc-token))

(define-condition leaked-fd (warning)
  ((value
    :initarg :value
    :reader leaked-fd-value))
  (:report (lambda (c s)
             (format s "File descriptor ~A was leaked"
                     (leaked-fd-value c)))))

(deftype fd ()
  '(integer 0))

(defgeneric fd-wrapper-value (fd-wrapper)
  (:documentation
   "Extract the actual file descriptor that the given wrapper object
holds."))

(defclass fd-wrapper ()
  ((open-p
    :initform t
    :reader fd-wrapper-open-p)
   (value
    :reader fd-wrapper-value
    :initarg :value
    :type fd
    :initform (required))
   (gc-token
    :type gc-token
    :accessor fd-wrapper-gc-token))
  (:documentation
   "A class that wraps a file descriptor and catches leaks.

If you're creating a new class to manage a file descriptor, you should
inherit from this class."))

(defmethod shared-initialize :after ((fd fd-wrapper) slot-names
                                     &rest initargs &key &allow-other-keys)
  (declare (ignore initargs slot-names))
  (unless (slot-boundp fd 'gc-token)
    (let ((value (fd-wrapper-value fd)))
      (setf (slot-value fd 'gc-token)
            (make-gc-token :warning (make-condition 'leaked-fd :value value))))))

(defmethod print-object ((fd fd-wrapper) stream)
  (print-unreadable-object (fd stream :type t)
    (format stream "~A" (fd-wrapper-value fd))))

(define-once-global %private-fds% (make-hash-table))
(define-once-global %private-fds-lock% (make-recursive-lock)
  (:documentation
   "The lock protecting `%private-fds%'."))
(defvar *private-fds-locked* nil)

(defmacro with-safe-fd-manipulation (&body body)
  (let ((fd (gensym "FD")))
    `(with-recursive-lock-held (%private-fds-lock%)
       (let ((*private-fds-locked* t))
         (labels
             ((remember-private-fd (,fd)
                (check-safe-fd-manipulation)
                (setf (gethash ,fd %private-fds%) t)
                ,fd)
              (forget-private-fd (,fd)
                (check-safe-fd-manipulation)
                (remhash ,fd %private-fds%)
                (values)))
           (declare (dynamic-extent #'remember-private-fd #'forget-private-fd)
                    (ignorable #'remember-private-fd #'forget-private-fd))
           ,@body)))))

(defun check-safe-fd-manipulation ()
  (unless *private-fds-locked*
    (error "Unsafe fd manipulation")))

(defgeneric fd-wrapper-close (fd-wrapper)
  (:documentation
   "This function is called when a file descriptor wrapper should
close the associated file descriptor.

It is an error to call this function twice on a single object.

You probably shouldn't get in the habit of calling this function
directly.  Some file descriptor wrappers (e.g. `ref-counted-fd') are
picky about when this function is called."))

(defmethod fd-wrapper-close :around ((fd-wrapper fd-wrapper))
  (with-safe-fd-manipulation
    (with-slots (open-p) fd-wrapper
      (unless open-p
        (error "Trying to close fd-wrapper twice: ~A" fd-wrapper))
      (let ((value (fd-wrapper-value fd-wrapper)))
        (call-next-method)
        (forget-private-fd value)
        (invalidate-gc-token (fd-wrapper-gc-token fd-wrapper))
        (setf open-p nil)))))

(defclass ref-counted-fd (fd-wrapper)
  ((retain-count
    :accessor ref-counted-fd-retain-count
    :initform 1
    :type (integer 0))
   (lock
    :reader ref-counted-fd-lock
    :initform (make-lock "ref-counted-fd-lock")))
  (:documentation
   "A class representing a file descriptor that is managed using
`fd-wrapper-retain' and `fd-wrapper-release'.

A ref counted fd will be closed when its retain count reaches 0."))

(defmethod print-object ((fd ref-counted-fd) stream)
  (print-unreadable-object (fd stream :type t)
    (format stream "~A, retain-count ~A"
            (fd-wrapper-value fd) (ref-counted-fd-retain-count fd))))

(defmethod fd-wrapper-close ((fd ref-counted-fd))
  (unless (zerop (ref-counted-fd-retain-count fd))
    (error "Cannot close FD ~A with nonzero retain count" fd))
  (posix-close (fd-wrapper-value fd)))

(defun make-ref-counted-fd (fd-value)
  (declare (type (integer 0) fd-value))
  (make-instance 'ref-counted-fd :value fd-value))

(defgeneric fd-wrapper-retain (fd)
  (:documentation
   "Indicate that the given wrapper object shouldn't be closed just
yet."))

(defgeneric fd-wrapper-release (fd)
  (:documentation
   "Indicate that you no longer wish to prevent the given wrapper
object from closing."))

(defmethod fd-wrapper-retain ((fd null))
  fd)

(defmethod fd-wrapper-release ((fd null))
  (values))

(defmethod fd-wrapper-release ((fd ref-counted-fd))
  (with-lock-held ((ref-counted-fd-lock fd))
    (unless (ref-counted-fd-retain-count fd)
      (error "~A is invalid" fd))
    (let ((count (decf (ref-counted-fd-retain-count fd))))
      (assert (not (minusp count)) nil "Retain count must not be negative")
      (when (zerop count)
        (fd-wrapper-close fd))
      (values))))

(defmethod fd-wrapper-retain ((fd ref-counted-fd))
  (with-lock-held ((ref-counted-fd-lock fd))
    (unless (ref-counted-fd-retain-count fd)
      (error "~A is invalid" fd))
    (let* ((count (incf (ref-counted-fd-retain-count fd))))
      (assert (plusp count) nil "Retain count must be positive")
      fd)))

(defmacro with-ref-counted-fd-retained (ref-counted-fd &body body)
  (let ((fd (gensym "FD")))
    `(let (,fd)
       (unwind-protect
            (progn
              (setf ,fd (fd-wrapper-retain ,ref-counted-fd))
              ,@body)
         (when ,fd
           (fd-wrapper-release ,fd))))))

(defmacro receive-ref-counted-fd ((variable (function-name &rest args)) &body body)
  "Safely receive a ref counted fd wrapper from a function.

`function-name' will be called with the given `args' and returned
value will be bound to `variable'.  For the dynamic extent of this
form, the ref counted fd will be retained.  As this form unwinds, the
ref counted fd will be released.

Note that the name of `function-name' is significant.  Functions that
start with `retained-fd-' should return a fd wrapper that the caller
is expected to release.  Functions that start with `retained-fds-'
should return a list of fd wrappers that the caller is expected to
release.  Functions that start with `get-fd-' should return a file
descriptor that the caller does not need to release (unless the caller
issues their own retain, of course).

This macro will respect the naming convention described above.  So,
for example, if `function-name' starts with `get-fd-', then the
returned fd wrapper will be retained before control enters the body
and it will be released as control exits the body.  If `function-name'
starts with `retained-fd-', then this macro will not issue a retain on
the returned value, but it will issue a release as control exits the
body."
  (let ((fd (gensym "FD"))
        (name (symbol-name function-name)))
    (labels
        ((string-starts-with (str prefix)
           (string-equal str prefix
                         :end1 (min (length str) (length prefix)))))
      (cond
        ((string-starts-with name "RETAINED-FD-")
         `(let (,fd)
            (unwind-protect
                 (let ((,variable (setf ,fd (,function-name ,@args))))
                   ,@body)
              (when ,fd
                (fd-wrapper-release ,fd)))))
        ((string-starts-with name "RETAINED-FDS-")
         (let ((the-fd (gensym "THE-FD")))
           `(let (,fd)
              (unwind-protect
                   (let ((,variable (setf ,fd (,function-name ,@args))))
                     ,@body)
                (dolist (,the-fd ,fd)
                  (fd-wrapper-release ,the-fd))))))
        ((string-starts-with name "GET-FD-")
         `(let ((,fd (,function-name ,@args)))
            (with-ref-counted-fd-retained ,fd
              (let ((,variable ,fd))
                ,@body))))
        (t
         (error "Not sure of the calling convention for a function named ~A" name))))))

(defclass unmanaged-fd ()
  ((value
    :reader fd-wrapper-value
    :type fd
    :initarg :value))
  (:documentation
   "A class representing a file descriptor that SHCL doesn't own.

SHCL will never attempt to close the file descriptor given to an
`unmanaged-fd'.  If your process has a file descriptor that SHCL
didn't open, you can use this class to create a fd wrapper that SHCL
understands.  This allows you to, for example, bind a virtual file
descriptor to an arbitrary physical file descriptor.

An error will be signaled if you try to initialize an instance of this
class using a file descriptor that SHCL owns.

When initializing an instance of this class, you can provide the
`:ensure-opened-p' keyword arugment.  If the value associated with
that argument is non-nil, then the given file descriptor will be examined and an error will be signaled if the file descriptor is closed."))

(defmethod shared-initialize :after ((wrapper unmanaged-fd) slot-names
                                     &rest initargs
                                     &key ensure-opened-p &allow-other-keys)
  (declare (ignore initargs slot-names))
  (let ((value (fd-wrapper-value wrapper)))
    (with-safe-fd-manipulation
      (when (gethash value %private-fds%)
        (error "Invalid fd ~A" value))
      (when ensure-opened-p
        (handler-case (fcntl value f-getfd)
          (syscall-error ()
            (error "Invalid fd ~A" value)))))))

(defmethod fd-wrapper-retain ((fd unmanaged-fd))
  fd)

(defmethod fd-wrapper-release ((fd unmanaged-fd))
  (values))

(defmethod fd-wrapper-close ((fd unmanaged-fd))
  (error "Cannot close an unmanaged fd"))

(defvar *fd-bindings* (fset:empty-map)
  "This variable contains information about how fds should be bound
when a new process is spawned.

Keys are `fd's (aka positive integers), values are fd wrapper
objects (e.g. `ref-counted-fd' or `unmanaged-fd').

This variable should not be accessed directly.  You should add
bindings with `set-fd-binding', remove bindings with
`unset-fd-binding', and query bindings with `get-fd-binding'.")

(defun unset-fd-binding (virtual-fd)
  "Removes any binding for `virtual-fd'."
  (let ((old-value (fset:lookup *fd-bindings* virtual-fd))
        (without-old-value (fset:less *fd-bindings* virtual-fd)))
    (when old-value
      (fd-wrapper-release old-value))
    (setf *fd-bindings* without-old-value)
    (values)))

(defun set-fd-binding (virtual-fd physical-fd-wrapper)
  "Bind the given virtual file descriptor to the given physical fd
wrapper.

If `physical-fd-wrapper' is nil, this simply calls
`unset-fd-binding'.

The given fd wrapper will be retained as long as this binding remains
in effect."
  (unless physical-fd-wrapper
    (return-from set-fd-binding (unset-fd-binding virtual-fd)))
  (with-ref-counted-fd-retained physical-fd-wrapper
    (unset-fd-binding virtual-fd)
    (setf (fset:lookup *fd-bindings* virtual-fd)
          (fd-wrapper-retain physical-fd-wrapper))
    (values)))

(defun get-fd-binding (virtual-fd &key (if-unbound :error))
  "Look up the fd wrapper object bound to the given virtual file
descriptor.

`virtual-fd' is a file descriptor (aka a positive integer).

`if-unbound' controls the behavior of this function is there is
nothing bound to `virtual-fd'."
  (check-type virtual-fd fd)
  (check-type if-unbound (member nil :error :unmanaged))
  (let ((result (nth-value 0 (fset:lookup *fd-bindings* virtual-fd))))
    (unless result
      (ecase if-unbound
        ((nil))
        (:error
         (error "Could not find binding for virtual fd ~A" virtual-fd))
        (:unmanaged
         (setf result (make-instance 'unmanaged-fd :value virtual-fd)))))
    result))

(defmacro fd-bind* (bindings &body body)
  "Establish file descriptor bindings for the dynamic extent of this
form.

Each binding is processed in sequence (like `let*' would).  A binding
is a list of two elements: the file descriptor to bind and the value
to bind it to.

The value form must produce a wrapper object which can be retained and
released with `fd-wrapper-retain' and `fd-wrapper-release'.  The value
form will be evaluated using `receive-ref-counted-fd', so the value
form must follow the naming conventions expected by that macro."
  (unless bindings
    (return-from fd-bind* `(progn ,@body)))
  (let ((old-fd (gensym "OLD-FD"))
        (new-fd (gensym "NEW-FD"))
        (virtual-fd (gensym "VIRTUAL-FD")))
    (destructuring-bind (virtual-fd-form physical-producer) (car bindings)
      `(let ((,virtual-fd ,virtual-fd-form))
         (receive-ref-counted-fd (,new-fd ,physical-producer)
           (receive-ref-counted-fd (,old-fd (get-fd-binding ,virtual-fd :if-unbound nil))
             (unwind-protect
                  (progn
                    (set-fd-binding ,virtual-fd ,new-fd)
                    (fd-bind* ,(cdr bindings)
                      ,@body))
               (set-fd-binding ,virtual-fd ,old-fd))))))))

(defun retain-fd-bindings (fd-bindings)
  "Retain all the file descriptors involved in the given set of
bindings."
  (fset:do-map (key-fd value-fd fd-bindings)
    (declare (ignore key-fd))
    (fd-wrapper-retain value-fd))
  fd-bindings)

(defun release-fd-bindings (fd-bindings)
  "Reclaim resources associated with the given fd bindings.

After this function returns, the given fd bindings cannot be used
again."
  (fset:do-map (key-fd value-fd fd-bindings)
    (declare (ignore key-fd))
    (fd-wrapper-release value-fd))
  (values))

(defun preserve-fd-bindings ()
  "Create a version of the current fd bindings that can be revived
later."
  (let ((bindings (or *fd-bindings* (fset:empty-map))))
    (retain-fd-bindings bindings)))

(defun destroy-fd-bindings (bindings)
  "Reclaim the resources associated with the given fd bindings.

The given fd bindings must be a return value from
`preserve-fd-bindings'.  The consequences are undefined if you attempt
to destroy the same value twice."
  (release-fd-bindings bindings)
  (values))

(defun call-with-fd-bindings (bindings continuation)
  "Restore the given fd bindings and call the given function."
  (%with-fd-scope continuation bindings))

(extend-shell-environment
 'fd-table
 'preserve-fd-bindings
 'call-with-fd-bindings
 'destroy-fd-bindings)

(defun %with-fd-scope (fn &optional bindings)
  "The function that implements `with-fd-scope'."
  (let ((*fd-bindings* (or bindings *fd-bindings* (fset:empty-map))))
    (retain-fd-bindings *fd-bindings*)
    (unwind-protect (funcall fn)
      (release-fd-bindings *fd-bindings*))))

(defmacro with-fd-scope (() &body body)
  "Introduce an fd scope."
  `(%with-fd-scope (lambda () ,@body)))

(defun compiler-owned-fds ()
  "Returns a list of file descriptors that the compiler owns."
  #+sbcl
  (list (sb-sys:fd-stream-fd sb-sys:*tty*))
  #-sbcl
  nil)

(defun track-compiler-owned-fds ()
  "Some compilers open fds for their own reasons.  Let's make sure
we're aware of them."
  (let ((fds (compiler-owned-fds)))
    (when fds
      (with-safe-fd-manipulation
        (dolist (fd fds)
          (remember-private-fd fd))))))
(on-revival track-compiler-owned-fds)

(defmacro with-private-fds ((fd-list-sym) &body body)
  "Lock the fd table and list all managed file descriptors.

Since this locks the fd table, it is very important to minimize the
amount of work done in the body of this macro.  Ideally, you would
do nothing exept spawn a new process."
  `(with-safe-fd-manipulation
     (let ((,fd-list-sym (hash-table-keys %private-fds%)))
       (declare (dynamic-extent ,fd-list-sym))
       ,@body)))

(defun retained-fd-dup (fd)
  "Dup the given file descriptor and return the new file descriptor
with a +1 retain count."
  (with-safe-fd-manipulation
    (let ((new-fd (remember-private-fd (dup fd))))
      (debug-log status "DUP ~A = ~A" new-fd fd)
      (make-ref-counted-fd new-fd))))

(defun retained-fd-open (pathname flags &optional mode)
  "This is a wrapper around the posix open function which
atomically adds the new fd to the fd table and gives it a +1 retain
count."
  (with-safe-fd-manipulation
    (let ((fd (remember-private-fd (posix-open pathname flags mode))))
      (debug-log status "OPEN ~A = ~A" fd pathname)
      (make-ref-counted-fd fd))))

(defun retained-fd-openat (dir-fd pathname flags &optional mode)
  "This is a wrapper around the posix openat function which
atomically adds the new fd to the fd table and gives it a +1 retain
count."
  (with-safe-fd-manipulation
    (let ((fd (remember-private-fd (openat (fd-wrapper-value dir-fd) pathname flags mode))))
      (debug-log status "OPENAT ~A, ~A = ~A" dir-fd fd pathname)
      (make-ref-counted-fd fd))))

(defun retained-fds-pipe ()
  "This is a wrapper around the posix pipe function which atomically
adds the new fds to the fd table and gives them +1 retain counts.

Returns a list of two values: the read-end of the pipe and the write
end of the pipe."
  (with-safe-fd-manipulation
    (multiple-value-bind (read-end write-end) (shcl/core/posix:pipe)
      (remember-private-fd read-end)
      (remember-private-fd write-end)
      (debug-log status "PIPE ~A -> ~A" write-end read-end)
      (list (make-ref-counted-fd read-end) (make-ref-counted-fd write-end)))))

(defmacro with-pipe ((read-end write-end) &body body)
  "Introduce a pipe into the retain table (as if with
`retained-fds-pipe') with a +0 retain count.

That is, you do not need to release the file descriptors produced by
this macro.  You must balance any retains you perform on the given
file descriptors."
  (let ((fd-list (gensym "FD-LIST")))
    `(receive-ref-counted-fd (,fd-list (retained-fds-pipe))
       (destructuring-bind (,read-end ,write-end) ,fd-list
         ,@body))))

(defstruct edge
  from
  to)

(defmethod fset:compare ((l edge) (r edge))
  (fset:compare-slots l r #'edge-from #'edge-to))

(defstruct vertex
  label
  ;; Internal details
  (outbound-edges (fset:empty-set))
  (inbound-edges (fset:empty-set))
  ;; For Tarjan's algorithm
  index
  lowlink
  on-stack-p)

(defmethod print-object ((vertex vertex) stream)
  (print-unreadable-object (vertex stream :type t)
    (format stream "~S" (vertex-label vertex))))

(defmethod fset:compare ((l vertex) (r vertex))
  (fset:compare-slots l r #'vertex-label #'vertex-outbound-edges
                      #'vertex-inbound-edges #'vertex-index
                      #'vertex-lowlink))

(defstruct graph
  (vertices (make-hash-table :test 'equal))
  (edges (fset:empty-set)))

(defun print-graph-as-graphviz (graph &optional (stream *standard-output*))
  (let ((current-id 0)
        (vertex-to-id (make-hash-table)))
    (labels
        ((id-for-vertex (vertex)
           (let ((id (gethash vertex vertex-to-id)))
             (unless id
               (setf id current-id)
               (incf current-id)
               (setf (gethash vertex vertex-to-id) id))
             id))
         (name-for-vertex (vertex)
           (format nil "node~A" (id-for-vertex vertex)))
         (show-vertex (label vertex)
           (let ((name (name-for-vertex vertex)))
             (format stream "~A [label=\"~A\"]~%" name label)
             (fset:do-set (edge (vertex-outbound-edges vertex))
               (format stream "~A -> ~A~%" name (name-for-vertex (edge-to edge))))
             (vertex-outbound-edges vertex))))
      (format stream "digraph G {~%")
      (maphash #'show-vertex (graph-vertices graph))
      (format stream "}~%"))))

(defun graph-vertex (graph vertex-label)
  (gethash vertex-label (graph-vertices graph)))

(defun graph-ensure-vertex (graph vertex-label)
  (multiple-value-bind (value found)
      (gethash vertex-label (graph-vertices graph))
    (if found
        value
        (setf (gethash vertex-label (graph-vertices graph))
              (make-vertex :label vertex-label)))))

(defun graph-ensure-edge (graph from-label to-label)
  (let* ((from-vertex (graph-ensure-vertex graph from-label))
         (to-vertex (graph-ensure-vertex graph to-label))
         (edge (make-edge :from from-vertex :to to-vertex)))
    (fset:adjoinf (vertex-outbound-edges from-vertex) edge)
    (fset:adjoinf (vertex-inbound-edges to-vertex) edge)
    (fset:adjoinf (graph-edges graph) edge)
    edge))

(defun fd-bindings-dependency-graph (fd-bindings)
  "Generate a graph representing the dependencies between file
descriptors.

Each vertex is labeled with an `fd' (i.e. integer).  An edge from x to
y indicates that x must be resolved (had its desired value fd dup'd
into place) prior to resolving y."
  (let ((graph (make-graph)))
    (fset:do-map (virtual-fd physical-fd fd-bindings)
      (graph-ensure-edge graph virtual-fd (fd-wrapper-value physical-fd)))
    graph))

(defun graph-tarjan-strongly-connected-components (graph)
  (let ((index 0)
        (stack (make-extensible-vector))
        (components (make-extensible-vector)))
    (labels
        ((strongly-connect (vertex)
           (setf (vertex-index vertex) index)
           (setf (vertex-lowlink vertex) index)
           (incf index)
           (vector-push-extend vertex stack)
           (setf (vertex-on-stack-p vertex) t)

           (fset:do-set (edge (vertex-outbound-edges vertex))
             (let ((neighbor (edge-to edge)))
               (cond
                 ((null (vertex-index neighbor))
                  (strongly-connect neighbor)
                  (setf (vertex-lowlink vertex)
                        (min (vertex-lowlink vertex)
                             (vertex-lowlink neighbor))))

                 ((vertex-on-stack-p neighbor)
                  (setf (vertex-lowlink vertex)
                        (min (vertex-lowlink vertex)
                             (vertex-index neighbor)))))))

           (when (equal (vertex-index vertex) (vertex-lowlink vertex))
             (let ((component (make-extensible-vector)))
               (loop
                  :for other-vertex = (vector-pop stack) :do
                  (progn
                    (setf (vertex-on-stack-p other-vertex) nil)
                    (vector-push-extend other-vertex component))
                  :while (not (equal other-vertex vertex)))
               (vector-push-extend component components)))

           (values)))

      (loop :for vertex :being :the :hash-values :of (graph-vertices graph) :do
         (unless (vertex-index vertex)
           (strongly-connect vertex)))

      components)))

#|
((1 2) (2 3) (3 1))
1 --> 2 --> 3
^-----------/

x --> y means that physical x gets bound to physical y
|#

(defun linearize-fd-bindings (&optional (fd-bindings *fd-bindings*))
  "Produce a linear sequence of bindings suitable for passing to
`shcl/core/fork-exec:run'.

SHCL makes a distinction between virtual file descriptor bindings and
physical file descriptors.  When its time to fork off a process, we
need to merge the physical file descriptors and the virtual file
descriptors.  This can be problematic.

Consider what happens if the following virtual => physical bindings
are in effect.

    virtual fd => physical fd (file that the physical fd might represent)
    1 => 2 (foo.txt)
    2 => 1 (bar.txt)
    3 => 1 (bar.txt)

So, in a subprocess, we would expect fd 1 to be bound to foo.txt and
fds 2 and 3 bound to bar.txt.  When its time to dup the physical file
descriptors into the places dictated by virtual fd bindings, you need
to be careful.  If you naievely walk through the bindings and apply
them in order you'll get the following result.

    1 => 1 (foo.txt)
    2 => 2 (foo.txt)
    3 => 3 (foo.txt)

Whoops!  We lost bar.txt.  The correct end result would actually be
the following.

    1 => 1 (foo.txt)
    2 => 2 (bar.txt)
    3 => 3 (bar.txt)

This function produces a sequence of fd dup2 assignments that will
produce the correct result.  See `shcl/core/fork-exec' for information
about the format of the list this function produces."
  (let* ((fd-graph (fd-bindings-dependency-graph fd-bindings))
         (components (nreverse (graph-tarjan-strongly-connected-components fd-graph)))
         (bindings (make-extensible-vector)))
    (loop :for component :across components :do
       (cond
         ((equal 1 (length component))
          (let* ((label (vertex-label (aref component 0)))
                 (bound-to (fset:lookup fd-bindings label)))
            (when bound-to
              (vector-push-extend (cons label (fd-wrapper-value bound-to)) bindings))))

         (t
          (assert (plusp (length component)))
          ;; Each file descriptor can only be bound to a single value.
          ;; That means that each vertex can only have one outbound
          ;; edge.  That means that there is a unique traversal for
          ;; every cycle.  In a standard graph, Tarjan's algorithm
          ;; outputs the vertices for a given component in an
          ;; arbitrary order.  In our case, once Tarjan's algorithm
          ;; visits a vertex in a cycle, it will not be able to
          ;; "escape" the cycle until it has detected and emitted the
          ;; cycle.  So, we can actually rely on the component
          ;; ordering being significant.  It is a reverse traversal of
          ;; the cycle!
          (let ((component (nreverse component)))
            (labels
                ((fd-to-dup-from-for-vertex (vertex)
                   (if (eq vertex (aref component 0))
                       :temp
                       (vertex-label vertex)))
                 (fd-to-dup-to-for-vertex (vertex)
                   (vertex-label vertex)))
              (let ((previous-vertex (aref component 0)))
                (vector-push-extend (cons :temp (vertex-label previous-vertex)) bindings)
                (loop :for index :from 1 :below (length component)
                   :for vertex = (aref component index) :do
                   (progn
                     (vector-push-extend (cons (fd-to-dup-to-for-vertex previous-vertex)
                                               (fd-to-dup-from-for-vertex vertex))
                                         bindings)
                     (setf previous-vertex vertex)))
                (vector-push-extend (cons (fd-to-dup-to-for-vertex previous-vertex)
                                          (fd-to-dup-from-for-vertex (aref component 0)))
                                    bindings)
                (vector-push-extend (cons :temp nil) bindings)))))))
    (coerce bindings 'list)))

(defclass fd-stream (fundamental-stream)
  ((fd
    :initarg :fd
    :type fd
    :initform (required)
    :documentation
    "The file descriptor this stream interacts with.")
   (symbolic
    :initarg :symbolic
    :initform nil
    :documentation
    "Non-nil if this stream should evaluate the given fd in the
context of the current fd bindings.

If this slot is non-nil, then a call to `set-fd-binding' could change
which file descriptor is actually being interacted with.  If this slot
is nil, then the stream will disregard the fd bindings and just
interact with the given fd."))
  (:documentation
   "A stream which interacts with a file descriptor."))

(defgeneric fd-stream-fd (stream)
  (:documentation
   "Return the actual file descriptor that the stream is currently
with."))
(defmethod fd-stream-fd ((stream fd-stream))
  (with-slots (symbolic fd) stream
    (if symbolic
        (fd-wrapper-value (get-fd-binding fd :if-unbound :unmanaged))
        fd)))

(defclass fd-input-stream (fd-stream fundamental-input-stream)
  ((buffer
    :initform (make-array 0)
    :documentation
    "A holding-place for content that has been read from the fd but
not yet read from the stream.")
   (buffer-offset
    :initform 0
    :documentation
    "How far into the `buffer' slot we have read.")
   (buffer-maximum-size
    :accessor fd-input-stream-buffer-maximum-size
    :initform 1
    :documentation
    "How large the buffer can become."))
  (:documentation
   "An `fd-stream' that can be used to read input."))

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
  "Read some content from the given fd-stream.

If `binary-p' is non-nil, then we will read bytes instead of
characters."
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
  "Unread one element."
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
  "Evaluate `body' in an environment where the standard streams are
bound to `fd-stream's.

`*standard-input*', `*standard-output*', and `*error-output*' are
bound to symbolic `fd-stream's for fds 0, 1, and 2 respectively."
  `(let ((*standard-input* (make-instance 'fd-character-input-stream :fd 0 :symbolic t))
         (*standard-output* (make-instance 'fd-character-output-stream :fd 1 :symbolic t))
         (*error-output* (make-instance 'fd-character-output-stream :fd 2 :symbolic t)))
     ,@body))

(defun make-binary-fd-stream (fd direction symbolic)
  "Create a binary `fd-stream'."
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
  "Make a character `fd-stream'."
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
  "Make an `fd-stream' for the given fd.

See the documentation for `fd-stream' to learn more about symbolic fd
streams."
  (if binary
      (make-binary-fd-stream fd direction symbolic)
      (make-character-fd-stream fd direction symbolic)))

(defclass file-ptr-wrapper (fd-wrapper)
    ((raw
      :initarg :raw
      :reader file-ptr-wrapper-raw
      :initform (required)))
  (:documentation
   "A wrapper around `file-ptr' which detects leaks."))

(defmethod translate-to-foreign ((value file-ptr-wrapper) (type file-ptr))
  (file-ptr-wrapper-raw value))

(defun dup-fd-into-file-ptr (fd mode)
  "Create a `file-ptr-wrapper' which interacts with a dup'd version of
the given fd.

This `file-ptr-wrapper' must be closed with `close-file-ptr'.  You
must not leak the `file-ptr-wrapper' without closing it first."
  (with-safe-fd-manipulation
    (let ((close-new-fd t)
          new-fd
          fdopen
          result)
      (unwind-protect
           (progn
             (setf new-fd (remember-private-fd (dup fd)))
             (setf fdopen (fdopen new-fd mode))
             (setf close-new-fd nil)
             (debug-log status "FDOPEN ~A = ~A" new-fd fd)
             (setf result (make-instance 'file-ptr-wrapper :raw fdopen :value new-fd))
             (setf fdopen nil)
             result)
        (cond
          (fdopen
           (assert new-fd)
           (fclose fdopen)
           (forget-private-fd new-fd))
          (close-new-fd
           (posix-close new-fd)
           (forget-private-fd new-fd)))))))

(defun close-file-ptr (file-ptr)
  "Close a `file-ptr-wrapper' pointer created with `dup-fd-into-file-ptr'."
  (fd-wrapper-close file-ptr))

(defmethod fd-wrapper-close ((file-ptr file-ptr-wrapper))
  (fclose (file-ptr-wrapper-raw file-ptr)))

(defclass dir-ptr-wrapper (fd-wrapper)
  ((raw
    :initarg :raw
    :reader dir-ptr-wrapper-raw
    :initform (required)))
  (:documentation
   "A wrapper around `dir-ptr' which detects leaks."))

(defmethod translate-to-foreign ((value dir-ptr-wrapper) (type dir-ptr))
  (dir-ptr-wrapper-raw value))

(defun dup-fd-into-dir-ptr (fd)
  "create a `dir-ptr-wrapper' which interacts with a dup'd version of
the given fd.

This `dir-ptr-wrapper' must be closed with `close-dir-ptr'.  You must
not leak the `dir-ptr-wrapper' without closing it first."
  (with-safe-fd-manipulation
    (let ((close-new-fd t)
          new-fd
          fdopen
          result)
      (unwind-protect
           (progn
             (setf new-fd (remember-private-fd (dup fd)))
             (setf fdopen (fdopendir new-fd))
             (setf close-new-fd nil)
             (debug-log status "FDOPENDIR ~A = ~A" new-fd fd)
             (setf result (make-instance 'dir-ptr-wrapper :raw fdopen :value new-fd))
             (setf fdopen nil)
             result)
        (cond
          (fdopen
           (assert new-fd)
           (fclose fdopen)
           (forget-private-fd new-fd))
          (close-new-fd
           (posix-close new-fd)
           (forget-private-fd new-fd)))))))

(defun close-dir-ptr (dir-ptr)
  "Close a `dir-ptr' pointer created with `dup-fd-into-dir-ptr'."
  (fd-wrapper-close dir-ptr))

(defmethod fd-wrapper-close ((dir-ptr dir-ptr-wrapper))
  (closedir (dir-ptr-wrapper-raw dir-ptr)))

(defmacro with-dir-ptr-for-fd ((dir-ptr-sym fd) &body body)
  "Open a `dir-ptr' with `dup-fd-into-dir-ptr', bind it to `dir-ptr-sym'
and evaluate `body'.

The `dir-ptr' will be closed when control leaves the body of this
macro."
  (let ((dir-ptr (gensym "DIR-PTR")))
    `(let (,dir-ptr)
       (unwind-protect
            (progn
              (setf ,dir-ptr (dup-fd-into-dir-ptr ,fd))
              (let ((,dir-ptr-sym ,dir-ptr))
                ,@body))
         (when ,dir-ptr
           (close-dir-ptr ,dir-ptr))))))
