(defpackage :shcl/shell/prompt
  (:use
   :common-lisp :cffi :trivial-gray-streams :shcl/core/utility
   :shcl/shell/prompt-types)
  (:import-from :shcl/core/environment #:env)
  (:import-from :shcl/core/posix #:file-ptr)
  (:import-from
   :shcl/core/fd-table
   #:make-fd-stream #:fd-stream #:get-fd #:dup-fd-into-file-ptr
   #:close-file-ptr)
  (:import-from :shcl/core/support #:string-table)
  (:import-from :fset)
  (:export #:get-line #:make-editline-stream))
(in-package :shcl/shell/prompt)

(optimization-settings)

;; Functions starting with el- take in editline-ptrs and should not be
;; exported.  Functions starting with editline- take in editline
;; instances and are eligable to be exported.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library libedit
    (t (:default "libedit")))
  (define-foreign-library libncursesw
    (t (:default "libncursesw"))))

(use-foreign-library libncursesw)
(use-foreign-library libedit)

(defctype editline-ptr :pointer
  "The Editline state")

(defctype history-ptr :pointer
  "The Editline history")

(defctype historyw-ptr :pointer
  "The Editline history (wide)")

(defctype histevent-ptr :pointer
  "History event")

(defctype histeventw-ptr :pointer
  "History event (wide)")

(defcfun (el-init "el_init" :library libedit) editline-ptr
  (prog :string)
  (fin file-ptr)
  (fout file-ptr)
  (ferr file-ptr))

(defcfun (el-end "el_end" :library libedit) :void
  (e editline-ptr))

(defcfun (el-reset "el_reset" :library libedit) :void
  (e editline-ptr))

(defcfun (%el-gets "el_gets" :library libedit) :string
  (e editline-ptr)
  (count (:pointer :int)))

(defun el-gets (e)
  "A wrapper around el_gets which returns the `count' output parameter
as a second value."
  (with-foreign-object (count :int)
    (let ((s (%el-gets e count)))
      (values s (mem-ref count :int)))))

#|
(defcfun (el-wgets "el_wgets" :library libedit) :pointer ;; to wchar
  (e editline-ptr)
  (count (:pointer :int)))
|#

(defcfun (el-getc "el_getc" :library libedit) :int
  (e editline-ptr)
  (ch (:pointer :char)))

#|
(defcfun (el-wgetc "el_wgetc" :library libedit) :int
  (e editline-ptr)
  (wc :pointer #| to wchar |#))
|#

(defcfun (el-push "el_push" :library libedit) :void
  (e editline-ptr)
  (mbs :string))

#|
(defcfun (el-wpush "el_wpush" :library libedit) :void
  (e editline-ptr)
  (wcs :pointer #| to wchar |#))
|#

(defcfun (el-parse "el_parse" :library libedit) :int
  (e editline-ptr)
  (argc :int)
  (argv string-table))

#|
(defcfun (el-wparse "el_wparse" :library libedit) :int
  (e editline-ptr)
  (argc :int)
  (argv :pointer #| to wchar strings |#))
|#

(defcfun (el-set "el_set" :library libedit) :int
  (e editline-ptr)
  (op :int)
  &rest)

(defun el-set-prompt (e prompt-callback)
  "A wrapper around `el-set' which provides a convenient way to use
the EL_PROMPT sub-routine of `el-set'."
  (el-set e +el-prompt+ :pointer prompt-callback))

(defun el-set-rprompt (e prompt-callback)
  "A wrapper around `el-set' which provides a convenient way to use
the EL_RPROMPT sub-routine of `el-set'."
  (el-set e +el-rprompt+ :pointer prompt-callback))

(defun el-set-editor (e mode)
  "A wrapper around `el-set' which provides a convenient way to use
the EL_EDITOR sub-routine of `el-set'."
  (el-set e +el-editor+ :string mode))

(defun el-set-addfn (e name-ptr help-ptr fn)
  "A wrapper around `el-set' which provides a convenient way to use
the EL_ADDFN sub-routine of `el-set'."
  (el-set e +el-addfn+ :pointer name-ptr :pointer help-ptr :pointer fn))

(defun el-set-bind (e key command)
  "A wrapper around `el-set' which provides a convenient way to use
the EL_BIND sub-routine of `el-set'."
  (el-set e +el-bind+ :string key :string command :pointer (null-pointer)))

(defcfun (el-wset "el_wset" :library libedit) :int
  (e editline-ptr)
  (op :int)
  &rest)

(defcfun (el-get "el_get" :library libedit) :int
  (e editline-ptr)
  (op :int)
  &rest)

(defcfun (el-wget "el_wget" :library libedit) :int
  (e editline-ptr)
  (op :int)
  &rest)

(defcfun (el-source "el_source" :library libedit) :int
  (e editline-ptr)
  (file :string))

(defcfun (el-resize "el_resize" :library libedit) :void
  (e editline-ptr))

(defcfun (el-cursor "el_cursor" :library libedit) :int
  (e editline-ptr)
  (count :int))

(defcfun (el-line "el_line" :library libedit) (:pointer (:struct lineinfo))
  (e editline-ptr))

(defcfun (el-wline "el_wline" :library libedit) :pointer #| to LineInfoW struct |#
  (e editline-ptr))

(defcfun (el-insertstr "el_insertstr" :library libedit) :int
  (e editline-ptr)
  (str :string))

(defcfun (el-winsertstr "el_winsertstr" :library libedit) :int
  (e editline-ptr)
  (str :string))

(defcfun (el-deletestr "el_deletestr" :library libedit) :void
  (e editline-ptr)
  (count :int))

(defcfun (el-wdeletestr "el_wdeletestr" :library libedit) :void
  (e editline-ptr)
  (count :int))

(defcfun (history-init "history_init" :library libedit) history-ptr)

(defcfun (history-winit "history_winit" :library libedit) historyw-ptr)

(defcfun (history-end "history_end" :library libedit) :void
  (h history-ptr))

(defcfun (history-wend "history_wend" :library libedit) :void
  (h historyw-ptr))

(defcfun (history "history" :library libedit) :int
  (h history-ptr)
  (ev histevent-ptr)
  (op :int))

(defcfun (history-w "history_w" :library libedit) :int
  (h historyw-ptr)
  (ev histeventw-ptr)
  (op :int))

(defvar *editline-sidetable* (make-hash-table)
  "This table provides a way to find the instance of the `editline'
class corresponding to a given `editline-ptr'.")

(defclass editline ()
  ((stdin
    :reader editline-stdin
    :documentation
    "A lisp stream which communicates with the same endpoint as the
stdin editline is using.")
   (stdout
    :reader editline-stdout
    :documentation
    "A lisp stream which communicates with the same endpoint as the
stdout editline is using.")
   (stderr
    :reader editline-stderr
    :documentation
    "A lisp stream which communicates with the same endpoint as the
stderr editline is using.")
   (ptr
    :documentation
    "The editline-ptr associated with this instance.")

   ;; Resources that need to be freed
   (prompt
    :documentation
    "The C-string representing the prompt that editline should use.")
   (rprompt
    :documentation
    "The C-string representing the rprompt that editline should use.")
   (fin
    :documentation
    "The file-ptr that was provided to editline for stdin.")
   (fout
    :documentation
    "The file-ptr that was provided to editline for stdout.")
   (ferr
    :documentation
    "The file-ptr that was provided to editline for stderr.")
   (command-data
    :initform (make-hash-table)
    :documentation
    "Contains `command-data' that was provided to editline.")))

(defstruct command-data
  "A struct that contains pointers which need to be freed."
  (name-ptr (null-pointer))
  (help-ptr (null-pointer)))

(defun extra (e)
  "Look up the `editline' instance associated with the given
`editline-ptr'."
  (gethash (pointer-address e) *editline-sidetable*))

(defun forget-extra (e)
  "Remove the `editline' instance associated with the given
`editline-ptr' from `*editline-sidetable*'."
  (remhash (pointer-address e) *editline-sidetable*)
  (values))

(defcallback get-prompt (:pointer :char)
    ((e editline-ptr))
  (slot-value (extra e) 'prompt))

(defcallback get-rprompt (:pointer :char)
    ((e editline-ptr))
  (slot-value (extra e) 'rprompt))

(defun make-editline (program stdin-fd stdout-fd stderr-fd &key (prompt "% ") (rprompt "") (editor "emacs"))
  "Create an `editline' instance.

This instance must be destroyed with `destroy-editline'.  You are
encouraged to use `with-editline' to ensure the object is destroyed."
  (let ((extra (make-instance 'editline)))
    (unwind-protect
         (with-slots
               (stdin stdout stderr
                      fin fout ferr
                      ptr (extra-prompt prompt) (extra-rprompt rprompt))
             extra
           (setf fin (dup-fd-into-file-ptr stdin-fd "r"))
           (setf fout (dup-fd-into-file-ptr stdout-fd "w"))
           (setf ferr (dup-fd-into-file-ptr stderr-fd "w"))
           (setf ptr (el-init program fin fout ferr))

           (setf extra-prompt (foreign-string-alloc prompt))
           (setf extra-rprompt (foreign-string-alloc rprompt))
           (el-set-prompt ptr (callback get-prompt))
           (el-set-rprompt ptr (callback get-rprompt))

           (el-set-editor ptr editor)

           (setf stdin (make-fd-stream stdin-fd :direction :input))
           (setf stdout (make-fd-stream stdout-fd :direction :output))
           (setf stderr (make-fd-stream stderr-fd :direction :output))

           (setf (gethash (pointer-address ptr) *editline-sidetable*) extra)
           (let ((result extra))
             (setf extra nil)
             result))
      (when extra
        (destroy-editline extra)))))

(defun destroy-editline (e)
  "Destroy and invalidate the given `editline' instance.

Resources associated with the `editline' instance will be reclaimed.
After destruction, the instance may no longer be passed to any
function in this package."
  (macrolet
      ((clear (place &body body)
         `(when ,place
            ,@body
            (setf ,place nil))))
    (with-slots (ptr fin fout ferr prompt rprompt command-data) e
      (clear ptr
        (forget-extra ptr)
        (el-end ptr))
      (clear fin
        (close-file-ptr fin))
      (clear fout
        (close-file-ptr fout))
      (clear ferr
        (close-file-ptr ferr))
      (clear prompt
        (foreign-free prompt))
      (clear rprompt
        (foreign-free rprompt))
      (clear command-data
        (loop :for data :being :the :hash-values :of command-data :do
           (progn
             (clear (command-data-name-ptr data)
               (foreign-free (command-data-name-ptr data)))
             (clear (command-data-help-ptr data)
               (foreign-free (command-data-help-ptr data))))))))
  (values))

(defmacro with-editline ((sym program-name stdin-fd stdout-fd stderr-fd &rest args) &body body)
  "Create an `editline' instance and bind it to `sym'.

The editline instance has dynamic extent."
  (let ((editline (gensym "EDITLINE")))
    `(let ((,editline (make-editline ,program-name ,stdin-fd ,stdout-fd ,stderr-fd ,@args)))
       (declare (dynamic-extent ,editline))
       (unwind-protect
            (let ((,sym ,editline))
              (declare (dynamic-extent ,sym))
              ,@body)
         (destroy-editline ,editline)))))

(defun editline-prompt (e)
  "Retrieve the prompt string associated with the given `editline'.

This place is `setf'-able.  Changing the prompt will not take effect
until the next time the prompt is drawn."
  (foreign-string-to-lisp (slot-value e 'prompt)))

(defun editline-rprompt (e)
  "Retrieve the rprompt string associated with the given `editline'.

This place is `setf'-able.  Changing the rprompt will not take effect
until the next time the prompt is drawn."
  (foreign-string-to-lisp (slot-value e 'rprompt)))

(defun (setf editline-prompt) (value e)
  "Change the prompt string for the given `editline'.

See `editline-prompt'."
  (with-slots (prompt) e
    (when prompt
      (foreign-free prompt))
    (setf prompt (foreign-string-alloc value))
    value))

(defun (setf editline-rprompt) (value e)
  "Change the rprompt string for the given `editline'.

See `editline-rprompt'."
  (with-slots (rprompt) e
    (when rprompt
      (foreign-free rprompt))
    (setf rprompt (foreign-string-alloc value))
    value))

(defun editline-gets (e)
  "Retrieve a line of input from the user."
  (nth-value 0 (el-gets (slot-value e 'ptr))))

(defmacro define-editline-trampoline (trampoline-name &optional (fn-name trampoline-name))
  "Create a cffi callback which calls the given lisp function.

`editline-set-addfn' takes a symbol naming a \"trampoline\".  This
macro creates such a trampoline.

CFFI callbacks can be a bit annoying to work with directly for two
reasons.

1. Redefining the callback doesn't replace the existing callback.  If
you've passed the callback to editline, then it won't call the new
definition of the callback.  Using a trampoline means that you can
redefine the lisp function that the callback calls.

2. We need to store extra information outside of the `editline-ptr'
that editline passes to the callback.  It can be anoying to retrieve
that info in every callback.  This trampoline takes care of that for
you."
  (check-type trampoline-name symbol)
  (check-type fn-name symbol)
  `(defcallback ,trampoline-name :unsigned-char
       ((e editline-ptr)
        (ch :int))
     (,fn-name (extra e) ch)))

(defun editline-set-addfn (e name help trampoline-sym)
  "Add a new function to the `editline' instance.

This function can be bound to a key with `editline-set-bind'.  It is
an error to bind to the same name twice."
  (check-type name string)
  (check-type help string)
  (with-slots (command-data ptr) e
    (when (gethash name command-data)
      (error "Function already set for ~A" name))
    (let ((new (make-command-data))
          set)
      (unwind-protect
           (progn
             (setf (command-data-name-ptr new) (foreign-string-alloc name))
             (setf (command-data-help-ptr new) (foreign-string-alloc help))
             (el-set-addfn ptr (command-data-name-ptr new) (command-data-help-ptr new) (get-callback trampoline-sym))
             (setf (gethash name command-data) new)
             (setf set t))
        (unless set
          (foreign-free (command-data-name-ptr new))
          (setf (command-data-name-ptr new) (null-pointer))
          (foreign-free (command-data-help-ptr new))
          (setf (command-data-help-ptr new) (null-pointer)))))))

(defun editline-set-bind (e key command)
  "Bind a key to the named command."
  (check-type command string)
  (with-slots (ptr) e
    (el-set-bind ptr key command)))

(defstruct lineinfo
  "Information about the state of the editline input.

`lineinfo-text' contains the text inputted by the user.

`lineinfo-cursor-index' indicates the index where the cursor is.  That
is, the next character to be inserted will appear at the index
returned by this function (assuming the user doesn't move the
cursor).  This value is a non-negative number less than or equal to
(length (lineinfo-text))"
  text
  cursor-index)

(defun convert-lineinfo (lineinfo-ptr)
  "Produce a `lineinfo' from a `lineinfo-ptr'."
  (let* ((buffer (foreign-slot-value lineinfo-ptr '(:struct lineinfo) 'buffer))
         (cursor (foreign-slot-value lineinfo-ptr '(:struct lineinfo) 'cursor))
         (lastchar (foreign-slot-value lineinfo-ptr '(:struct lineinfo) 'lastchar))
         (buffer-text (foreign-string-to-lisp buffer :count (- (pointer-address lastchar) (pointer-address buffer))))
         (cursor-position (- (pointer-address cursor) (pointer-address buffer))))
    (make-lineinfo :text buffer-text :cursor-index cursor-position)))

(defun editline-line (e)
  "Retrieve information about the line the user is currently authoring.

Returns a `lineinfo' instance."
  (with-slots (ptr) e
    (convert-lineinfo (el-line ptr))))

(defun get-line (prompt)
  "This is intended to be the super-high-level auto-magic way to get
input from the user.

This interacts with the user on symbolic fds 0, 1, and 2."
  (let ((stdin-fd (get-fd 0))
        (stdout-fd (get-fd 1))
        (stderr-fd (get-fd 2)))
    (with-editline (e "shcl" stdin-fd stdout-fd stderr-fd)
      (setf (editline-prompt e) prompt)
      (editline-gets e))))

(defclass editline-stream (fundamental-character-input-stream)
  ((text
    :initform (fset:empty-seq)
    :documentation
    "This slot contains characters that haven't been read yet.")
   (prompt-fn
    :initform (constantly "% ")
    :initarg :prompt-fn
    :documentation
    "This slot contains a function which produces the prompt string
which the user sees when input is needed."))
  (:documentation
   "An `editline-stream' is an input stream which retrieves its
contents from the user (using `get-line').

You can just read from this stream as though it was a normal input
stream.  The user will see a prompt whenever additional content is
required.

The prompt the user sees is decided by the `prompt-fn'."))

(defun extend-editline-stream (stream)
  "Add another line of content to an `editline-stream'."
  (with-slots (text prompt-fn) stream
    (unless (open-stream-p stream)
      (error "Stream is closed"))
    (unless (zerop (fset:size text))
      (error "Stream isn't empty yet"))
    (let ((next-line (get-line (funcall prompt-fn))))
      (cond
        (next-line
         (assert (plusp (length next-line)))
         (fset:appendf text next-line))

        (t
         (close stream)))
      (values))))

(defun buffer-read-char (s hang-p)
  "Read a single character from an `editline-stream'."
  (with-slots (text) s
    (tagbody
     again
       (return-from buffer-read-char
         (cond
           ((plusp (fset:size text))
            (let ((result (fset:pop-first text)))
              result))

           ((not (open-stream-p s))
            :eof)

           (hang-p
            (extend-editline-stream s)
            (go again))

           (t
            nil))))))

(defmethod stream-read-char ((s editline-stream))
  (let ((value (buffer-read-char s t)))
    (assert value)
    value))

(defmethod stream-unread-char ((s editline-stream) char)
  (with-slots (text) s
    (fset:push-first text char)
    nil))

(defmethod stream-read-char-no-hang ((s editline-stream))
  (buffer-read-char s nil))

(defmethod stream-clear-input ((s editline-stream))
  (with-slots (text) s
    (setf text (fset:empty-seq))))

(defun make-editline-stream (prompt-fn)
  "Create a stream whose contents are retrieved from the user using
the editline library."
  (make-instance 'editline-stream :prompt-fn prompt-fn))
