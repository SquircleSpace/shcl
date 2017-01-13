(defpackage :shcl/shell/prompt
  (:use :common-lisp :cffi :shcl/core/utility :shcl/shell/prompt-types)
  (:import-from :shcl/core/environment #:env)
  (:import-from :shcl/core/posix #:posix-write #:dup)
  (:import-from :shcl/core/fd-table
                #:make-fd-stream #:fd-stream #:with-safe-fd-manipulation #:track
                #:forget #:get-fd)
  (:import-from :shcl/core/support #:string-table)
  (:import-from :fset)
  (:export #:get-line))
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

(defctype file-ptr :pointer
  "The C standard library FILE * type.")

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
  (el-set e +el-prompt+ :pointer prompt-callback))

(defun el-set-rprompt (e prompt-callback)
  (el-set e +el-rprompt+ :pointer prompt-callback))

(defun el-set-editor (e mode)
  (el-set e +el-editor+ :string mode))

(defun el-set-add-fn (e name-ptr help-ptr fn)
  (el-set e +el-addfn+ :pointer name-ptr :pointer help-ptr :pointer fn))

(defun el-set-bind (e key command)
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

(defcfun (fdopen "fdopen") file-ptr
  (fd :int)
  (mode :string))

(defcfun (fclose "fclose") :int
  (stream file-ptr))

(defcfun (fileno "fileno") :int
  (stream file-ptr))

(defun close-file-ptr (file-ptr)
  (with-safe-fd-manipulation
    (debug-log status "FCLOSE ~A" file-ptr)
    (forget (fileno file-ptr))
    (fclose file-ptr)))

(defun file-ptr-wrapper-for-fd (fd mode)
  (with-safe-fd-manipulation
    (let* ((new-fd (track (dup fd)))
           (result (fdopen new-fd mode)))
      (debug-log status "FDOPEN ~A ~A = ~A" result new-fd fd)
      result)))

(defvar *editline-sidetable* (make-hash-table))

(defclass editline ()
  ((stdin
    :reader editline-stdin)
   (stdout
    :reader editline-stdout)
   (stderr
    :reader editline-stderr)

   ;; Low-level stuff
   prompt
   rprompt
   fin
   fout
   ferr
   ptr
   (command-data
    :initform (make-hash-table))))

(defstruct command-data
  (name-ptr (null-pointer))
  (help-ptr (null-pointer)))

(defun extra (e)
  (gethash (pointer-address e) *editline-sidetable*))

(defun forget-extra (e)
  (remhash (pointer-address e) *editline-sidetable*)
  (values))

(defcallback get-prompt (:pointer :char)
    ((e editline-ptr))
  (slot-value (extra e) 'prompt))

(defcallback get-rprompt (:pointer :char)
    ((e editline-ptr))
  (slot-value (extra e) 'rprompt))

(defun make-editline (program stdin-fd stdout-fd stderr-fd &key (prompt "% ") (rprompt "") (editor "emacs"))
  (let ((extra (make-instance 'editline)))
    (unwind-protect
         (with-slots
               (stdin stdout stderr
                      fin fout ferr
                      ptr (extra-prompt prompt) (extra-rprompt rprompt))
             extra
           (setf fin (file-ptr-wrapper-for-fd stdin-fd "r"))
           (setf fout (file-ptr-wrapper-for-fd stdout-fd "w"))
           (setf ferr (file-ptr-wrapper-for-fd stderr-fd "w"))
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
  (let ((editline (gensym "EDITLINE")))
    `(let ((,editline (make-editline ,program-name ,stdin-fd ,stdout-fd ,stderr-fd ,@args)))
       (unwind-protect
            (let ((,sym ,editline))
              ,@body)
         (destroy-editline ,editline)))))

(defun editline-prompt (e)
  (foreign-string-to-lisp (slot-value e 'prompt)))

(defun editline-rprompt (e)
  (foreign-string-to-lisp (slot-value e 'rprompt)))

(defun (setf editline-prompt) (value e)
  (with-slots (prompt) e
    (when prompt
      (foreign-free prompt))
    (setf prompt (foreign-string-alloc value))
    value))

(defun (setf editline-rprompt) (value e)
  (with-slots (rprompt) e
    (when rprompt
      (foreign-free rprompt))
    (setf rprompt (foreign-string-alloc value))
    value))

(defun editline-gets (e)
  (nth-value 0 (el-gets (slot-value e 'ptr))))

(defmacro define-editline-trampoline (trampoline-name &optional (fn-name trampoline-name))
  (check-type trampoline-name symbol)
  (check-type fn-name symbol)
  `(defcallback ,trampoline-name :unsigned-char
       ((e editline-ptr)
        (ch :int))
     (,fn-name (extra e) ch)))

(defun editline-set-add-fn (e name help trampoline-fn)
  (with-slots (command-data ptr) e
    (when (gethash name command-data)
      (error "Function already set for ~A" name))
    (let ((new (make-command-data))
          set)
      (unwind-protect
           (progn
             (setf (command-data-name-ptr new) (foreign-string-alloc name))
             (setf (command-data-help-ptr new) (foreign-string-alloc help))
             (el-set-add-fn ptr (command-data-name-ptr new) (command-data-help-ptr new) trampoline-fn)
             (setf (gethash name command-data) new)
             (setf set t))
        (unless set
          (foreign-free (command-data-name-ptr new))
          (setf (command-data-name-ptr new) (null-pointer))
          (foreign-free (command-data-help-ptr new))
          (setf (command-data-help-ptr new) (null-pointer)))))))

(defun editline-set-bind (e key command)
  (with-slots (ptr) e
    (el-set-bind ptr key command)))

(defstruct lineinfo
  text
  cursor-index)

(defun convert-lineinfo (lineinfo-ptr)
  (let* ((buffer (foreign-slot-value lineinfo-ptr '(:struct lineinfo) 'buffer))
         (cursor (foreign-slot-value lineinfo-ptr '(:struct lineinfo) 'cursor))
         (lastchar (foreign-slot-value lineinfo-ptr '(:struct lineinfo) 'lastchar))
         (buffer-text (foreign-string-to-lisp buffer :count (- (pointer-address lastchar) (pointer-address buffer))))
         (cursor-position (- (pointer-address cursor) (pointer-address buffer))))
    (make-lineinfo :text buffer-text :cursor-index cursor-position)))

(defun editline-line (e)
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
