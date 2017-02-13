(defpackage :shcl/shell/prompt-types
  (:use :common-lisp :cffi-grovel)
  (:export
   #:lineinfo #:buffer #:cursor #:lastchar
   #:histevent #:num #:str
   #:+el-prompt+ #:+el-rprompt+ #:+el-editor+ #:+el-bind+ #:+el-addfn+
   #:+el-hist+
   #:+cc-norm+ #:+cc-newline+ #:+cc-eof+ #:+cc-arghack+ #:+cc-refresh+
   #:+cc-refresh_beep+ #:+cc-cursor+ #:+cc-redisplay+ #:+cc-error+
   #:+cc-fatal+
   #:+h-setsize+ #:+h-enter+))
