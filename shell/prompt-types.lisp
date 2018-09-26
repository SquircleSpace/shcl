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

(defpackage :shcl/shell/prompt-types
  (:use :common-lisp :cffi-grovel)
  (:export
   #:wchar-t
   #:lineinfo #:buffer #:cursor #:lastchar
   #:histevent #:num #:str
   #:+el-prompt+ #:+el-rprompt+ #:+el-editor+ #:+el-bind+ #:+el-addfn+
   #:+el-hist+ #:+el-prompt-esc+
   #:+cc-norm+ #:+cc-newline+ #:+cc-eof+ #:+cc-arghack+ #:+cc-refresh+
   #:+cc-refresh-beep+ #:+cc-cursor+ #:+cc-redisplay+ #:+cc-error+
   #:+cc-fatal+
   #:+h-setsize+ #:+h-enter+))
