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

(defpackage :shcl/shell/builtins
  (:use :common-lisp :shcl/core/utility :shcl/core/command)
  (:import-from :shcl/shell/complete #:completion-suggestions-for-input)
  (:import-from :shcl/core/iterator #:do-iterator)
  (:import-from :shcl/core/lexer)
  (:import-from :shcl/shell/prompt
   #:completion-suggestion-display-text #:completion-suggestion-replacement-text
   #:completion-suggestion-replacement-range)
  (:import-from :fset))
(in-package :shcl/shell/builtins)

(define-builtin -shcl-dump-logs ()
  "Dump available logs to stdout."
  (dump-logs)
  0)

(define-builtin -shcl-list-commands ()
  "List all the commands known in the current command namespace."
  (fset:do-map (name map (command-namespace-table *command-namespace*))
    (when (and (not (fset:empty? map))
               (stringp name))
      (format t "~A~%" name)))
  0)

(define-builtin -shcl-man (&required command)
  "Print the documentation for the given command."
  (let ((doc (documentation command 'command)))
    (unless doc
      (error 'command-error :message (format nil "No documentation found for ~A~%" command)))
    (write-string doc)
    (unless (equal #\newline (aref doc (1- (length doc))))
      (terpri)))
  0)

(define-builtin -shcl-complete (&flag show-result &option point &required input-string)
  (cond
    ((< 1 (length point))
     (error 'command-error :message "point option must not be specified more than once."))
    ((equal 1 (length point))
     (setf point (wrap-errors
                   (parse-integer (aref point 0) :junk-allowed nil))))
    ((equal 0 (length point))
     (setf point (length input-string))))

  (setf show-result (not (zerop (length show-result))))

  (let ((readtable (shcl/core/lexer:standard-shell-readtable)))
    (do-iterator (value (completion-suggestions-for-input input-string point readtable))
      (format t "~A~%" (completion-suggestion-display-text value))
      (when show-result
        (write-string "    => ")
        (write-string input-string *standard-output* :end (car (completion-suggestion-replacement-range value)))
        (write-string (completion-suggestion-replacement-text value))
        (write-string input-string *standard-output* :start (cdr (completion-suggestion-replacement-range value)))
        (format t "~%"))))
  0)
