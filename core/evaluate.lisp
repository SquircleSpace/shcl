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

(defpackage :shcl/core/evaluate
  (:use
   :common-lisp :alexandria :bordeaux-threads
   :shcl/core/utility :shcl/core/shell-grammar :shcl/core/lexer
   :shcl/core/expand :shcl/core/posix :shcl/core/posix-types
   :shcl/core/exit-info :shcl/core/fd-table
   :shcl/core/working-directory :shcl/core/iterator)
  (:import-from :shcl/core/shell-form
   #:shell-pipeline #:shell-not #:& #:shell-not #:with-subshell #:shell-if
   #:shell-while #:shell-for #:shell-run #:shell-and #:shell-or)
  (:import-from :shcl/core/sequence
   #:head #:do-while-popf #:tail #:empty-p #:walkable-to-list #:walk)
  (:shadowing-import-from :alexandria #:when-let #:when-let*)
  (:shadowing-import-from :shcl/core/posix #:pipe)
  (:export #:evaluation-form-iterator #:translate #:expansion-preparation-form))
(in-package :shcl/core/evaluate)

(optimization-settings)

(defgeneric translate (thing)
  (:documentation
   "Translate a shell syntax tree object into its corresponding lisp form."))

(defgeneric expansion-preparation-form (token)
  (:documentation
   "Returns a form should be evaluated and used instead of `token'
when expansion takes place.

The returned form will be evaluated in the same lexical scope where
the token appeared."))

(defmethod expansion-preparation-form (token)
  token)

(defmethod expansion-preparation-form ((token compound-word))
  (let* ((value (gensym "VALUE"))
         (parts (walkable-to-list (walk (compound-word-parts token))))
         (new-parts-forms (mapcar 'expansion-preparation-form parts)))
    (cond
      ((equal new-parts-forms parts)
       token)
      (t
       `(let ((,value ,token))
          (setf (compound-word-parts ,value)
                (vector ,@new-parts-forms))
          ,value)))))

(defmethod expansion-preparation-form ((token assignment-word))
  (let* ((value (gensym "VALUE"))
         (old-name (assignment-word-name token))
         (old-value (assignment-word-value-word token))
         (new-name-form (expansion-preparation-form old-name))
         (new-value-form (expansion-preparation-form old-value))
         (new-name-p (eq new-name-form old-name))
         (new-value-p (eq new-value-form old-value)))
    (cond
      ((and (not new-name-p)
            (not new-value-p))
       token)

      (t
       `(let ((,value ,token))
          ,@(unless new-name-p
              `((setf (assignment-word-name ,value) ,new-name-form)))
          ,@(unless new-value-p
              `((setf (assignment-word-value-word ,value) ,new-value-form)))
          ,value)))))

(defun evaluation-form-iterator (command-iterator)
  "Given an iterator that produces shell syntax trees, return an
iterator that produces Common Lisp forms for evaluating the shell
command.

This just maps `evaluation-form' onto every value produced by the
given iterator."
  (mapped-iterator command-iterator 'translate))

(defun expansion-form-for-tokens (tokens &key expand-aliases expand-pathname-words split-fields)
  (let ((prepared (mapcar 'expansion-preparation-form tokens)))
    `(with-fd-streams ()
       (expansion-for-words (list ,@prepared)
                            :expand-aliases ,(not (not expand-aliases))
                            :expand-pathname-words ,(not (not expand-pathname-words))
                            :split-fields ,(not (not split-fields))))))

(defparameter *umask*
  (logior s-irusr s-iwusr s-irgrp s-iroth)
  "The umask that should be used when creating new files.")

(defgeneric open-args-for-redirect (redirect)
  (:documentation
   "Returns the flags that should be passed to the posix open function
for the given redirect."))
(defmethod open-args-for-redirect ((r less))
  (declare (ignore r))
  (logior o-rdonly))
(defmethod open-args-for-redirect ((r great))
  (declare (ignore r))
  (logior o-wronly o-creat o-trunc))
(defmethod open-args-for-redirect ((r clobber))
  (declare (ignore r))
  (logior o-wronly o-creat o-trunc))
(defmethod open-args-for-redirect ((r dgreat))
  (declare (ignore r))
  (logior o-wronly o-creat o-append))
(defmethod open-args-for-redirect ((r lessgreat))
  (declare (ignore r))
  (logior o-rdwr o-creat))

(defun separator-par-p (separator)
  "Return non-nil iff the given separator non-terminal describes a
& (par) separator."
  (check-type separator separator)
  (with-slots (separator-op) separator
    (when (slot-boundp separator 'separator-op)
      (typep separator-op 'par))))

(defun translate-background-job (sy)
  `(& ,(translate sy)))

(defmethod translate ((sy complete-command))
  (with-slots (newline-list complete-command command-list) sy
    (cond
      ((and (slot-boundp sy 'complete-command)
            complete-command)
       (return-from translate (translate complete-command)))
      ((slot-boundp sy 'command-list)
       (return-from translate (translate command-list)))
      (t
       (return-from translate '(truthy-exit-info))))))

(defmethod translate ((sy command-list))
  (with-slots (and-or separator-op command-list) sy
    (let ((no-wait (typep separator-op 'par)))

      (unless command-list
        (if no-wait
            (return-from translate (translate-background-job and-or))
            (return-from translate (translate and-or))))

      (progn-concatenate
       (if no-wait
           (translate-background-job and-or)
           (translate and-or))
       (translate command-list)))))

(defun translate-and-or (sy)
  (labels
      ((operation (and-or-tail)
         (cond
           ((slot-boundp and-or-tail 'and-if)
            'shell-and)
           ((slot-boundp and-or-tail 'or-if)
            'shell-or)
           (t
            (error "Invalid and-or-tail: ~A" and-or-tail))))
       (pipeline (and-or)
         (with-slots (pipeline) and-or
           pipeline))
       (next (and-or)
         (with-slots (and-or-tail) and-or
           and-or-tail)))
    (declare (dynamic-extent #'operation #'pipeline #'next))
    (let ((form (translate (pipeline sy)))
          last-operation)

      (loop :for node = (next sy) :then (next node) :while node :do
         (let ((operation (operation node))
               (pipeline-form (translate (pipeline node))))
           (if (eq operation last-operation)
               (setf form (nconc form (list pipeline-form)))
               (setf form `(,operation ,form ,pipeline-form)))
           (setf last-operation operation)))

      form)))

(defmethod translate ((sy and-or))
  (with-slots (pipeline and-or-tail) sy
    (if and-or-tail
        (return-from translate (translate-and-or sy))
        (return-from translate (translate pipeline)))))

(defmethod translate ((sy pipeline))
  (with-slots (pipe-sequence) sy
    (if (slot-boundp sy 'bang)
        (return-from translate `(shell-not ,@(bodyify (translate pipe-sequence))))
        (return-from translate (translate pipe-sequence)))))

(defun translate-pipe-sequence (sy)
  (with-slots (pipe-sequence-tail command) sy
    (unless pipe-sequence-tail
      (return-from translate-pipe-sequence (translate command))))

  (let ((commands
         (loop :for node = sy :then (slot-value node 'pipe-sequence-tail)
            :while node
            :collect (translate (slot-value node 'command)))))
    `(shell-pipeline
      ,@commands)))

(defmethod translate ((sy pipe-sequence))
  (return-from translate (translate-pipe-sequence sy)))

(defgeneric translate-io-source-to-fd-binding (redirect))

(defmethod translate-io-source-to-fd-binding ((io-file io-file))
  (with-slots (redirect filename fd-description) io-file
    (labels
        ((filename-expansion-form ()
           (let ((expansion (gensym "EXPANSION")))
             `(let ((,expansion ,(expansion-form-for-tokens (list filename) :expand-pathname-words t :split-fields nil)))
                (unless (and (not (empty-p ,expansion)) (empty-p (tail ,expansion)))
                  (error 'not-implemented :feature "file name expanding to multiple words"))
                (aref ,expansion 0))))
         (file-source-form ()
           `(retained-fd-openat (get-fd-current-working-directory)
                                ,(filename-expansion-form)
                                ,(open-args-for-redirect redirect)
                                *umask*))
         (fd-source-form ()
           `(get-fd-binding ,(parse-integer (simple-word-text fd-description)
                                            :junk-allowed nil)
                            :if-unbound :unmanaged)))
      (etypecase redirect
        (less
         `(0 ,(file-source-form)))
        (lessand
         `(0 ,(fd-source-form)))
        (great
         `(1 ,(file-source-form)))
        (greatand
         `(1 ,(fd-source-form)))
        (dgreat
         `(1 ,(file-source-form)))
        (lessgreat
         `(0 ,(file-source-form)))
        (clobber
         `(1 ,(file-source-form)))))))

(defmethod translate-io-source-to-fd-binding ((io-here io-here))
  (error 'not-implemented :feature "Here documents"))

(defmethod translate-io-source-to-fd-binding ((io-redirect io-redirect))
  (with-slots (io-number io-source) io-redirect
    (let ((binding (translate-io-source-to-fd-binding io-source)))
      (when (slot-boundp io-redirect 'io-number)
        (setf binding (cons (io-number-fd io-number) (cdr binding))))
      binding)))

(defun translate-redirect-list-to-fd-bindings (sy)
  (loop :for redirect-list = sy
     :then (slot-value redirect-list 'redirect-list-tail)
     :while redirect-list :collect
     (translate-io-source-to-fd-binding (slot-value redirect-list 'io-redirect))))

(defmethod translate ((sy command))
  (with-slots (compound-command redirect-list) sy
    (cond
      ((and (slot-boundp sy 'redirect-list)
            redirect-list)
       `(fd-bind*
            ,(translate-redirect-list-to-fd-bindings redirect-list)
          ,@(bodyify (translate compound-command))))
      ((and (slot-boundp sy 'compound-command)
            compound-command)
       (translate compound-command))

      (t
       (error "Malformed COMMAND instance")))))

(defmethod translate ((sy subshell))
  (with-slots (term-sequence) sy
    (return-from translate
      `(with-subshell
        ,@(bodyify (apply 'progn-concatenate (mapcar 'translate (walkable-to-list term-sequence))))))))

(defmethod translate ((sy term))
  (with-slots (and-or separator) sy
    (cond
      ((or (not separator)
           (not (slot-boundp separator 'separator-op))
           (not (slot-value separator 'separator-op))
           (typep (slot-value separator 'separator-op) 'semi))
       (translate and-or))

      ((and (slot-boundp separator 'separator-op)
            (typep (slot-value separator 'separator-op) 'par))
       (translate-background-job and-or))

      (t
       (error "Unrecognized separator ~A" separator)))))

(defmethod translate ((sy compound-list))
  (with-slots (term-sequence) sy
    (return-from translate
      (apply 'progn-concatenate (mapcar 'translate (walkable-to-list term-sequence))))))

(defmethod translate ((sy while-clause))
  (with-slots (condition body) sy
    `(shell-while ,(translate condition)
       ,(translate body))))

(defmethod translate ((sy until-clause))
  (with-slots (condition body) sy
    `(shell-while (shell-not ,(translate condition))
       ,@(bodyify (translate body)))))

(defmethod translate ((sy for-clause))
  (with-slots (name-nt for-clause-range body) sy
    (let ((name (simple-word-text (slot-value name-nt 'name)))
          (words
           (cond
             ((or (not for-clause-range)
                  (not (slot-boundp for-clause-range 'in-nt)))
              `(,(make-instance 'double-quote :parts `#(,(make-instance 'variable-expansion-word :variable "@")))))

             (t
              (slot-value for-clause-range 'words)))))
      `(shell-for (,name ,(expansion-form-for-tokens (walkable-to-list words)
                                                     :expand-pathname-words t))
         ,@(bodyify (translate body))))))

(defmethod translate ((sy else-part))
  (with-slots (condition body else-part) sy
    (cond
      ((and (slot-boundp sy 'elif)
            (not (typep else-part 'fi)))
       `(shell-if ,(translate condition)
            ,(translate body)
            ,(translate else-part)))
      ((slot-boundp sy 'elif)
       `(shell-if ,(translate condition)
            ,(translate body)))
      (t
       (translate body)))))

(defmethod translate ((sy if-clause))
  (with-slots (condition body else-part) sy
    (cond
      ((typep else-part 'fi)
       `(shell-if ,(translate condition)
                  ,(translate body)))
      (t
       `(shell-if ,(translate condition)
                  ,(translate body)
                  ,(translate else-part))))))

(defmethod translate ((sy brace-group))
  (with-slots (compound-list) sy
    (translate compound-list)))

(defmethod translate ((sy do-group))
  (with-slots (compound-list) sy
    (translate compound-list)))

(defun cmd-prefix-parts (prefix)
  "Given a cmd-prefix, separate it into the 2 things it
describes (variable assignments and io redirects)."
  (with-slots (io-redirect assignment-word cmd-prefix-tail) prefix
    (multiple-value-bind (assignments redirects)
        (when cmd-prefix-tail
          (cmd-prefix-parts cmd-prefix-tail))

      (when (slot-boundp prefix 'io-redirect)
        (push io-redirect redirects))

      (when (slot-boundp prefix 'assignment-word)
        (push assignment-word assignments))

      (values assignments redirects))))

(defun cmd-suffix-parts (suffix)
  "Given a cmd-suffix, separate it into the things id
describes (command arguments and io redirects)."
  (with-slots (io-redirect a-word cmd-suffix-tail) suffix
    (multiple-value-bind (arguments redirects)
        (when cmd-suffix-tail
          (cmd-suffix-parts cmd-suffix-tail))

      (when (slot-boundp suffix 'io-redirect)
        (push io-redirect redirects))

      (when (slot-boundp suffix 'a-word)
        (push a-word arguments))

      (values arguments redirects))))

(defun simple-command-parts (sy)
  "Given a simple-command, extract the assignments, command arguments,
and io redirects."
  (let (assignments
        arguments
        redirects)
    (with-slots (cmd-prefix a-word cmd-suffix) sy
      (when (and (slot-boundp sy 'cmd-prefix)
                 cmd-prefix)
        (multiple-value-bind (prefix-assignments prefix-redirects) (cmd-prefix-parts cmd-prefix)
          (dolist (a prefix-assignments)
            (push a assignments))
          (dolist (r prefix-redirects)
            (push r redirects))))

      (when (and (slot-boundp sy 'a-word)
                 a-word)
        (push a-word arguments))

      (when (and (slot-boundp sy 'cmd-suffix)
                 cmd-suffix)
        (multiple-value-bind (suffix-arguments suffix-redirects) (cmd-suffix-parts cmd-suffix)
          (dolist (a suffix-arguments)
            (push a arguments))
          (dolist (r suffix-redirects)
            (push r redirects))))

      (values (nreverse assignments) (nreverse arguments) (nreverse redirects)))))

(defun final (walkable)
  (let (result)
    (do-while-popf (value walkable)
      (setf result value))
    result))

(defun translate-assignment (assignment)
  (let ((result-words (gensym "RESULT-WORDS"))
        (result-exit-infos (gensym "RESULT-EXIT-INFOS")))
    `(,(simple-word-text (assignment-word-name assignment))
       (multiple-value-bind (,result-words ,result-exit-infos)
           ,(expansion-form-for-tokens (list (assignment-word-value-word assignment))
                                       :expand-pathname-words t
                                       :split-fields nil)
         (values (head ,result-words)
                 (final ,result-exit-infos))))))

(defmethod translate ((sy simple-command))
  (multiple-value-bind (raw-assignments raw-arguments raw-redirects) (simple-command-parts sy)
    (let* ((redirects (mapcar 'translate-io-source-to-fd-binding (walkable-to-list raw-redirects)))
           (assignments (mapcar 'translate-assignment (walkable-to-list raw-assignments)))
           (arguments (expansion-form-for-tokens raw-arguments :expand-aliases t :expand-pathname-words t)))
      `(shell-run ,arguments :environment-changes ,assignments :fd-changes ,redirects))))
