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
   :shcl/core/expand :shcl/core/environment
   :shcl/core/posix :shcl/core/posix-types :shcl/core/exit-info :shcl/core/fd-table
   :shcl/core/working-directory :shcl/core/iterator)
  (:import-from :shcl/core/shell-form #:shell #:& #:lisp #:! #:run)
  (:import-from :shcl/core/baking #:bake-form)
  (:shadowing-import-from :alexandria #:when-let #:when-let*)
  (:shadowing-import-from :shcl/core/posix #:pipe)
  (:export #:evaluation-form #:evaluation-form-iterator #:translate))
(in-package :shcl/core/evaluate)

(optimization-settings)

(defgeneric translate (thing))

(defun evaluation-form (thing)
  (let ((bake-form (bake-form thing))
        (translation `(shell ,(translate thing))))
    (progn-concatenate bake-form translation)))

(defun evaluation-form-iterator (command-iterator)
  (map-iterator command-iterator 'evaluation-form))

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
      ((slot-boundp sy 'complete-command)
       (return-from translate (translate complete-command)))
      ((slot-boundp sy 'command-list)
       (return-from translate (translate command-list)))
      (t
       (return-from translate t)))))

(defun translate-command-list (sy)
  (with-slots (and-or separator-op command-list-tail) sy
    (let ((no-wait (typep separator-op 'par)))

      (unless command-list-tail
        (if no-wait
            (return-from translate-command-list (translate-background-job and-or))
            (return-from translate-command-list (translate and-or))))

      (return-from translate-command-list
        (progn-concatenate
         (if no-wait
             (translate-background-job and-or)
             (translate and-or))
         (translate command-list-tail))))))

(defmethod translate ((sy command-list))
  (translate-command-list sy))
(defmethod translate ((sy command-list-tail))
  (translate-command-list sy))

(defun translate-and-or (sy)
  (labels
      ((operation (and-or-tail)
         (cond
           ((slot-boundp and-or-tail 'and-if)
            'and)
           ((slot-boundp and-or-tail 'or-if)
            'or)
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
        (return-from translate `(! ,(translate pipe-sequence)))
        (return-from translate (translate pipe-sequence)))))

(defun translate-pipe-sequence (sy)
  (with-slots (pipe-sequence-tail command) sy
    (unless pipe-sequence-tail
      (return-from translate-pipe-sequence (translate command))))

  (let ((commands
         (loop :for node = sy :then (slot-value node 'pipe-sequence-tail)
            :while node
            :collect (translate (slot-value node 'command)))))
    `(shcl/core/shell-form:pipeline
      ,@commands)))

(defmethod translate ((sy pipe-sequence))
  (return-from translate (translate-pipe-sequence sy)))

(defgeneric translate-io-source-to-fd-binding (redirect))

(defmethod translate-io-source-to-fd-binding ((io-file io-file))
  (with-slots (redirect filename fd-description) io-file
    (labels
        ((filename-expansion-form ()
           (let ((expansion (gensym "EXPANSION")))
             `(let ((,expansion (expansion-for-words (fset:seq ,filename) :split-fields nil :expand-pathname t)))
                (unless (equal 1 (fset:size ,expansion))
                  (error 'not-implemented :feature "file name expanding to multiple words"))
                (fset:first ,expansion))))
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
    `(fd-bind*
         ,(translate-redirect-list-to-fd-bindings redirect-list)
       ,(translate compound-command))))

(defmethod translate ((sy subshell))
  (with-slots (compound-list) sy
    (return-from translate
      `(subshell
        ,(translate compound-list)))))

(defmethod translate ((sy compound-list))
  (with-slots (term) sy
    (return-from translate (translate term))))

(defun translate-term (sy)
  (with-slots (and-or separator term-tail) sy
    (unless (slot-boundp sy 'separator)
      (return-from translate-term (translate and-or)))

    `(shell
       ,@(loop :for node = sy :then (when (typep node '(or term term-tail))
                                      (slot-value node 'term-tail))
            :while node :collect
            (typecase node
              ((or term term-tail)
               (let ((translation (translate (slot-value node 'and-or))))
                 (if (separator-par-p (slot-value node 'separator))
                     `(& ,translation)
                     translation)))
              (t
               (translate node)))))))

(defmethod translate ((sy term))
  (return-from translate (translate-term sy)))

(defmethod translate ((sy while-clause))
  (with-slots (condition body) sy
    `(shcl/core/shell-form:while ,(translate condition)
       ,(translate body))))

(defmethod translate ((sy until-clause))
  (with-slots (condition body) sy
    `(while (! ,(translate condition))
       ,(translate body))))

(defun wordlist-words (wordlist)
  (let ((result (make-extensible-vector)))
    (labels
        ((handle (x)
           (with-slots (a-word wordlist-tail) x
             (vector-push-extend a-word result)
             (when wordlist-tail
               (handle wordlist-tail)))))
      (handle wordlist)
      result)))

(defmethod translate ((sy for-clause))
  (with-slots (name-nt wordlist body) sy
    (let ((name (simple-word-text (slot-value name-nt 'name)))
          (words
           (cond
             ((not (slot-boundp sy 'in-nt))
              `#(,(make-instance 'double-quote :parts `#(,(make-instance 'variable-expansion-word :variable "@")))))
             ((slot-boundp sy 'wordlist)
              (wordlist-words wordlist))
             (t
              #()))))
      `(shcl/core/shell-form:for (,name (expansion-for-words ,words :expand-pathname t))
         ,(translate body)))))

(defmethod translate ((sy else-part))
  (with-slots (condition body else-part) sy
    (cond
      ((slot-boundp sy 'else-part)
       `(if ,(translate condition)
            ,(translate body)
            ,(translate else-part)))
      ((slot-boundp sy 'condition)
       `(if ,(translate condition)
            ,(translate body)))
      (t
       (translate body)))))

(defmethod translate ((sy if-clause))
  (with-slots (condition body else-part) sy
    (cond
      ((slot-boundp sy 'else-part)
       `(if ,(translate condition)
            ,(translate body)
            ,(translate else-part)))
      (t
       `(if ,(translate condition)
            ,(translate body))))))

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
    (with-slots (cmd-prefix cmd-word cmd-name cmd-suffix) sy
      (when (slot-boundp sy 'cmd-prefix)
        (multiple-value-bind (prefix-assignments prefix-redirects) (cmd-prefix-parts cmd-prefix)
          (dolist (a prefix-assignments)
            (push a assignments))
          (dolist (r prefix-redirects)
            (push r redirects))))

      (when (slot-boundp sy 'cmd-name)
        (push cmd-name arguments))

      (when (slot-boundp sy 'cmd-word)
        (push cmd-word arguments))

      (when (slot-boundp sy 'cmd-suffix)
        (multiple-value-bind (suffix-arguments suffix-redirects) (cmd-suffix-parts cmd-suffix)
          (dolist (a suffix-arguments)
            (push a arguments))
          (dolist (r suffix-redirects)
            (push r redirects))))

      (values (nreverse assignments) (nreverse arguments) (nreverse redirects)))))

(defun translate-assignment (assignment)
  `(,(simple-word-text (assignment-word-name assignment))
     (expand-1 '(,(assignment-word-value-word assignment)))))

(defmethod translate ((sy simple-command))
  (with-slots (cmd-prefix cmd-word cmd-name cmd-suffix) sy
    (multiple-value-bind (raw-assignments raw-arguments raw-redirects) (simple-command-parts sy)
      (let ((redirects (mapcar 'translate-io-source-to-fd-binding raw-redirects))
            (assignments (mapcar 'translate-assignment raw-assignments))
            (arguments `(fset:convert 'list (with-fd-streams () (expansion-for-words ',raw-arguments :expand-aliases t :expand-pathname t)))))
        `(run ,arguments :environment-changes ,assignments :fd-changes ,redirects)))))
