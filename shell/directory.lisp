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

(defpackage :shcl/shell/directory
  (:use
   :common-lisp :cffi :shcl/core/utility :shcl/core/iterator :shcl/core/command
   :shcl/core/environment :shcl/core/working-directory :shcl/core/fd-table
   :shcl/core/lisp-interpolation :shcl/core/posix :shcl/core/posix-types))
(in-package :shcl/shell/directory)

(optimization-settings)

(defun physical-pwd ()
  (let ((raw-path (capture (:streams '(:stdout)) (evaluate-constant-shell-string "pwd -P"))))
    (string-right-trim #(#\newline) raw-path)))

(defun path-iterator (path)
  (let ((current-part (make-string-output-stream))
        (string-iterator (vector-iterator path))
        (first-p t)
        (slash-count 0))
    (make-iterator ()
      (labels
          ((emit-slash ()
             (cond
               ((equal slash-count 2)
                (setf slash-count nil)
                (emit "//"))
               ((not (zerop slash-count))
                (setf slash-count nil)
                (emit "/")))))
        (do-iterator (c string-iterator)
          (cond
            (first-p
             (case c
               (#\/
                (incf slash-count))
               (otherwise
                (setf first-p nil)
                (write-char c current-part)
                (emit-slash))))

            (t
             (case c
               (#\/
                (let ((part (get-output-stream-string current-part)))
                  (unless (zerop (length part))
                    (emit part))))

               (otherwise
                (write-char c current-part))))))
        (when slash-count
          (emit-slash))
        (let ((part (get-output-stream-string current-part)))
          (if (zerop (length part))
              (stop)
              (emit part)))))))

(defun path-parts (path)
  (iterator-values (path-iterator path)))

(defun combine-path-parts (parts)
  (let ((result (make-string-output-stream)))
    (when (equal #\/ (aref (aref parts 0) 0))
      (write-string (aref parts 0) result))
    (loop :for index :from 1 :below (length parts) :do
       (progn
         (write-string (aref parts index) result)
         (unless (equal index (- (length parts) 1))
           (write-char #\/ result))))
    (get-output-stream-string result)))

(defun path-byte-length (path)
  (1+ (strlen path)))

(defun interpret-path (path physical-p)
  ;; Step 1 and 2
  (unless path
    (let ((home $home))
      (when (zerop (length home))
        (return-from interpret-path))

      (setf path $home)))

  (let (curpath
        pwd-curpath)
    (tagbody
       ;; Step 3
       (when (equal #\/ (aref path 0))
         (setf curpath path)
         (go step-7))

       ;; Step 4
       (let ((first-part (next (path-iterator path))))
         (when (or (equal "." first-part)
                   (equal ".." first-part))
           (go step-6)))

       ;; Step 5
       (do-iterator (cdpath (colon-list-iterator $cdpath))
         (let* ((cdpath (if (equal "" cdpath) "./" cdpath))
                (slash-terminated (equal #\/ (aref cdpath (- (length cdpath) 1))))
                (query-path (concatenate 'string cdpath (if slash-terminated "" "/") path)))
           (when (directory-p query-path)
             (setf curpath query-path)
             (go step-7))))

     step-6
       (setf curpath path)

     step-7
       (assert curpath)
       (when physical-p
         (go step-10))
       (unless (equal #\/ (aref curpath 0))
         (let* ((pwd $pwd)
                (slash (if (equal #\/ (aref pwd (- (length pwd) 1)))
                           ""
                           "/")))
           (setf curpath (concatenate 'string pwd slash curpath))))

       ;; Step 8
       (let ((parts (path-parts curpath))
             (clean-parts (make-extensible-vector)))
         (loop :for part :across parts :do
            (cond
              ((equal part ".")) ;; Do nothing

              ((equal part "..")
               (let ((previous-part (aref clean-parts (- (length clean-parts) 1))))
                 (cond
                   ((or (zerop (length clean-parts))
                        (equal ".." previous-part))
                    (vector-push-extend part clean-parts))

                   ;; The standard seems to say we should add the
                   ;; .. in this case, but, that's pretty redundant.
                   ((or (equal "/" previous-part)
                        (equal "//" previous-part))) ;; Do nothing

                   ((directory-p (combine-path-parts clean-parts))
                    (vector-pop clean-parts))

                   (t
                    (let ((message
                           (format nil
                                   "The path ~A does not refer to a directory"
                                   (combine-path-parts clean-parts))))
                      (error 'path-invalid :message message))))))

              (t
               (vector-push-extend part clean-parts))))
         (setf curpath (combine-path-parts clean-parts)))

       ;; Step 9
       (setf pwd-curpath curpath)
       (when (>= (path-byte-length curpath) path-max)
         (error 'not-implemented :feature "Long paths"))

     step-10
       (let (pwd-string
             cd-string)
         (if physical-p
             (setf cd-string curpath)
             (progn
               (assert pwd-curpath)
               (setf pwd-string pwd-curpath)
               (setf cd-string curpath)))
         ;; Tagbody returns nil, so we need to forcefully return our
         ;; result
         (return-from interpret-path (values pwd-string cd-string))))))

(defun switch-directory (command-name path physical-p switcher-fn)
  (handler-bind
      ((path-invalid
        (lambda (e)
          (format *error-output* "~A: ~A~%" command-name (path-invalid-message e))
          (return-from switch-directory 1))))

    (multiple-value-bind (pwd-string cd-string) (interpret-path path physical-p)
      (debug-log status "CD ~A [~A => ~A] PWD=~A"
                 (if physical-p "physical" "logical")
                 path cd-string pwd-string)
      (funcall switcher-fn cd-string)
      (unless pwd-string
        (setf pwd-string (physical-pwd)))
      (setf $oldpwd $pwd)
      (setf $pwd pwd-string)
      0)))

(defun parse-cd-args (args)
  (let ((command-name (pop args))
        physical-p
        directory)

    (cond
      ((equal "-P" (first args))
       (pop args)
       (setf physical-p t))
      ((equal "-L" (first args))
       (pop args)
       (setf physical-p nil)))

    (when (cdr args)
      (error 'command-error :message "Too many arguments"))
    (setf directory (pop args))

    (values command-name physical-p directory)))

(define-builtin (builtin-cd "cd") (&argv0 argv0 &rest args)
  (let (print-pwd)
    (when (and (not (cdr args))
               (equal "-" (car args)))
      (setf args (list $oldpwd))
      (setf print-pwd t))

    (multiple-value-bind (command-name physical-p directory) (parse-cd-args (cons argv0 args))
      (unless directory
        (let ((home (env "HOME")))
          (when (zerop (length home))
            (format *error-output* "cd: Could not locate home~%")
            (return-from builtin-cd 1))
          (setf directory home)))

      (let ((result (switch-directory command-name directory physical-p 'cd)))
        (when print-pwd
          (evaluate-constant-shell-string "pwd"))
        result))))

(define-builtin pushd (&whole whole &rest rest)
  (declare (ignore rest))
  (multiple-value-bind (command-name physical-p directory) (parse-cd-args whole)
    (unless directory
      (setf directory "."))

    (switch-directory command-name directory physical-p 'push-working-directory)))

(define-builtin popd ()
  (pop-working-directory)
  0)
