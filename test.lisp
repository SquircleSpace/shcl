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

(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3) (compilation-speed 0)))

(let ((here (truename ".")))
  (push here asdf:*central-registry*)
  (push (merge-pathnames #P"test/" here) asdf:*central-registry*))

(handler-bind
    ((error
      (lambda (c)
        (unless (interactive-stream-p *standard-output*)
          (format *error-output* "Fatal error: ~A~%" c)
          (uiop:quit 1)))))
  (asdf:load-system :shcl-test)

  (let* ((run-tests-sym (intern "RUN-TESTS" (find-package "SHCL-TEST/MAIN")))
         (interactive-p (interactive-stream-p *standard-output*))
         (target-file (uiop:getenv "TEST_OUTPUT"))
         (stream
          (if target-file
              (open target-file :direction :output :if-exists :supersede :external-format :utf8)
              *standard-output*)))
    (funcall run-tests-sym :enable-colors interactive-p :output-stream stream)
    (finish-output stream))
  (uiop:quit 0))
