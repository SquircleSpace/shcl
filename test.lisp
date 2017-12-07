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

  (let* ((env-sym (intern "ENV" (find-package "SHCL/CORE/ENVIRONMENT")))
         (run-tests-sym (intern "RUN-TESTS" (find-package "SHCL-TEST/MAIN")))
         (interactive-p (interactive-stream-p *standard-output*))
         (target-file (funcall env-sym "TEST_OUTPUT" nil))
         (stream
          (if target-file
              (open target-file :direction :output :if-exists :supersede :external-format :utf8)
              *standard-output*)))
    (funcall run-tests-sym :enable-colors interactive-p :output-stream stream)
    (finish-output stream))
  (uiop:quit 0))
