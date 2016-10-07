(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3) (compilation-speed 0)))

(let ((here (truename ".")))
  (push here asdf:*central-registry*))

(handler-bind
    ((error
      (lambda (c)
        (format *error-output* "Fatal error: ~A~%" c)
        (uiop:quit 1))))
  (asdf:compile-system :shcl)
  (asdf:load-system :shcl)
  (asdf:compile-system :shcl-test)
  (asdf:load-system :shcl-test)
  (symbol-macrolet
      ((enable-colors (symbol-value (intern "*ENABLE-COLORS*" (find-package "PROVE"))))
       (test-result-output (symbol-value (intern "*TEST-RESULT-OUTPUT*" (find-package "PROVE"))))
       (env-sym (intern "ENV" (find-package "SHCL.ENVIRONMENT")))
       (run-test-all-sym (intern "RUN-TEST-ALL" (find-package "PROVE"))))
    (unless (interactive-stream-p *standard-output*)
      (setf enable-colors nil)
      (setf test-result-output
           (open (funcall env-sym "TEST_OUTPUT" "test-results.txt")
                 :direction :output :if-exists :supersede :external-format :utf8)))

    (funcall run-test-all-sym)

    (unless (interactive-stream-p *standard-output*)
      (finish-output test-result-output)))
  (uiop:quit 0))
