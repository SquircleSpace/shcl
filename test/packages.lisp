(defpackage :shcl-test.suites
  (:use :common-lisp :fiveam)
  (:export #:shcl #:shcl-failing #:lexer #:lexer-failing #:environment #:utility))

(defpackage :shcl-test.lexer
  (:use :common-lisp :fiveam :shcl-test.suites :shcl.lexer))

(defpackage :shcl-test.environment
  (:use :common-lisp :fiveam :shcl-test.suites :shcl.environment))

(defpackage :shcl-test.utility
  (:use :common-lisp :fiveam :shcl-test.suites :shcl.utility))
