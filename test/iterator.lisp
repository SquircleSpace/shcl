;; Copyright 2018 Bradley Jensen
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

(defpackage :shcl/test/iterator
  (:use :common-lisp :prove :shcl/core/iterator :shcl/test/foundation)
  (:import-from :shcl/core/utility #:optimization-settings))
(in-package :shcl/test/iterator)

(optimization-settings)

(define-test iterator-tests
  (let* ((vector #(1 2 3 4 5))
         (list '(a b c d e))
         (seq (fset:seq 'q 'w 'e 'r))
         (vector-iterator (vector-iterator vector))
         (list-iterator (list-iterator list))
         (generic-iterator (iterator seq)))
    (is (coerce (iterator-values vector-iterator) 'list)
        (coerce vector 'list)
        :test #'equal)
    (is (coerce (iterator-values list-iterator) 'list)
        list
        :test #'equal)
    (is (coerce (iterator-values generic-iterator) 'list)
        (fset:convert 'list seq)
        :test #'equal)))

(define-test lookahead-iterator-tests
  (let* ((count 5)
         (iter (make-iterator (:type 'lookahead-iterator)
                 (when (equal 0 count)
                   (stop))
                 (decf count)))
         fork)
    (is 5 count :test #'equal)
    (is 4 (peek-lookahead-iterator iter) :test #'equal)
    (is 4 count :test #'equal)
    (setf fork (fork-lookahead-iterator iter))
    (is 4 (peek-lookahead-iterator fork) :test #'equal)
    (is 4 (peek-lookahead-iterator iter) :test #'equal)
    (is 4 count :test #'equal)
    (is 4 (next iter) :test #'equal)
    (is 3 (next iter) :test #'equal)
    (is 2 (next iter) :test #'equal)
    (is 2 count :test #'equal)
    (is 4 (peek-lookahead-iterator fork) :test #'equal)
    (is 2 count :test #'equal)
    (is 4 (next fork) :test #'equal)
    (is 3 (next fork) :test #'equal)
    (is 2 count :test #'equal)
    (is 1 (next iter) :test #'equal)
    (is 1 count :test #'equal)
    (move-lookahead-to fork iter)
    (is 0 (next iter) :test #'equal)
    (is 0 count :test #'equal)
    (is nil (next iter) :test #'equal)
    (is 0 count :test #'equal)
    (is 0 (next fork) :test #'equal)
    (is 0 count :test #'equal)
    (is nil (next fork) :test #'equal)
    (is 0 count :test #'equal)))
