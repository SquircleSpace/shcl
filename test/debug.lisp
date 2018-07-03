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

(defpackage :shcl/test/debug
  (:use :common-lisp :shcl/core/utility :shcl/core/debug
        :shcl/test/foundation :prove))
(in-package :shcl/test/debug)

(define-test no-undocumented-symbols
  (is (fset:size (undocumented-symbols))
      0
      "No new undocumented symbols found"))
