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

(defpackage :shcl/core/posix-types
  (:use :common-lisp :cffi-grovel)
  (:export
   #:size-t #:ssize-t #:pid-t
   #:dirent #:d-name #:errno #:mode-t #:environ
   #:s-irusr #:s-iwusr #:s-ixusr #:s-irgrp #:s-iwgrp #:s-ixgrp #:s-iroth
   #:s-iwoth #:s-ixoth #:o-rdonly #:o-wronly
   #:o-rdwr #:o-creat #:o-trunc #:o-append #:f-getfd #:f-setfd #:fd-cloexec #:wuntraced
   #:eintr #:enoent #:dev-t #:ino-t #:nlink-t #:uid-t #:gid-t #:off-t #:blksize-t
   #:blkcnt-t #:time-t #:stat #:st-dev #:st-ino #:st-mode #:st-nlink #:st-uid
   #:st-gid #:st-rdev #:st-size #:st-blksize #:st-blocks #:st-atime #:st-mtime
   #:st-ctime #:at-symlink-nofollow #:path-max #:r-ok #:w-ok #:x-ok #:f-ok
   #:at-eaccess))
