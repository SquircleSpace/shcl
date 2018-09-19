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

(in-package :shcl/core/posix-types)

(include "errno.h" "sys/types.h" "sys/stat.h" "fcntl.h" "dirent.h")

;; Types
(ctype size-t "size_t")
(ctype ssize-t "ssize_t")
(ctype pid-t "pid_t")
(ctype mode-t "mode_t")
(ctype dev-t "dev_t")
(ctype ino-t "ino_t")
(ctype nlink-t "nlink_t")
(ctype uid-t "uid_t")
(ctype gid-t "gid_t")
(ctype off-t "off_t")
(ctype blksize-t "blksize_t")
(ctype blkcnt-t "blkcnt_t")
(ctype time-t "time_t")

(cstruct stat "struct stat"
         (st-dev "st_dev" :type dev-t)
         (st-ino "st_ino" :type ino-t)
         (st-mode "st_mode" :type mode-t)
         (st-nlink "st_nlink" :type nlink-t)
         (st-uid "st_uid" :type uid-t)
         (st-gid "st_gid" :type gid-t)
         (st-rdev "st_rdev" :type dev-t)
         (st-size "st_size" :type off-t)
         (st-blksize "st_blksize" :type blksize-t)
         (st-blocks "st_blocks" :type blkcnt-t)
         (st-atime "st_atime" :type time-t)
         (st-mtime "st_mtime" :type time-t)
         (st-ctime "st_ctime" :type time-t))

(cstruct dirent "struct dirent"
  (d-name "d_name" :type :char :count 1))

;; The ever-important errno
(cvar ("errno" errno) :int)

(cvar ("environ" environ) (:pointer :string))

(constant (s-irusr "S_IRUSR"))
(constant (s-iwusr "S_IWUSR"))
(constant (s-ixusr "S_IXUSR"))

(constant (s-irgrp "S_IRGRP"))
(constant (s-iwgrp "S_IWGRP"))
(constant (s-ixgrp "S_IXGRP"))

(constant (s-iroth "S_IROTH"))
(constant (s-iwoth "S_IWOTH"))
(constant (s-ixoth "S_IXOTH"))

(constant (o-rdonly "O_RDONLY"))
(constant (o-wronly "O_WRONLY"))
(constant (o-rdwr "O_RDWR"))
(constant (o-creat "O_CREAT"))
(constant (o-trunc "O_TRUNC"))
(constant (o-append "O_APPEND"))
(constant (f-getfd "F_GETFD"))
(constant (f-setfd "F_SETFD"))
(constant (fd-cloexec "FD_CLOEXEC"))
(constant (wuntraced "WUNTRACED"))
(constant (eintr "EINTR"))
(constant (enoent "ENOENT"))
(constant (at-symlink-nofollow "AT_SYMLINK_NOFOLLOW"))
(constant (path-max "PATH_MAX"))

(constant (r-ok "R_OK"))
(constant (w-ok "W_OK"))
(constant (x-ok "X_OK"))
(constant (f-ok "F_OK"))
(constant (at-eaccess "AT_EACCESS"))
