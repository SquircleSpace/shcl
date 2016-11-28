(defpackage :shcl/core/posix-types
  (:use :common-lisp :cffi-grovel)
  (:export
   #:size-t #:ssize-t #:pid-t #:posix-spawn-file-actions-t
   #:posix-spawnattr-t #:dirent #:d-name #:errno #:mode-t #:environ
   #:s-irusr #:s-iwusr #:s-irgrp #:s-iroth #:o-rdonly #:o-wronly
   #:o-rdwr #:o-creat #:o-trunc #:o-append #:f-getfd #:wuntraced
   #:eintr #:dev-t #:ino-t #:nlink-t #:uid-t #:gid-t #:off-t #:blksize-t
   #:blkcnt-t #:time-t #:stat #:st-dev #:st-ino #:st-mode #:st-nlink #:st-uid
   #:st-gid #:st-rdev #:st-size #:st-blksize #:st-blocks #:st-atime #:st-mtime
   #:st-ctime #:at-symlink-nofollow))
