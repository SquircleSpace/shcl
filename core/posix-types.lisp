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
   #:st-ctime #:at-symlink-nofollow #:path-max))
