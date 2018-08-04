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

(defpackage :shcl/core/positional-stream
  (:use :common-lisp :shcl/core/utility :trivial-gray-streams)
  (:import-from :shcl/core/data #:define-data #:clone)
  (:export
   #:positional-stream #:positional-stream-line
   #:positional-stream-column #:positional-stream-offset
   #:positional-stream-underlying-stream
   #:make-position-record-from-positional-stream
   #:position-record-line #:position-record-column
   #:position-record-offset #:position-record
   #:positional-input-stream #:positional-output-stream))
(in-package :shcl/core/positional-stream)

(optimization-settings)

(defgeneric positional-stream-line (stream)
  (:documentation
   "Returns the current line of the stream or nil if it is unknown.

The first line is line 0."))

(defgeneric positional-stream-column (stream)
  (:documentation
   "Returns the current column of the stream or nil if it is unknown.

The first column is column 0."))

(defgeneric positional-stream-offset (stream)
  (:documentation
   "Returns the stream's offset from the start of the stream or nil
if it is unknown.

When the stream is at the beginning, the return value is 0.  Each
character that travels through the stream increments the offset by 1.
For input streams, peeking does not increment the offset.  Unreading a
character decrements the offset."))

(defgeneric make-position-record-from-positional-stream (stream)
  (:documentation
   "Create a `position-record' from a positional stream.

This allows you to easily capture all the information about where a
stream is."))

(defgeneric positional-stream-underlying-stream (stream)
  (:documentation
   "Retrieve the stream that a given `positional-stream' interacts
with."))

(defgeneric position-record-line (position-record)
  (:documentation
   "Returns the line slot of a `position-record'."))

(defgeneric position-record-column (position-record)
  (:documentation
   "Returns the column slot of a `position-record'."))

(defgeneric position-record-offset (position-record)
  (:documentation
   "Returns the offset slot of a `position-record'."))

(define-data position-record ()
  ((line
    :initform 0
    :initarg :line
    :type (integer 0)
    :reader position-record-line)
   (column
    :initform 0
    :initarg :column
    :type (integer 0)
    :reader position-record-column)
   (offset
    :initform 0
    :initarg :offset
    :type (integer 0)
    :reader position-record-offset))
  (:documentation
   "A class that encapsulates all the information about a location in
a character stream."))

(defmethod print-object ((position position-record) stream)
  (print-unreadable-object (position stream :type t)
    (with-slots (line column offset) position
      (format stream "line ~A, column ~A, offset ~A"
              line column offset))))

(defclass positional-stream (fundamental-character-stream)
  ((previous-line-length
    :initform nil
    :type (or null (integer 0)))
   (position-record
    :initarg :initial-position
    :initform (make-instance 'position-record)
    :type position-record)
   (underlying-stream
    :initarg :underlying-stream
    :initform (required)
    :reader positional-stream-underlying-stream
    :type stream))
  (:documentation
   "This class is a wrapper around an existing stream that tries to
keep track of the stream's line, column, and overall offset."))

(defmethod positional-stream-line ((stream stream))
  (when-let ((record (make-position-record-from-positional-stream stream)))
    (position-record-line record)))

(defmethod positional-stream-column ((stream stream))
  (when-let ((record (make-position-record-from-positional-stream stream)))
    (position-record-column record)))

(defmethod positional-stream-offset ((stream stream))
  (when-let ((record (make-position-record-from-positional-stream stream)))
    (position-record-offset record)))

(defmethod make-position-record-from-positional-stream ((stream stream))
  nil)

(defmethod positional-stream-line ((stream positional-stream))
  (with-slots (position-record) stream
    (position-record-line position-record)))

(defmethod positional-stream-column ((stream positional-stream))
  (with-slots (position-record) stream
    (position-record-column position-record)))

(defmethod positional-stream-offset ((stream positional-stream))
  (with-slots (position-record) stream
    (position-record-offset position-record)))

(defmethod make-position-record-from-positional-stream ((stream positional-stream))
  (with-slots (position-record) stream
    (clone position-record)))

(defclass positional-input-stream
    (positional-stream fundamental-character-input-stream)
  ()
  (:documentation
   "A `positional-stream' for wrapping input streams."))

(defclass positional-output-stream
    (positional-stream fundamental-character-output-stream)
  ()
  (:documentation
   "A `positional-stream' for wrapping output streams."))

(defun handle-read-char (stream char)
  (with-slots (previous-line-length position-record) stream
    (with-slots
          (line column offset)
        position-record
      (case char
        (:eof)
        (#\Newline
         (incf offset)
         (incf line)
         (setf previous-line-length column)
         (setf column 0))
        (otherwise
         (incf offset)
         (incf column)
         (setf previous-line-length nil))))))

(defun handle-unread-char (stream char)
  (with-slots (previous-line-length position-record) stream
    (with-slots
          (line column offset)
        position-record
      (case char
        (:eof)
        (#\Newline
         (decf offset)
         (decf line)
         (setf column (or previous-line-length (error "Cannot unread that far")))
         (setf previous-line-length nil))
        (otherwise
         (decf offset)
         (decf column)
         (setf previous-line-length nil))))))

(defmethod stream-read-char ((stream positional-input-stream))
  (with-slots (underlying-stream) stream
    (let ((c (read-char underlying-stream nil :eof)))
      (handle-read-char stream c)
      c)))

(defmethod stream-unread-char ((stream positional-input-stream) char)
  (with-slots (underlying-stream) stream
    (prog1
        (unread-char char underlying-stream)
      (handle-unread-char stream char))))

(defmethod stream-read-char-no-hang ((stream positional-input-stream))
  (with-slots (underlying-stream) stream
    (let ((c (read-char-no-hang underlying-stream nil :eof)))
      (handle-read-char stream c)
      c)))

(defmethod stream-peek-char ((stream positional-input-stream))
  (with-slots (underlying-stream) stream
    (peek-char nil underlying-stream nil :eof)))

(defmethod stream-listen ((stream positional-input-stream))
  (with-slots (underlying-stream) stream
    (listen underlying-stream)))

(defmethod stream-clear-input ((stream positional-input-stream))
  (with-slots (underlying-stream) stream
    (clear-input underlying-stream)))

(defmethod stream-write-char ((stream positional-output-stream) char)
  (with-slots (underlying-stream) stream
    (prog1
        (write-char char underlying-stream)
      (handle-read-char stream char))))

(defmethod stream-line-column ((stream positional-output-stream))
  (with-slots (position-record) stream
    (position-record-column position-record)))

(defmethod stream-finish-output ((stream positional-output-stream))
  (with-slots (underlying-stream) stream
    (finish-output underlying-stream)))

(defmethod close ((stream positional-stream) &key abort)
  (with-slots (underlying-stream) stream
    (close underlying-stream :abort abort)))

(defmethod open-stream-p ((stream positional-stream))
  (with-slots (underlying-stream) stream
    (open-stream-p underlying-stream)))

(defmethod stream-element-type ((stream positional-stream))
  (with-slots (underlying-stream) stream
    (stream-element-type underlying-stream)))
