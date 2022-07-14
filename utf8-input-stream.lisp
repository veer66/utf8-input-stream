(defpackage #:utf8-input-stream
  (:use #:cl #:trivial-gray-streams #:zstd))

(in-package :utf8-input-stream)

(defparameter *line-buffer-size* #x10000)
(defparameter *line-tmp-size* #x10)
(defconstant +max-bytes-in-ch+ 4)

(defstruct stream-context
  binary-input-stream
  (pos 0 :type integer)
  (buf-pos 0 :type integer)
  buf
  (buf-len 0 :type integer))

(defclass character-input-stream (fundamental-character-input-stream)
  ((ctx :accessor ctx)))

(defun read-from-stream (ctx)
  (setf (stream-context-buf-len ctx)
	(read-sequence (stream-context-buf ctx)
		       (stream-context-binary-input-stream ctx))))

(defun new-buf ()
  (make-array *line-buffer-size* :fill-pointer t
				 :element-type '(unsigned-byte 8)))

(defun make-character-input-stream (binary-input-stream)
  (let ((s (make-instance 'character-input-stream))
	(ctx (make-stream-context)))
    (setf (stream-context-binary-input-stream ctx) binary-input-stream)
    (setf (stream-context-pos ctx) 0)
    (setf (stream-context-buf-pos ctx) 0)
    (setf (stream-context-buf ctx) (new-buf))
    (read-from-stream ctx)
    (setf (ctx s) ctx)
    s))

(defun end-of-stream? (ctx)
  (eq 0 (stream-context-buf-len ctx)))

(defun reset-buf-pos (ctx)
  (setf (stream-context-buf-pos ctx) 0))

(defmacro buf-is-consumed? (ctx)
  `(>= (stream-context-buf-pos ,ctx)
       (stream-context-buf-len ,ctx)))

(defmacro refill-buffer (ctx)
  `(when (buf-is-consumed? ,ctx)
    (read-from-stream ,ctx)
    (reset-buf-pos ,ctx)))

(defun make-ch-buf ()
  (make-array +max-bytes-in-ch+
	      :fill-pointer 0 :element-type '(unsigned-byte 8)))

(defun read-byte-from-buf (ctx)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((b (elt (stream-context-buf ctx) (stream-context-buf-pos ctx))))
    (incf (stream-context-buf-pos ctx))
    (incf (stream-context-pos ctx))
    b))

(define-condition character-encoding-error (error)
  ((pos :initarg :pos :reader pos)))

(defun managed-read-byte (ctx)
  (refill-buffer ctx)
  (when (end-of-stream? ctx)
    (error 'character-encoding-error :pos (stream-context-pos ctx)))
  (read-byte-from-buf ctx))

(defmacro one-byte-ch? (b0)
  `(eq 0 (mask-field (byte 1 7) ,b0)))

(defmacro two-bytes-ch? (b0)
  `(eq #b11000000 (mask-field (byte 3 5) ,b0)))

(defmacro three-bytes-ch? (b0)
  `(eq #b11100000 (mask-field (byte 4 4) ,b0)))

(defmacro four-bytes-ch? (b0)
  `(eq #b11110000 (mask-field (byte 5 3) ,b0)))

(defun fetch-1-byte-ch (ctx b0)
  (declare (ignore ctx))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (code-char b0))

(defun fetch-2-bytes-ch (ctx b0)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((b1 (managed-read-byte ctx)))
    (when (not (eq #b10000000 (mask-field (byte 2 6) b1)))      
      (error 'character-encoding-error :pos (stream-context-pos ctx)))
    (code-char (+ (ash (mask-field (byte 5 0) b0) 6)
		  (mask-field (byte 6 0) b1)))))

(defun fetch-3-bytes-ch (ctx b0)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((b1 (managed-read-byte ctx))
	(b2 (managed-read-byte ctx)))
    (when (or (not (eq #b10000000 (mask-field (byte 2 6) b1)))
	      (not (eq #b10000000 (mask-field (byte 2 6) b2))))
      (error 'character-encoding-error :pos (stream-context-pos ctx)))
    (code-char (+ (mask-field (byte 6 0) b2)
		  (ash (mask-field (byte 6 0) b1) 6)
		    (ash (mask-field (byte 4 0) b0) 12)))))

(defun fetch-4-bytes-ch (ctx b0)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((b1 (managed-read-byte ctx))
	(b2 (managed-read-byte ctx))
	(b3 (managed-read-byte ctx)))
    (when (or (not (eq #b10000000 (mask-field (byte 2 6) b1)))
	      (not (eq #b10000000 (mask-field (byte 2 6) b2)))
	      (not (eq #b10000000 (mask-field (byte 2 6) b3))))
      (error 'character-encoding-error :pos (stream-context-pos ctx)))
    (code-char (+ (mask-field (byte 6 0) b3)
		    (ash (mask-field (byte 6 0) b2) 6)
		    (ash (mask-field (byte 6 0) b1) 12)
		    (ash (mask-field (byte 4 0) b0) 18)))))

(defun fetch-ch (ctx)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((b0 (read-byte-from-buf ctx)))
    (cond
      ((one-byte-ch? b0) (fetch-1-byte-ch ctx b0))
      ((two-bytes-ch? b0) (fetch-2-bytes-ch ctx b0))
      ((three-bytes-ch? b0) (fetch-3-bytes-ch ctx b0))
      ((four-bytes-ch? b0) (fetch-4-bytes-ch ctx b0))
      (t (error 'character-encoding-error :stream-context-pos (pos ctx))))))

(defun read-char-from-buf (ctx)  
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop do
    (refill-buffer ctx)
    (when (end-of-stream? ctx)
      (return :EOF))
    (return (fetch-ch ctx))))

(defmethod stream-read-char ((s character-input-stream))
  (read-char-from-buf (ctx s)))

(defmethod stream-read-line ((s character-input-stream))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((ctx (ctx s))
	(ch-vec (make-array *line-tmp-size* :fill-pointer 0 :element-type 'character)))
    (loop for ch = (read-char-from-buf ctx)
	  do (case ch
	       (:EOF (return (values ch-vec t)))
	       (#\Newline (return (values ch-vec nil)))
	       (#\Return nil)
	       (otherwise (vector-push-extend ch ch-vec))))))
