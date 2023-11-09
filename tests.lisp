(defpackage #:utf8-input-stream.tests
  (:use :cl))

(in-package #:utf8-input-stream.tests)

(5am:def-suite utf8-test-suite)

(5am:in-suite utf8-test-suite)

(defmacro with-utf8-input-stream ((var source) &body body)
  `(let ((,var (utf8-input-stream:make-utf8-input-stream
                (flex:make-in-memory-input-stream
                 (flex:string-to-octets ,source :external-format
                                        (flex:make-external-format :utf-8))))))
     ,@body))

(5am:def-test read-a-single-ascii-character-of-a-string ()
  (with-utf8-input-stream (s "a simple string")
    (5am:is (char-equal (read-char s) #\a))))

(5am:def-test peek-a-single-ascii-character-of-a-string ()
  (with-utf8-input-stream (s "a simple string")
    (5am:is (char-equal (peek-char t s) (read-char s)))))

(5am:def-test unread-a-single-ascii-character-of-a-string ()
  (with-utf8-input-stream (s "a simple string")
    (unread-char (read-char s) s)
    (5am:is (char-equal (peek-char t s) #\a))))

(5am:def-test read-a-single-utf8-character-of-a-string ()
  (with-utf8-input-stream (s "été")
    (5am:is (string-equal (string (read-char s)) "é"))))

(5am:def-test peek-a-single-utf8-character-of-a-string ()
  (with-utf8-input-stream (s "été")
    (5am:is (string-equal (string (peek-char t s))
                          (string (read-char s))))))

(5am:def-test unread-a-single-utf8-character-of-a-string ()
  (with-utf8-input-stream (s "été")
    (unread-char (read-char s) s)
    (5am:is (string-equal (string (peek-char t s)) "é"))))
