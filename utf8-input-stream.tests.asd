;;;; rulegen.asd

(asdf:defsystem #:utf8-input-stream.tests
  :description "A UTF-8 string input stream over a binary stream for Common Lisp"
  :author "Vee Satayamas <vsatayamas@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:fiveam
               #:flexi-streams
               #:utf8-input-stream)
  :components ((:file "tests")))
