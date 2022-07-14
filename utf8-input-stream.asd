;;;; rulegen.asd

(asdf:defsystem #:utf8-input-stream
  :description "A UTF-8 string input stream over a binary stream for Common Lisp"
  :author "Vee Satayamas <vsatayamas@gmail.com>"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:babel #:trivial-gray-streams)
  :components ((:file "utf8-input-stream")))
