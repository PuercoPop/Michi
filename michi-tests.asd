(in-package :cl-user)
(asdf:defsystem #:michi-tests
  :depends-on (michi fiveam)
  :pathname "t/"
  :serial t
  :components ((:file "packages")
               (:file "board-accessors")))
