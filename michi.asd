(in-package :cl-user)

(asdf:defsystem #:michi
  :name "michi"
  :description "A tic-tac-toe program."
  :author "Javier Olaechea <pirata@gmail.com>"
  :version "20130401"
  :serial t
  :license "<3"
  :pathname "src/"
  :components ((:file "packages")
               (:file "michi")))
