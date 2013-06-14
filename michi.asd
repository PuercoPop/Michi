(in-package :cl-user)

(asdf:defsystem #:michi
  :name "michi"
  :description "A tic-tac-toe program."
  :author "Javier Olaechea <pirata@gmail.com>"
  :version "20130401"
  :serial t
  :license "<3"
  :pathname "src/"
  :depends-on (#:string-case
               #:alexandria)
  :components ((:file "packages")
               (:file "conditions")
               (:file "player")
               (:file "michi")))
