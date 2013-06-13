(in-package :michi)

(define-condition invalid-move (error)
  ((text :initarg :text :reader :text)))
