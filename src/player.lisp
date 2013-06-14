(in-package :michi)

(defclass player ()
  ((name :initarg :player-name)))

(defgeneric make-move (player)
  (:documentation "Return the player's choice for the given board. The board
should either be passed as a parameter be a dynamic-variable, *board*"))

(defmethod print-object ((player player) stream)
  (with-slots (name) player
    (format stream "~S" name)))

(defparameter +valid-move-inputs+ (list "TL" "TC" "TR"
                                        "ML" "MC" "MR"
                                        "BL" "BC" "BR"))

(defclass human-player (player)
  ())

(defmethod make-move ((player human-player))
  (let ((player-input (read-line)))
    (if (member player-input +valid-move-inputs+ :test #'string-equal)
        player-input
        (make-move player))))

(defclass cpu-player (player)
  ())
