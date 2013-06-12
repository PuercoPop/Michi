(in-package :michi)

;; (defvar *board* (make-sequence 'list 9 :initial-element :empty))
(defparameter +players+ (list :player-1 :player-2))
(defparameter +valid-moves+ (list "TL" "TC" "TR"
                                  "ML" "MC" "MR"
                                  "BL" "BC" "BR"))

(defun game-ended? (board)
  (or (not (has-empty-places? board))
      (has-one-player-won? board)))

(defun has-empty-places? (board)
  (find :empty board))

(defun has-one-player-won? (board)
  "Check individually if each player has won."
  (or (has-player-won? :player-1 board)
      (has-player-won? :player-2 board)))

(defun has-player-won? (player board)
  (or (are-the-same? player
                     (top-left board)
                     (top-center board)
                     (top-right board))
      (are-the-same? player
                     (middle-left board)
                     (middle-center board)
                     (middle-right board))
      (are-the-same? player
                     (bottom-left board)
                     (bottom-center board)
                     (bottom-right board))
      (are-the-same? player
                     (top-left board)
                     (middle-left board)
                     (bottom-left board))
      (are-the-same? player
                     (top-center board)
                     (middle-center board)
                     (bottom-center board))
      (are-the-same? player
                     (top-right board)
                     (middle-right board)
                     (bottom-right board))
      (are-the-same? player
                     (top-left board)
                     (middle-center board)
                     (bottom-right board))
      (are-the-same? player
                     (top-right board)
                     (middle-center board)
                     (bottom-left board))))

(defun top-left (board)
  (car board))

(defun (setf top-left) (value board)
  (setf (car board) value))

(defun top-center (board)
  (car (cdr board)))

(defun (setf top-center) (value board)
  (setf (car (cdr board)) value))

(defun top-right (board)
  (car (cdr (cdr board))))

(defun (setf top-right) (value board)
  (setf (car (cdr (cdr board))) value))

(defun middle-left (board)
  (car (cdr (cdr (cdr board)))))

(defun (setf middle-center) (value board)
  (setf (car (cdr (cdr (cdr (cdr board))))) value))

(defun middle-center (board)
  (car (cdr (cdr (cdr (cdr board))))))

(defun (setf middle-center) (value board)
  (setf (car (cdr (cdr (cdr (cdr board))))) value))

(defun middle-right (board)
  (car (cdr (cdr (cdr (cdr (cdr board)))))))

(defun (setf middle-right) (value board)
  (setf (car (cdr (cdr (cdr (cdr (cdr board)))))) value))

(defun bottom-left (board)
  (car (cdr (cdr (cdr (cdr (cdr (cdr board))))))))

(defun (setf bottom-left) (value board)
  (setf (car (cdr (cdr (cdr (cdr (cdr board)))))) value))

(defun bottom-center (board)
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr board)))))))))

(defun (setf bottom-center) (value board)
  (setf (car (cdr board)) value))

(defun bottom-right (board)
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr board))))))))))

(defun (setf bottom-right) (value board)
  (setf (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr board))))))))) value))

(defun are-the-same? (&rest xs)
  (every (lambda (pair) (apply #'eq pair)) (transitive-pairing xs)))

(defun transitive-pairing (xs)
  (cond ((> 2 (length xs)) nil)
        (t  (cons `(,(car xs) ,(cadr xs)) (transitive-pairing (cdr xs))))))

(defun print-board (board)
  (let* ((length (length board))
         (column (mod length 3))
         (padding (if (eq :empty (car board))
                      "   "
                      "")))
    (cond ((= length 0) nil)
          ((= column 1)
           (format t "  ~A~A~%" (car board) padding)
           (unless (= length 1)
             (format t "--------------------------------~%"))
           (print-board (cdr board)))
          (t
           (format t " ~A~A |" (car board) padding)
           (print-board (cdr board))))))


(defun player-move (player move board)
  (prog1
      (string-case (move)
        ("TL" (setf (top-left board) player))
        ("TC" (setf (top-center board) player))
        ("TR" (setf (top-right board) player))
        ("ML" (setf (middle-left board) player))
        ("MC" (setf (middle-center board) player))
        ("MR" (setf (middle-right board) player))
        ("BL" (setf (bottom-left board) player))
        ("BC" (setf (bottom-center board) player))
        ("BR" (setf (bottom-right board) player)))
    (update-player player)))

(defun update-player (player)
  (if (eq *current-player* :player-1)
        (setf *current-player* :player-2)
        (setf *current-player* :player-1)))

(defun read-player-move ()
  (let ((player-input (read-line)))
    (if (member player-input +valid-moves+ :test #'string-equal)
        player-input
        (read-player-move))))

(defun print-current-turn-message (player board)
  (print-board board)
  (format t "It is ~A's turn. Input Your move~%" player)
  )

(defun print-eog-message (player board)
  (update-player player)
  (format t "~A won. Congrats" player))

(defun main ()
  (let ((*current-player* :player-1)
        (*board* (make-sequence 'list 9 :initial-element :empty)))
    (declare (special *current-player*)
             (special *board*))

    (loop
       until (game-ended? *board*)
       do
         (print-current-turn-message *current-player* *board*)
         (player-move *current-player* (read-player-move) *board*))

    (print-eog-message *current-player*
                       *board*)))
