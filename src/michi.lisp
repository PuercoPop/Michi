(in-package :michi)

;; (defvar *board* (make-sequence 'list 9 :initial-element :empty))
(defparameter +valid-move-inputs+ (list "TL" "TC" "TR"
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

(defmacro define-accessor (name accessor)
  `(progn
    (defun ,name (board)
       ,accessor)
    (defun (setf ,name) (value board)
       (if (eq (,name board) :empty)
           (setf ,accessor value)
           (error 'invalid-move :text "The square is not empty")))))

(define-accessor top-left (car board))

(define-accessor top-center (car (cdr board)))

(define-accessor top-right (car (cdr (cdr board))))

(define-accessor middle-left (car (cdr (cdr (cdr board)))))

(define-accessor middle-center (car (cdr (cdr (cdr (cdr board))))))

(define-accessor middle-right (car (cdr (cdr (cdr (cdr (cdr board)))))))

(define-accessor bottom-left (car (cdr (cdr (cdr (cdr (cdr (cdr board))))))))

(define-accessor bottom-center (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr board)))))))))

(define-accessor bottom-right (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr board))))))))))



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
    (update-player)))

(defun update-player ()
  "Updates current player"
  (setf *current-player* (car *players*))
  (setf *players* (cdr *players*)))

(defun read-player-move ()
  (let ((player-input (read-line)))
    (if (member player-input +valid-move-inputs+ :test #'string-equal)
        player-input
        (read-player-move))))

(defun print-current-turn-message (player board)
  (print-board board)
  (format t "It is ~A's turn. Input Your move~%" player)
  )

(defun print-eog-message (player board)
  "End of Game message"
  (print-board board)
  (format t "~A won. Congrats" player))

(defun main ()
  (let ((*players* (circular-list :player-1 :player-2))
        (*board* (make-sequence 'list 9 :initial-element :empty)))
    (declare (special *players*)
             (special *current-player*)
             (special *board*))

    (update-player)
    (loop
       until (game-ended? *board*)
       do
         (print-current-turn-message *current-player* *board*)
         (handler-case
             (player-move *current-player* (read-player-move) *board*)
           (invalid-move (message) (format t "~A~%" message))))

    (update-player)
    (print-eog-message *current-player*
                       *board*)))
