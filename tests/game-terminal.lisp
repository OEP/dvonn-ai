(load "dvonn.lisp")
(load "tests/dvonn-game.lisp")

(setf playertypes '(white (1 3)))

(defun pretty-color (c)
  (cond
    ((eql c color-white) "White")
    ((eql c color-black) "Black")
    ((eql c color-red) "Red")))

(defun dispatch (msg &rest args)
  (cond
    ((eql msg 'received-move)
      (apply #'on-received-move args))
    ((eql msg 'board-refresh)
      (apply #'on-board-update args))
    ((eql msg 'cannot-move)
      (apply #'on-cannot-move args))
    ((eql msg 'input-prompt)
      (apply #'on-input-prompt args))
    ((eql msg 'bad-input)
      (apply #'on-bad-input args))
    ((eql msg 'invalid-move-notstack)
      (apply #'on-invalid-move-notstack args))
    ((eql msg 'game-end)
      (apply #'on-game-end args))
    ((eql msg 'invalid-move-illegal)
      (apply #'on-invalid-move-illegal args))
    ((eql msg 'thinking)
      (apply #'on-thinking args))
    (t (format t "Unhandled message: ~A~%" msg))))

(defun on-thinking (color)
  (if (eql color color-white)
    (format t "White is thinking...~%")
    (format t "Black is thinking...~%")))

(defun on-game-end (wscore bscore)
  (cond
    ((> wscore bscore)
      (format t "White wins!~%"))
    ((> bscore wscore)
      (format t "Black wins!~%"))
    (t
      (format t "It's a tie!~%"))))

(defun on-invalid-move-illegal(from to)
  (format t "You can't move ~A onto ~A.~%~%" from to))

(defun on-invalid-move-notstack(from)
  (format t "'~A' is not a stack.~%~%" from))

(defun on-bad-input()
  (format t "Didn't understand!~%~%")
  (format t "Say something like: 3 0 W~%"))

(defun on-input-prompt (who)
  (format t "~A, what will you do?: " who))

(defun on-board-update (board n who wscore bscore)
  (pretty-print-board board t)
  (format t "Score: White ~A   ::   Black ~A~%~%" wscore bscore))

(defun on-received-move (who coord direction)
  (format t
    "~A moves ~A ~A.~%" (pretty-color who) coord direction))

(defun on-cannot-move (who)
  (format t
    "~A cannot move.~%" (pretty-color who)))

(defun render-board (board base move)
  (let ((filename (format nil "~A-move~A.svg" base move)))
    (board-to-svg board filename)
    (format t "Wrote board to '~A'~%" filename)))

(game-driver
  (create-random-board 11 5) #'dispatch)

(randomize-next-game)
