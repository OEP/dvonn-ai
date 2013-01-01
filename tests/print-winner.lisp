(load "dvonn.lisp")
(load "tests/dvonn-game.lisp")

(cond
  ((/= 4 (length *args*))
    (format t
      "Usage: clisp ~A ~A ~A ~A ~A~%"
      "<script-name>"
      "<w-ply>"
      "<w-thresh>"
      "<b-ply>"
      "<b-thresh>")
    (quit 1)))

(setf playertypes
  (list
    (list
      (parse-integer (first *args*))
      (parse-integer (second *args*)))
    (list
      (parse-integer (third *args*))
      (parse-integer (fourth *args*)))))

(defun pretty-color (c)
  (cond
    ((eql c color-white) "White")
    ((eql c color-black) "Black")
    ((eql c color-red) "Red")))

(defun dispatch (msg &rest args)
  (cond
    ((eql msg 'game-end)
      (apply #'on-game-end args))
    (t nil)))

(defun on-game-end (wscore bscore)
  (cond
    ((> wscore bscore)
      (format t "WHITE ~A ~A~%" wscore bscore))
    ((< wscore bscore)
      (format t "BLACK ~A ~A~%" wscore bscore))
    (t
      (format t "TIE ~A ~A~%" wscore bscore))))

(game-driver
  (create-random-board 11 5) #'dispatch)

(randomize-next-game)
