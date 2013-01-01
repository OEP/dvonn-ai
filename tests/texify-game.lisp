(load "dvonn.lisp")
(load "tests/dvonn-game.lisp")

(defun pretty-color (c)
  (cond
    ((eql c color-white) "White")
    ((eql c color-black) "Black")
    ((eql c color-red) "Red")))

(with-open-file (slog "game-log.tex"
                  :direction :output
                  :if-exists :supersede
                  :if-does-not-exist :create)
  (defun dispatch (msg &rest args)
    (cond
      ((eql msg 'received-move)
        (apply #'on-received-move args))
      ((eql msg 'board-refresh)
        (apply #'on-board-update args))
      ((eql msg 'cannot-move)
        (apply #'on-cannot-move args))
      (t (format t "Unhandled message: ~A~%" msg))))

  (defun on-board-update (board n who wscore bscore)
    (format slog
      "\\subsection{~A Move}~%" (pretty-color who))
    (format slog
      "\\includegraphics[width=3in]{fig/example-move~A.pdf} \\\\~%" n)
    (format slog
      "White: ~A \\\\~%Black: ~A \\\\~%" wscore bscore)
    (render-board board "example" n))

  (defun on-received-move (who coord direction)
    (format slog
      "~A moves ~A ~A. \\\\~%" (pretty-color who) coord direction))
  
  (defun on-cannot-move (who)
    (format slog
      "~A cannot move. \\\\~%" (pretty-color who)))

  (defun render-board (board base move)
    (let ((filename (format nil "~A-move~A.svg" base move)))
      (board-to-svg board filename)
      (format t "Wrote board to '~A'~%" filename)))
  
  (game-driver
    (create-random-board 11 5) #'dispatch))
