(load "dvonn.lisp")

(defun choice (choices)
  (nth (random (list-length choices)) choices))

(defun do-random-move (board color &optional (nomove nil))
  (pretty-print-board board)
  (let ((moves (board-possible-moves board color)))
    (cond
      ((and (null moves) nomove)
        (format t "~%Score: White ~A     Black ~A~%~%"
           (score-color board color-white) (score-color board color-black))
        'done)
      ((null moves)
        (format t "~%Can't move: ~A~%~%" color)
        (do-random-move board (if (eql color 'w) 'b 'w) t))
      (t
        (let ((move (choice moves)))
          (format t "~%Move: ~A~%~%" move)
          (do-random-move (apply-move board move)
            (if (eql color 'w) 'b 'w)))))))
  

(let ((board (create-random-board 13 5)))
  (do-random-move board 'W))
