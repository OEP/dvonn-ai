(load "dvonn.lisp")

(let ((board (create-random-board 13 5)))
  (pretty-print-board board)
  (print (board-movable-pieces board 'W))
  (print (board-possible-moves board 'W))
)
