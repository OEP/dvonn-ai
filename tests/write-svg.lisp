(load "dvonn.lisp")
(load "svg.lisp")

(board-to-svg
  (create-random-board 11 5)
  "board.svg")
