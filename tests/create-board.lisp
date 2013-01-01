(load "dvonn.lisp")

(defun do-test (tests)
  (cond
    ((null tests) 'Done)
    (t
      (format t "~%~%")
      (format t "Size: ~A~%" (car tests))
      (format t "~A~%" '=================================)
      (pretty-print-board
        (create-random-board (nth 0 (car tests)) (nth 1 (car tests))))
      (do-test (cdr tests))
    )
  )
)

(do-test '(
  (3 3)
  (3 5)
  (5 3)
  (11 5) ; Standard DVONN board.
))
