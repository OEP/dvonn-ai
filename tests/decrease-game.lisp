(load "minimax.lisp")

(defun is-terminal (state)
  (or (= 0 (nth 0 state)) (= 0 (nth 1 state)))
)

(defun rank-state (state)
  (let ((me (nth 0 state)) (you (nth 1 state)))
    (cond
      ((<= me 0) 'NEGINF)
      ((<= you 0) 'POSINF)
      (t (- me you))))
  )

(defun apply-move (move state)
  (list (+ (nth 0 move) (nth 0 state))
    (+ (nth 1 move) (nth 1 state)))
)

(defun generate-children (state)
  (defun helper (moves)
    (cond
      ((null moves) nil)
      (t (cons (list (apply-move (car moves) state) (car moves))
        (helper (cdr moves))))))
  (helper '( (0 -1) (1 0) (0 1) (-1 0) ))
)

(best-move '(5 3) 3 #'generate-children #'rank-state #'is-terminal)
(best-move '(5 3) 1 #'generate-children #'rank-state #'is-terminal)
