(load "minimax.lisp")
(load "dvonn.lisp")
(load "svg.lisp")

;; Try to load a previous random state.
(if (probe-file "random-state.txt")
  (with-open-file (s "random-state.txt" :direction :input)
    (let ((state (read s)))
      (loop repeat 10 collect (random 100 state))
      (setf *random-state* state))))

(setf playernum 0)
(setf playertags (list color-white color-black))
(setf playertypes '((1 POSINF) (1 3)))

(defun next-playernum()
  (if (= playernum 0) 1 0))

(defun next-playertag()
  (nth (next-playernum) playertags))

(defun current-playertag()
  (nth playernum playertags))

(defun swap-players()
  (setf playernum (next-playernum)))

(defun current-playertype()
  (nth playernum playertypes))

(defun nextPlayer (currentPlayer)
  (if (eql currentPlayer color-white)
    color-black
    color-white))

(defun terminalp (board)
  (and
    (eql nil (board-possible-moves board color-white))
    (eql nil (board-possible-moves board color-black))))

(defun rank-state (board)
  (let ((me (score-color board (current-playertag)))
        (end (terminalp board))
        (them (score-color board (next-playertag))))
    (cond
      ((and end (> me them)) 'POSINF)
      ((and end (> them me)) 'NEGINF)
      (t (- me them)))))

(defun generate-children (board color)
  (defun helper (moves)
    (cond
      ((null moves) nil)
      (t (cons (list (apply-move board (car moves)) (car moves))
            (helper (cdr moves))))))
  (let ((possibles (board-possible-moves board color)))
    (helper possibles)))

(defun pick-best-move (board)
  (best-move board (car (current-playertype)) (cadr (current-playertype))
    #'generate-children #'rank-state #'terminalp #'nextPlayer (current-playertag)))

(defun game-driver (board callback &optional (movecount 1) (nomove nil))
  (funcall callback 'board-refresh board movecount
    (current-playertag)
    (score-color board color-white)
    (score-color board color-black))
  (let ((moves (board-possible-moves board (current-playertag))))
    (cond
      ((and (not nomove) (null moves))
        (funcall callback 'cannot-move (current-playertag))
        (swap-players)
        (game-driver board callback (+ movecount 1) t))
      ((null moves)
        (funcall callback 'game-end
          (score-color board color-white)
          (score-color board color-black)))
      (t
        (let ((move (move-dispatch board callback)))
          (cond
            ((null move) (game-driver board callback movecount nomove))
            (t
              (funcall callback 'received-move
                (current-playertag)
                (car move)
                (dir2sym (cadr move)))
              (swap-players)
              (game-driver
                (apply-move board move)
                callback
                (+ movecount 1)))))))))

(defun move-dispatch (board callback)
  (cond
    ((consp (current-playertype))
      (do-smart-move board callback))
    (t
      (do-interactive-move board callback))))

(defun safe-read (i src sstream)
  (if (< i (length src)) (read sstream) nil))

(defun str2move (str)
  (with-input-from-string (ss str)
    (let ((i (read ss nil nil))
          (j (read ss nil nil))
          (d (str2dir (read ss nil nil))))
      (cond
        ((and (integerp i) (integerp j) (not (null d)))
          (list (list i j) d))
        (t nil)))))

(defun do-interactive-move (board callback)
  (funcall callback 'input-prompt (current-playertype))
  (let ((s (read-line)))
    (let ((move (str2move s)))
      (cond
        ((null move)
          (funcall callback 'bad-input)
          nil)
        ((not (legal-move board move (current-playertag)))
          (let ((piece (piece-at board (car move))))
            (if (not (is-stack piece))
              (funcall callback 'invalid-move-notstack
                piece)
              (funcall callback 'invalid-move-illegal
                (dvonn-stack-tostring (piece-at board (car move)))
                (dvonn-stack-tostring
                  (piece-at board (move-move board move))))))
          nil)
        (t move)))))
        
(defun do-smart-move (board callback)
  (funcall callback 'thinking (current-playertag))
  (pick-best-move board))
  
;; Save the random state so we get a different board next time.
(defun randomize-next-game()
  (with-open-file
    (s "random-state.txt" :direction :output :if-exists :supersede)
    (let ((state (make-random-state *random-state*)))
      (print state s)
    (loop repeat 10 collect (random 100 state)))))
