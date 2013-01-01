(defun best-move (state depth thresh childgen rank terminalp nextPlayer maxPlayer)
  (defun pick-random (choices)
    (cond
      ((null choices) nil)
      (t (nth (random (list-length choices)) choices))))

  (defun new-node (state lastmove score tag)
    (list state lastmove score tag))

  (defun node-lastmove(node)
    (nth 1 node))

  (defun node-score(node)
    (nth 2 node))

  (defun node-state(node)
    (nth 0 node))

  (defun node-label(node)
    (nth 3 node))

  (defun score-le (s1 s2)
    (cond
      ((eql s1 s2) t)
      ((eql s1 'NEGINF) t)
      ((eql s1 'POSINF) nil)
      ((eql s2 'NEGINF) nil)
      ((eql s2 'POSINF) t)
      (t (<= s1 s2))))

  (defun node-le (one two)
    (let ((s1 (node-score one)) (s2 (node-score two)))
      (score-le s1 s2)))
  
  (defun score-minus (x y)
    (cond
      ((eql x 'POSINF) 'POSINF)
      ((eql x 'NEGINF) 'NEGINF)
      ((eql y 'NEGINF) 'POSINF)
      ((eql y 'POSINF) 'NEGINF)
      (t (- x y))))

  (defun score-negate(x)
    (score-minus 0 x)) 

  (defun node-gt (one two)
    (not (node-le one two)))

  (defun score-ge (x y)
    (or (not (score-le x y)) (eql x y)))

  (defun min-node (one two)
    (if (node-le one two) one two))

  (defun max-node (one two)
    (if (node-gt one two) one two))

  (defun fetch-childstates (node player)
    (let ((children (funcall childgen (node-state node) player)))
      (sort
        (mapcar
          (lambda (x)
            (new-node
              (node-state x)
              (node-lastmove x)
              (funcall rank (node-state x))
              'GENERATED-CHILD))
          children)
        (lambda (x y)
          (if (eql player maxPlayer) (node-gt x y) (node-le x y))))))

  (defun is-noisy (parent child player)
    (let ((diff (score-minus (node-score parent) (node-score child))))
      (or
        (and
          (eql player maxPlayer)
          (score-ge diff thresh))
        (score-le diff (score-negate thresh)))))

  (defun alphabeta-search (node children depth alpha beta player)
    (cond
      ((or (null children) (node-le beta alpha))
        (if (eql player maxPlayer) alpha beta))
      (t
        (let ((test (alphabeta (car children)
                (if (is-noisy node (car children) player) depth (- depth 1))
                alpha beta (funcall nextPlayer player))))
          (if (eql player maxPlayer)
            (alphabeta-search node (cdr children)
              depth (max-node alpha test) beta player)
            (alphabeta-search node (cdr children)
              depth alpha (min-node beta test) player))))))

  (defun alphabeta (node depth alpha beta player)
    (cond
      ((or (= depth 0) (funcall terminalp (node-state node)))
        node)
      (t
        (let ((result
                (alphabeta-search node (fetch-childstates node player)
                  depth alpha beta player)))
        (new-node
          (node-state node)
          (if (eql (node-label node) 'ROOT)
            (node-lastmove result)
            (node-lastmove node))
          (node-score result)
          (if (eql (node-label node) 'ROOT)
            (node-label result)
            (node-label node)))))))

  (let ((allmoves (funcall childgen state maxPlayer)))
    (let
      ((result
        (alphabeta
          (new-node
            state
            (nth 1 (pick-random allmoves))
            (funcall rank state)
            'ROOT)
          depth
          (new-node state nil 'NEGINF 'DEFAULT-ALPHA)
          (new-node state nil 'POSINF 'DEFAULT-BETA) maxPlayer)))
      (node-lastmove result))))
