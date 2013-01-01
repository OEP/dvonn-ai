
; Projecting hexagonal tiles on 2D grid means funny rules for calculating
; the movements. The following are constants describing the deltas to the array
; indices for moving in the legal directions in DVONN.

; Store legal deltas in form (EE/OE OO/EO)
(setf NE '(( 0 -1) ( 1 -1)))
(setf NW '((-1 -1) ( 0 -1)))
(setf SE '(( 0  1) ( 1  1)))
(setf SW '((-1  1) ( 0  1)))
(setf E  '(( 1  0) ( 1  0)))
(setf W  '((-1  0) (-1  0)))
(setf ALL-DIRECTIONS (list NE NW W SW SE E))

(defun dir2sym (dir)
  (cond
    ((equal dir NE) 'NE)
    ((equal dir NW) 'NW)
    ((equal dir SE) 'SE)
    ((equal dir SW) 'SW)
    ((equal dir E) 'E)
    ((equal dir W) 'W)
    (t nil)))

(defun str2dir (dir)
  (let ((strdir (string-upcase (if (not (stringp dir)) (write-to-string dir) dir))))
    (cond
      ((equal strdir "NE") NE)
      ((equal strdir "NW") NW)
      ((equal strdir "SE") SE)
      ((equal strdir "SW") SW)
      ((equal strdir "E") E)
      ((equal strdir "W") W)
      (t nil))))

(defun move2sym (move)
  (list 'move (car move) (dir2sym (cadr move))))

(setf GRID-NOTILE 'X)
(setf GRID-EMPTY 'O)
(setf COLOR-RED 'R)
(setf COLOR-BLACK 'B)
(setf COLOR-WHITE 'W)
(setf TYPE-NORMAL 'N)
(setf TYPE-CONTROL 'C)

;; Data structure to model a dvonn stack.
;; If stype is omitted, the stack type is guessed by the color.
(defun dvonn-stack (color &optional (height 1) (stype nil))
  (cond
    ((eql stype nil)
      (list height color
        (if (eql color COLOR-RED) TYPE-CONTROL TYPE-NORMAL)))
    (t (list height color stype))))

(defun stack-pieces (from to)
  (dvonn-stack
    (stack-color from)
    (+ (stack-height from) (stack-height to))
    (if (or (eql (stack-type from) TYPE-CONTROL)
            (eql (stack-type to) TYPE-CONTROL))
      TYPE-CONTROL
      TYPE-NORMAL)))

(defun is-stack (stack)
  (and (consp stack) (= (list-length stack) 3)))

(defun is-color (stack color)
  (and (consp stack) (eql (stack-color stack) color)))

(defun is-type (stack typ)
  (and (consp stack) (eql (stack-type stack) typ)))

(defun stack-color (stack)
  (nth 1 stack))

(defun stack-height (stack)
  (nth 0 stack))

(defun stack-type (stack)
  (nth 2 stack))

(defun dvonn-stack-tostring (stack)
  (cond
    ((eql stack GRID-NOTILE) " ")
    ((eql stack GRID-EMPTY) "O")
    (t (format nil "~A~A~A" (stack-height stack)
      (stack-color stack)
      (stack-type stack)))))

;; Data structure to model a dvonn board.
(defun dvonn-board (width height grid &optional (deltas nil))
  (list width height grid deltas))

(defun board-width (board)
  (nth 0 board))

(defun board-height (board)
  (nth 1 board))

(defun board-grid (board)
  (nth 2 board))

(defun board-deltas (board)
  (nth 3 board))

;; Returns item at (i,j) on 2D grid.
(defun grid-at (grid i j)
  (aref grid i j))

(defun pick-delta (board coord)
  (let ((result
    (remove-if-not
      (lambda (x)
        (if (equal (car x) coord) t))
      (board-deltas board))))
    (if result (cadar result) nil)))
  

;; Returns item at (i,j) on DVONN board.
;; Items off grid may be read, returned as GRID-NOTILE.
(defun piece-at (board coord)
  (let ((delta (pick-delta board coord)))
    (cond
      ((or
        (>= (second coord) (board-height board))
        (>= (first coord) (board-width board))
        (< (first coord) 0)
        (< (second coord) 0)
        (null (board-grid board))) GRID-NOTILE)
      ((not (null delta)) delta)
      (t
        (grid-at (board-grid board) (first coord) (second coord))))))

(defun grid-set (grid coord value)
  (setf (aref grid (first coord) (second coord)) value))

;; Pretty-prints a dvonn board.
(defun pretty-print-board (board &optional (nums nil))
  (defun printcell (thing)
    (format t "~5A" thing))
  (defun printis (i)
    (cond
      ((>= i (board-width board)) nil)
      ((= i 0) (format t "~%   ") (printcell i) (printis (+ i 1)))
      (t (printcell i) (printis (+ i 1)))))
  (if nums (printis 0) nil)
  (mapboard
    (lambda (piece i j)
      (cond
        ((and (= i 0) (oddp j))
          (if (not nums)
            (format t "~%   ")
            (format t "~%~A    " j)) 
          (printcell (dvonn-stack-tostring piece)))
        ((= i 0)
          (if (not nums)
            (format t "~%")
            (format t "~%~A  " j))
          (printcell (dvonn-stack-tostring piece)))
        (t (printcell (dvonn-stack-tostring piece)))))
    board)
  (format t "~%")
  nil)

;; Creates a random list of pieces.
(defun create-pieces (b w r)
  (cond
    ((= 0 (+ b w r)) nil)
    (t
      (let ((n (random (+ b w r))))
        (cond
          ((< n b) (cons (dvonn-stack COLOR-BLACK) (create-pieces (- b 1) w r)))
          ((< n (+ b w)) (cons (dvonn-stack COLOR-WHITE)
            (create-pieces b (- w 1) r)))
          (t (cons (dvonn-stack COLOR-RED) (create-pieces b w (- r 1)))))))))

;; Find and replace item in seq with items, returning a list of the
;; resulting sequence and unused items.
(defun find-and-replace (seq item items)
  (cond
    ((or (null seq) (null items)) (list seq items))
    ((eql (car seq) item)
      (let ((result (find-and-replace (cdr seq) item (cdr items))))
        (list
          (cons (car items) (car result))
          (cadr result))))
    (t
      (let ((result (find-and-replace (cdr seq) item items)))
        (list
          (cons (car seq) (car result))
          (cadr result))))))

;; Creates a randomly-arranged dvonn board using an heuristic to guess the
;; shape of the board (for non-standard width/height) and the number of pieces.
(defun create-random-board (width height)
  (defun add-pieces (grid coords pieces)
    (cond
      ((or (null coords) (null pieces)) grid)
      (t
        (grid-set grid (car coords) (car pieces))
        (add-pieces grid (cdr coords) (cdr pieces)))))

  (let ((bwpieces (/ (- (* width height) 7 (/ (- height 1) 2)) 2)))
    (let ((board (create-empty-board width height)))
      (dvonn-board width height
        (add-pieces (board-grid board)
          (filterboard (lambda (p i j) (equal p GRID-EMPTY)) board t)
          (create-pieces bwpieces bwpieces 3))))))

;; Creates an empty DVONN grid.
(defun create-empty-board (width height)
  (mapboard
    (lambda (piece i j)
      (cond
        ((and (or (= i 0) (= i (- width 1))) (or (= j (- height 1)) (= j 0)))
          GRID-NOTILE)
        ((and (= i (- width 1)) (oddp j)) GRID-NOTILE)
        (t GRID-EMPTY)))
    (dvonn-board width height nil)))

;; Returns true if the grid at coord is surrounded by other pieces.
(defun surrounded (board coord)
  (let ((pieces
        (mapcar (lambda (x) (piece-at board x))
          (mapcar (lambda (x) (move-one coord x)) all-directions))))
    (not (or (find GRID-NOTILE pieces) (find GRID-EMPTY pieces)))))

;; 
(defun deep-find (needle haystack)
  (find-if (lambda (x) (equal x needle)) haystack))

;; Gets the positions which are connected to a control piece.
(defun connected-components (board)
  (defun qsearch (coords visited)
    (cond
      ((null coords) visited)
      ((deep-find (car coords) visited)
        (qsearch (cdr coords) visited))
      ((not (is-stack (piece-at board (car coords))))
        (qsearch (cdr coords) (cons (car coords) visited)))
      (t
        (qsearch
          (cons (move-one (car coords) NE)
          (cons (move-one (car coords) NW)
          (cons (move-one (car coords) SE)
          (cons (move-one (car coords) SW)
          (cons (move-one (car coords) W)
          (cons (move-one (car coords) E)
            coords))))))
          (cons (car coords) visited)))))
  (qsearch
    (filterboard
      (lambda (piece i j)
        (is-type piece TYPE-CONTROL))
      board
      t)
    nil))

(defun score-color (board color)
  (let ((cs (filterboard (lambda (p i j) (is-color p color)) board)))
    (cond
      ((null cs) 0)
      ((null (cdr cs)) (stack-height (car cs)))
      (t
        (reduce
          (lambda (x y)
            (if (is-stack x)
              (+ (stack-height x) (stack-height y))
              (+ x (stack-height y))))
          cs)))))

(defun prune-stacks (board)
  (let ((coords (connected-components board)))
    (mapboard
      (lambda (piece i j)
        (if (deep-find (list i j) coords) piece
          (if (eql piece GRID-NOTILE) GRID-NOTILE GRID-EMPTY)))
      board)))

(defun filterboard (func board &optional (coord nil))
  (defun filter-helper (i j)
    (cond
      ((>= i (board-width board)) (filter-helper 0 (+ j 1)))
      ((>= j (board-height board)) nil)
      (t  (let ((piece (piece-at board (list i j))))
          (if (funcall func piece i j)
            (cons
              (if coord (list i j) piece)
              (filter-helper (+ i 1) j))
            (filter-helper (+ i 1) j))))))
  (filter-helper 0 0)
)

(defun mapboard (func board)
  (defun map-helper (i j arr)
    (cond
      ((or (= j (board-height board)) (= i (board-width board))) arr)
      ((= i 0)
        (setf (aref arr i j)
          (funcall func (piece-at board (list i j)) i j))
        (map-helper (+ i 1) j arr)
        (map-helper i (+ j 1) arr))
      ((and (< i (board-width board)) (< j (board-height board)))
        (setf (aref arr i j)
          (funcall func (piece-at board (list i j)) i j))
        (map-helper (+ i 1) j arr))
      (t nil)))
  (list (board-width board) (board-height board)
    (map-helper 0 0 (make-array (list (board-width board) (board-height board))))))

(defun apply-move (board move)
  (let ((from (first move)) (dir (second move)))
    (let ((to (move-piece board from dir)))
      (prune-stacks
        (dvonn-board
          (board-width board)
          (board-height board)
          (board-grid board)
          (cons
            (list from GRID-EMPTY)
            (cons
              (list to
                (stack-pieces
                  (piece-at board from)
                  (piece-at board to)))
              (board-deltas board))))))))

(defun legal-move (board move &optional (color nil))
  (let ((coord (first move)) (dir (second move)))
    (let ((piece (piece-at board coord)))
      (cond
        ((not (is-stack piece)) nil)
        (t
          (let ((dest (move-piece board coord dir)))
            (let ((dest-stack (piece-at board dest)))
              (and
                (is-stack piece)
                (or (not color) (eql (stack-color piece) color))
                (not (eql dest-stack GRID-EMPTY))
                (not (eql dest-stack GRID-NOTILE))))))))))

(defun board-possible-moves (board color)
  (defun all-moves (coords dirs)
    (cond
      ((null coords) nil)
      ((null dirs) (all-moves (cdr coords) all-directions))
      (t (cons (list (car coords) (car dirs))
          (all-moves coords (cdr dirs))))))

  (remove-if-not (lambda (x) (legal-move board x))
    (all-moves (board-movable-pieces board color) all-directions)))

(defun board-movable-pieces (board color)
  (filterboard
    (lambda (x i j)
      (can-move board color (list i j)))
    board t))

(defun can-move (board color coord)
  (let ((piece (piece-at board coord)))
    (not (or
        (eql piece GRID-EMPTY)
        (eql piece GRID-NOTILE)
        (not (eql (stack-color piece) color))
        (surrounded board coord)))))

;; Returns which delta (0 or 1) we should use for one of the directions.
(defun which-delta (coord)
  (let ((x (nth 0 coord)) (y (nth 1 coord)))
  (cond
    ((or (and (evenp x) (evenp y)) (and (oddp x) (evenp y))) 0)
    (t 1))))

;; Adds lists components as if they were vectors.
(defun vector-add (v1 v2)
  (cond
    ((or (null v1) (null v2)) nil)
    (t (cons (+ (car v1) (car v2))
      (vector-add (cdr v1) (cdr v2))))))

;; Friendly wrapper for translating move into cooresponding coordinate.
(defun move-move (board move)
  (move-piece board (first move) (second move)))

;; Moves piece located at coordinate in given direction.
(defun move-piece (board coord dir)
  (let ((piece (piece-at board coord)))
    (move-n coord (stack-height piece) dir)))

;; Moves coordinate one time in given direction.
(defun move-one (coord dir)
  (vector-add coord (nth (which-delta coord) dir))
)

;; Moves coordinate n times in given direction.
(defun move-n (coord n dir)
  (cond
    ((> n 0) (move-n (move-one coord dir) (- n 1) dir))
    (t coord)))
