(load "dvonn.lisp")

(defun compute-color (piece)
  (cond
    ((not (is-stack piece)) "")
    ((eql (stack-color piece) COLOR-RED) "Red")
    ((eql (stack-color piece) COLOR-BLACK) "Black")
    ((eql (stack-color piece) COLOR-WHITE) "White")
    (t "Unknown")))

(defun compute-class (piece)
  (cond
    ((eql piece GRID-EMPTY) "Empty Tile")
    ((not (is-stack piece)) "")
    ((eql (stack-type piece) TYPE-NORMAL)
      (format nil "~A Normal Tile" (compute-color piece)))
    ((eql (stack-type piece )TYPE-CONTROL)
      (format nil "~A Control Tile" (compute-color piece)))
    (t "Unknown")))

(defun compute-height (piece)
  (cond
    ((is-stack piece)
      (format nil "~%    <text>~A</text>" (stack-height piece)))
    (t "")))

(defun compute-innerring (piece)
  (cond
    ((not (is-stack piece)) "")
    ((eql (stack-type piece) TYPE-CONTROL)
      "<circle class='Control' r='22' />")
    (t "")))

(defun board-to-svg (board filename)
  (with-open-file (sout filename :direction :output :if-exists :supersede)
    (format sout
"<?xml version='1.0' standalone='no'?>
<?xml-stylesheet type='text/css' href='dvonn-style.css' ?>
<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.0//EN' 'http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd'>
<svg width='880' height='360' viewBox='0 0 880 360' xmlns:svg='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink' xmlns='http://www.w3.org/2000/svg'>
<style type='text/css'>
.Tile circle.Control {
  stroke: red;
  stroke-dasharray: 7,7;
}

.Tile {
  fill: none;
}

.Tile path {
  stroke: black;
  stroke-width: 4;
}
.Tile circle {
  stroke-width: 8;
}
.Red circle {
  stroke: red;
}
.Black circle {
  stroke: black;
}
.Empty circle {
  stroke: none;
}
.White circle {
  stroke: #dddddd;
}

.Tile text {
  fill: #000000;
  text-anchor: middle;
}
</style>
<title>DVONN game board</title>
<g transform='translate(50,0) scale(1.02,1.02)'>")
    (defun write-tile (x y color stack)
      (format sout
"  <g transform='translate(~A,~A)' class='~A'>~A
    <circle class='Player' r='22' />~A
    <path d='M34.64101615,-20 V20 L0,40 L-34.6410161,20 V-20 L0,-40Z'/>
  </g>~%"
        x y
        (compute-class stack)
        (compute-height stack)
        (compute-innerring stack)))
    (mapboard
      (lambda (piece i j)
        (let
          (
            (x
              (if
                (evenp j)
                (+  0 (* 72 i))
                (+ 36 (* 72 i))))
            (y (* 60 (+ j 1))))
          (cond
            ((eql piece GRID-NOTILE) nil)
            ((eql piece GRID-EMPTY)
              (write-tile x y "Empty" piece))
            ((eql (stack-color piece) COLOR-RED)
              (write-tile x y "Red" piece))
            ((eql (stack-color piece) COLOR-WHITE)
              (write-tile x y "White" piece))
            ((eql (stack-color piece) COLOR-BLACK)
              (write-tile x y "Black" piece)))))
      board)
    (format sout "</g></svg>")))
