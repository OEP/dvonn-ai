(load "dvonn.lisp")

(defun do-test (tests)
  (defun do-subtest (n coord dirs)
    (cond
      ((null dirs) 'Done)
      (t (let ((dir (car dirs)))
        (print (move-n coord n dir))
        (do-subtest n coord (cdr dirs))))))

  (cond
    ((null tests) 'Done)
    (t
      (format t "~%~%")
      (format t "Points around ~A" (cadar tests))
      (print '=================================)
      (do-subtest (caar tests) (cadar tests) all-directions)
      (do-test (cdr tests))
    )
  )
)

(do-test '(
  (1 (2 2))
  (1 (1 2))
  (1 (2 1))
  (1 (1 1))
  (2 (2 2))
))
