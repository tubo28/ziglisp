; https://en.wikipedia.org/wiki/Tak_(function)
(defun tarai (x y z)
  (if (<= x y) y
    (tarai (tarai (- x 1) y z) (tarai (- y 1) z x) (tarai (- z 1) x y))))

(tarai 8 4 0)