; https://en.wikipedia.org/wiki/Tak_(function)
(define (tarai x y z)
  (if (< y x) (tarai (tarai (- x 1) y z) (tarai (- y 1) z x) (tarai (- z 1) x y))
  y))

(print (tarai 12 6 0))
