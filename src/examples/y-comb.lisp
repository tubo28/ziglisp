(let ((f (lambda (fib)
           (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (1 (+ (fib (- n 1)) (fib (- n 2))))))))
      (y (lambda (f)
           ((lambda (x) (f (lambda (m) ((x x) m))))
            (lambda (x) (f (lambda (m) ((x x) m))))))))
  ((y f) 10))
