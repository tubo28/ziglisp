(let ((f (lambda (fib)
           (lambda (n)
             (cond ((< n 2) '(1 1))
                   (#t (let ((tail (fib (- n 1))))
                         (append (list (+ (car tail) 
                                          (car (cdr tail)))) tail)))))))

      (y (lambda (f)
           ((lambda (x) (f (lambda (m) ((x x) m))))
            (lambda (x) (f (lambda (m) ((x x) m))))))))

  (print (reverse ((y f) 20))))
