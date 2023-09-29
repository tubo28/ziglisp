(let ((f (lambda (fb)
           (lambda (n)
             (cond ((= n 1) '())
                   (#t (fb (- n 1))))
             (cond ((= (modulo n 15) 0) (print 'fizzbazz))
                   ((= (modulo n 3) 0)  (print 'fizz))
                   ((= (modulo n 5) 0)  (print 'bazz))
                   (#t (print n))))))

      (y (lambda (f)
           ((lambda (x) (f (lambda (m) ((x x) m))))
            (lambda (x) (f (lambda (m) ((x x) m))))))))

  ((y f) 20))
