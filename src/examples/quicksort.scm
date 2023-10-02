(define (quick-sort lst)
  (if (null? lst) lst
    (let ((pivot (car lst)))
              (let ((left (filter (lambda (x) (< x pivot)) (cdr lst)))
                    (right (filter (lambda (x) (<= pivot x)) (cdr lst))))
                (append (quick-sort left) (cons pivot (quick-sort right)))))))

(quick-sort '(3 1 4 1 5 9 2 6 5 3 5 8 9 7 9))
