(define (merge f s)
  (cond
    ((= (length f) 0) s)
    ((= (length s) 0) f)
    ((< (car f) (car s)) (append (list (car f)) (merge (cdr f) s)))
    ((> (car f) (car s)) (append (list (car s)) (merge f (cdr s))))
    ((= (car f) (car s)) (append (list (car f) (car s)) (merge (cdr f) (cdr s))))))

(define (merge-sort lst)
  (let ((len (length lst)))
    (cond
      ((= len 1) lst)
      (#t
      (merge (merge-sort (subsequence lst 0 (/ len 2)))
              (merge-sort (subsequence lst (/ len 2) len)))))))

(merge-sort '(3 1 4 1 5 9 2 6 5 3 5 8 9 7 9))
