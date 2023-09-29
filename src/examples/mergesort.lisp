(define (append list1 list2)
  (cond
    ((null? list1) list2)
    (#t (cons (car list1) (append (cdr list1) list2)))))

(define (subseq lst begin end)
  (cond
    ((or (null? lst) (<= end 0)) '())
    ((> begin 0) (subseq (cdr lst) (- begin 1) (- end 1)))
    (#t (cons (car lst) (subseq (cdr lst) 0 (- end 1))))))

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
      (merge (merge-sort (subseq lst 0 (/ len 2)))
              (merge-sort (subseq lst (/ len 2) len)))))))

(merge-sort '(3 1 4 1 5 9 2 6 5 3 5 8 9 7 9))
