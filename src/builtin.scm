(define (not a) (if a '#f '#t))
(define (<= a b) (not (< b a)))
(define (> a b) (< b a))
(define (>= a b) (<= b a))

(define (append list1 list2)
  (cond
    ((null? list1) list2)
    (#t (cons (car list1) (append (cdr list1) list2)))))

(define (reverse lst)
    (if (null? lst)
        lst
        (append (reverse (cdr lst)) (list (car lst)))))

(define (subsequence lst begin end)
  (cond
    ((or (null? lst) (<= end 0)) '())
    ((> begin 0) (subsequence (cdr lst) (- begin 1) (- end 1)))
    (#t (cons (car lst) (subsequence (cdr lst) 0 (- end 1))))))
