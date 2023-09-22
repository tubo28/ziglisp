(progn
  (defun append (list1 list2)
    (cond
      ((null list1) list2)
      (t (cons (car list1) (append (cdr list1) list2)))))
  (defun subseq (lst begin end)
    (cond
      ((or (null lst) (<= end 0)) '())
      ((> begin 0) (subseq (cdr lst) (- begin 1) (- end 1)))
      (t (cons (car lst) (subseq (cdr lst) 0 (- end 1))))))
  (defun merge_ (f s)
    (cond
      ((= (length f) 0) s)
      ((= (length s) 0) f)
      ((< (car f) (car s)) (append (list (car f)) (merge_ (cdr f) s)))
      ((> (car f) (car s)) (append (list (car s)) (merge_ f (cdr s))))
      ((= (car f) (car s)) (append (list (car f) (car s)) (merge_ (cdr f) (cdr s))))))
  (defun merge-sort(lst)
    (let ((len (length lst)))
      (cond
       ((= len 1) lst)
       (t
        (merge_ (merge-sort (subseq lst 0 (/ len 2)))
                (merge-sort (subseq lst (/ len 2) len)))))))
  (merge-sort '(3 1 4 1 5 9 2 6 5)))