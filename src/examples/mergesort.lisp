(progn
  (defun merge-sort(lst)
    (defun merge_(f s)
      (cond
       ((= (length f) 0) s)
       ((= (length s) 0) f)
       ((< (car f) (car s)) (append (list (car f)) (merge_ (cdr f) s)))
       ((> (car f) (car s)) (append (list (car s)) (merge_ f (cdr s))))
       ((= (car f) (car s)) (append (list (car f) (car s)) (merge_ (cdr f) (cdr s))))))
    (let ((len (length lst)))
      (cond
       ((= len 1) lst)
       (t
        (merge_ (merge-sort (seq-subseq lst 0 (ceiling (/ len 2))))
                (merge-sort (seq-subseq lst (ceiling (/ len 2)))))))))
  (merge-sort '(3 1 4 1 5 9 2 6 5)))
