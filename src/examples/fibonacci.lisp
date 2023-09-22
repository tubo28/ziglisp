(progn
  (defun fibonacci (n)
    (if (or (eq n 0) (eq n 1)) 1
        (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
  (fibonacci 10))
