# Ziglisp

A yet another Lisp interpreter written in Zig.

Disclaimer: the author make it for his own learning Zig purposes, so the quality is just that.

## Usage

* `zig run src/main.zig` to start REPL.
* `zig run src/main.zig -- src/examples/mergesort.lisp` to evaluate file.

```
$ zig run src/main.zig
>>> (setq a 1)
1
>>> (+ a a)
2
>>> (print '(hello world))
#print: (hello world)
(hello world)
>>> 
```

```
$ cat src/examples/mergesort.lisp
(defun append (list1 list2)
  (cond
    ((null list1) list2)
    (t (cons (car list1) (append (cdr list1) list2)))))

(defun subseq (lst begin end)
  (cond
    ((or (null lst) (<= end 0)) '())
    ((> begin 0) (subseq (cdr lst) (- begin 1) (- end 1)))
    (t (cons (car lst) (subseq (cdr lst) 0 (- end 1))))))

(defun merge (f s)
  (cond
    ((= (length f) 0) s)
    ((= (length s) 0) f)
    ((< (car f) (car s)) (append (list (car f)) (merge (cdr f) s)))
    ((> (car f) (car s)) (append (list (car s)) (merge f (cdr s))))
    ((= (car f) (car s)) (append (list (car f) (car s)) (merge (cdr f) (cdr s))))))

(defun merge-sort(lst)
  (let ((len (length lst)))
    (cond
      ((= len 1) lst)
      (t
      (merge (merge-sort (subseq lst 0 (/ len 2)))
              (merge-sort (subseq lst (/ len 2) len)))))))

(merge-sort '(3 1 4 1 5 9 2 6 5 3 5 8 9 7 9))

$ zig run src/main.zig -- src/examples/mergesort.lisp
(1 1 2 3 3 4 5 5 5 6 7 8 9 9 9)
```

Developed under Zig version 0.12.0-dev.494+a8d2ed806.

## Features


### Data types

* Cons cell
* Function
* Number (`i64`)
* Symbol (`[]const u8`)

### Built-in functions and special forms

* Functions
    * `-`
    * `*`
    * `/`
    * `+`
    * `<`
    * `<=`
    * `=`
    * `>`
    * `>=`
    * `and`
    * `car`
    * `cdr`
    * `cons`
    * `length`
    * `list`
    * `null`
    * `or`
    * `print`
* Special forms:
    * `cond`
    * `defun`
    * `if`
    * `let`
    * `progn`
    * `quote`
    * `setq`

## License

[MIT](https://choosealicense.com/licenses/mit/)
