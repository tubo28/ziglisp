# Ziglisp

A yet another Lisp interpreter written in Zig.

Disclaimer: the author make it for his own learning Zig purposes, so the quality is just that.

## Usage

* `zig run src/main.zig` to start REPL.
* `zig run src/main.zig -- src/examples/mergesort.lisp` to evaluate file.

```
$ zig run src/main.zig
>>> (print '(hello world))
#print: (hello world)
(hello world)
>>> 
```

```
$ cat src/examples/y-comb.lisp 
(let ((f (lambda (fib)
           (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (1 (+ (fib (- n 1)) (fib (- n 2))))))))
      (y (lambda (f)
           ((lambda (x) (f (lambda (m) ((x x) m))))
            (lambda (x) (f (lambda (m) ((x x) m))))))))
  ((y f) 10)) ; The 10th fibonacci number
$ zig run src/main.zig -- src/examples/y-comb.lisp 
55
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

## License

[MIT](https://choosealicense.com/licenses/mit/)
