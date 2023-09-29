# Ziglisp

A yet another Lisp interpreter written in Zig.

Disclaimer: the author make it for his own learning Zig purposes, so the quality is just that.

## Usage

* `zig run src/main.zig` to start REPL.
* `zig run src/main.zig -- src/examples/mergesort.scm` to evaluate file.

```
$ zig run src/main.zig
>>> (print '(hello world))
#print: (hello world)
(hello world)
>>> 
```

```
$ cat src/examples/y-comb.scm 
(let ((f (lambda (fib)
           (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (1 (+ (fib (- n 1)) (fib (- n 2))))))))
      (y (lambda (f)
           ((lambda (x) (f (lambda (m) ((x x) m))))
            (lambda (x) (f (lambda (m) ((x x) m))))))))
  ((y f) 10)) ; The 10th fibonacci number
$ zig run src/main.zig -- src/examples/y-comb.scm 
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

Basically respecting [R5RS Scheme](https://docs.racket-lang.org/r5rs/r5rs-std/).

* Functions
    * `-`
    * `*`
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
    * `modulo`
    * `null?`
    * `or`
    * `print`
    * `quotient`
* Special forms:
    * `begin`
    * `cond`
    * `define`
    * `if`
    * `let`
    * `quote`

## TODOs

- [x] Lambda.
- [ ] Macro syntax.
- [ ] Represent environment by tree structure.
- [ ] GC.

## License

[MIT](https://choosealicense.com/licenses/mit/)
