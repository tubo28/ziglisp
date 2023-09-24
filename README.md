# Ziglisp

A yet another Lisp interpreter written in Zig.

Disclaimer: the author create the interpreter for his own learning Zig purposes, so the quality is just that.

## Usage

* `zig run src/main.zig` to start REPL.
* `zig run src/main.zig -- src/examples/mergesort.lisp` to evaluate file.

Tested under Zig version 0.12.0-dev.494+a8d2ed806.

```
$ zig run src/main.zig
>>> (setq a 1)
1
>>> (+ a a)
2
>>> (print '(hello world))
#print: (hello . (world . nil))
(hello . (world . nil))
```

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