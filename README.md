# Ziglisp

A yet another Lisp interpreter written in Zig.

## Usage

* `zig run src/main.zig` to start REPL.
* `zig run src/main.zig -- src/examples/mergesort.lisp` to evaluate file.

Tested under Zig version `0.12.0-dev.278+0e8f130ae`.

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


### Value types

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

Yes, some of special forms do not have to be special forms, they can be achieved by macros.
However, this interpreter does not yet have the macro feature and it is a future work.

## License

[MIT](https://choosealicense.com/licenses/mit/)