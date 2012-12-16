Bench Result
============

## Fibonacci List

```
time echo '(Y (-> (f) (cons 1 (zipWith f + (cdr f)) -cons 1)) -take 10 -reverse -fold (compose (compose ++ (++ ",")) show) "")' | gosh Nadeko.scm -l=examples/srfi-1.nadeko
```

### With SECD VM
gosh Nadeko.scm -l=examples/srfi-1.nadeko  4.53s user 0.02s system 99% cpu 4.548 total


### With Krivine's Machine
gosh Nadeko.scm -l=examples/srfi-1.nadeko  2.38s user 0.01s system 99% cpu 2.396 total

### With Krivine's Machine Extended with proper support for primitives
gosh Nadeko.scm -l=examples/srfi-1.nadeko  1.76s user 0.01s system 99% cpu 1.773 total

## Tarai

### Nadeko

code

```lisp
(:= (tarai x y z)
  (<= x y y
    (tarai (tarai (- x 1) y z)
           (tarai (- y 1) z x)
           (tarai (- z 1) x y))))
(** time (tarai 18 9 0))
```

result

```lisp
;(time (get-constant (car p-args)))
; real   0.062
; user   0.060
; sys    0.010
```