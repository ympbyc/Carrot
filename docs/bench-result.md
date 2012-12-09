Bench Result
============

```
time echo '(Y (-> (f) (cons 1 (zipWith f + (cdr f)) -cons 1)) -take 10 -reverse -fold (compose (compose ++ (++ ",")) show) "")' | gosh Nadeko.scm -l=examples/srfi-1.nadeko
```

With SECD VM
------------

gosh Nadeko.scm -l=examples/srfi-1.nadeko  4.53s user 0.02s system 99% cpu 4.548 total


With Krivine's Machine
----------------------

gosh Nadeko.scm -l=examples/srfi-1.nadeko  2.38s user 0.01s system 99% cpu 2.396 total