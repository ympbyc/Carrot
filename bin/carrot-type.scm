#!/usr/local/bin/gosh

(add-load-path "../lib/" :relative)
(add-load-path "/usr/local/share/Carrot/2.2.0/lib/" :absolute)

(use DataTypes)
(use Type)

(define (main _)
  (let* ([exprs*types*genmap (read)]
         [_ (format (standard-error-port) "Type-checking your carrot...\n")]
         [exprs*t (acquire-checked-program exprs*types*genmap)])
    (display
     (pair (write-hash-table (fst exprs*t))
           (snd exprs*t)))))
