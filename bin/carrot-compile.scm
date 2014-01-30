#!/usr/local/bin/gosh

(add-load-path "../lib/" :relative)
(add-load-path "../compilers/" :relative)
(add-load-path "/usr/local/share/Carrot/2.2.0/lib/" :absolute)
(add-load-path "/usr/local/share/Carrot/2.2.0/compilers/" :absolute)

(use Util)
(use DataTypes)

(define (main args)
    (let* ([compiler-name (cadr args)]
           [exprs*t (read)])
      (format (standard-error-port)
              "Compiling     your carrot ~A...\n" compiler-name)
      (load (str compiler-name ".scm"))
      (let1 res (eval `(compile ,(fst exprs*t))
                      (find-module (string->symbol compiler-name)))
            (cond [(hash-table? res)
                   (display (write-hash-table res))]
                  [string? res (display res)]))))
