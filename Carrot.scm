#!/usr/local/bin/gosh

;;;; Carrot ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here

(add-load-path "./lib/" :relative)

(use Util)
(use K-Compiler)
(use Krivine)
(use Check)
(use Read)
(use gauche.parseopt)

;;; REPL ;;;
(define (REPL exprs*types ctr)
  (format #t "carrot ~S> " ctr)
  (flush)
  (let* ([expr  (read)]
         [res (read-s-exprs (list expr))]
         [_ (hash-table-union! (car exprs*types) (car res))]
         [_ (hash-table-union! (cdr exprs*types) (cdr res))]
         [exprs-ht (car exprs*types)]
         [checks?  (type-check exprs*types)])
    (unless checks?
            (print "Skipping execution due to one or more type errors _(′︿‵｡_)")
            (REPL exprs*types (+ ctr 1)))
    (print (Krivine (compile exprs-ht)))
    (REPL exprs*types (+ ctr 1))))  ;loop with new global-environment

(define banner
"             ----------------------
             |    CARROT 2.1.2    |
             ----------------------
         https://github.com/ympbyc/Carrot\n")

(define (main args)
  (print banner)
  (format #t "Loading ~S ... done\n" (cdr args))
  (load "standard-macros.scm")
  (let* ([fnames (cons "examples/prelude.nadeko" (cdr args))]
         [exprs*types (fold (fn [fname exprs*types]
                                (let1 res (load-file fname)
                                      (hash-table-union! (car exprs*types) (car res))
                                      (hash-table-union! (cdr exprs*types) (cdr res))
                                      exprs*types))
                            (cons (make-hash-table 'eq?) (make-hash-table 'eq?))
                            fnames)])
    (print (sort (map symbol->string (hash-table-keys (car exprs*types)))))
    (REPL exprs*types 0)))

;;string * {types} -> ({types} . typed-expr)
(define (load-file fname)
  (call-with-input-file fname
    (fn [file-port]
        (let* ([exprs*types (read-s-exprs (read-list file-port))]
               [checks? (type-check exprs*types)])
          exprs*types))))

(define (read-list port)
  (let ((exp (read port)))
    (if (eof-object? exp) '()
      (cons exp (read-list port)))))
