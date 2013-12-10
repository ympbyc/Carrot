#!/usr/local/bin/gosh

;;;; Carrot ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here

(add-load-path "./lib/" :relative)

(use Util)
(use K-Compiler)
(use Krivine)
(use Check)
(use gauche.parseopt)

;;; REPL ;;;
(define (REPL g-env ctr)
  (format #t "carrot ~S> " ctr)
  (flush)
  (REPL (hash-table-union! g-env (type-check (list (read)) g-env))
        (+ ctr 1))
  #|(let* ([expr    (read)]
         [new-env (compile (list expr))]
         [new-env (hash-table-union! g-env new-env)]
         [result  (Krivine new-env)])
    (print result)
    (REPL new-env))|#)  ;loop with new global-environment

(define banner
"             ----------------------
             |    CARROT 2.1.2    |
             ----------------------     https://github.com/ympbyc/Carrot\n")

(define (main args)
  (print banner)
  (format #t "Loading ~S ... done\n" (cdr args))
  (print "type `help` to get started")
  (load "standard-macros.scm")
  (REPL (make-hash-table 'eq?) 0)
  #|(let ([fnames (cons "examples/prelude.nadeko" (cdr args))])
    (REPL (fold (fn [fname binding] (hash-table-union! binding (pre-load fname)))
                (make-hash-table 'eq?)
                fnames)))|#)

(define (pre-load fname)
  (call-with-input-file fname
    (lambda (file-port)
      (compile (read-list file-port)))))

(define (read-list port)
  (let ((exp (read port)))
    (if (eof-object? exp) '()
      (cons exp (read-list port)))))
