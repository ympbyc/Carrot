#!/usr/local/bin/gosh

;;;; Nadeko ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here

(add-load-path "./lib/" :relative)

(use Util)
(use K-Compiler)
(use Krivine)
(use gauche.parseopt)

;;; REPL ;;;
(define (REPL g-env)
  (display "nadeko> ")
  (flush)
  (let* ([new-env (compile `(,(read)))]
         [new-env (hash-table-union! g-env new-env)]
         [result  (Krivine new-env)])
    (print result)
    (REPL new-env)))  ;loop with new global-environment

(define (main args)
  (let-args (cdr args)
    ((load-fname "l|load=s"))
    (print "Nadeko, version 2.1.0: https://github.com/ympbyc/Nadeko ^C to exit")
    (load "standard-macros.scm")
    (let ([prelude-g (pre-load "examples/prelude.nadeko" (make-hash-table 'eq?))])
      (REPL
       (if load-fname (pre-load load-fname prelude-g) prelude-g)))))

(define (pre-load fname g-e)
  (call-with-input-file fname
    (lambda (file-port)
      (hash-table-union! g-e (compile (read-list file-port))))))

(define (read-list port)
  (let ((exp (read port)))
    (if (eof-object? exp) '()
      (cons exp (read-list port)))))
