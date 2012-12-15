#!/usr/local/bin/gosh

;;;; Nadeko ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here

(add-load-path "./lib/" :relative)

(use K-Compiler)
(use gauche.parseopt)

;;; REPL ;;;
(define (REPL g-env)
  (display "nadeko> ")
  (flush)
  (receive (result bindings) (Krivine (compile `(,(read))) g-env)
    (print result)
    (REPL bindings))) ;loop with new global-environment

(define (main args)
  (let-args (cdr args)
    ((load-fname "l|load=s"))
   (print "Nadeko, version 1.0.0: https://github.com/ympbyc/Nadeko ^C to exit")
   (let ([prelude-g (pre-load "examples/prelude.nadeko" '())])
   (REPL 
    (if load-fname (pre-load load-fname prelude-g) prelude-g)))))

(define (pre-load fname g-e)
  (call-with-input-file fname (lambda (file-port)
    (receive (result g-env) 
             (Krivine (compile (read-list file-port)) g-e)
      g-env))))

(define (read-list port)
  (let ((exp (read port)))
    (if (eof-object? exp) '()
      (cons exp (read-list port)))))
