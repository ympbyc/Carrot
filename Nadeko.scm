;;;; Nadeko ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here

(load "./Compiler.scm")

;;; REPL ;;;
(define (REPL g-env)
  (display "nadeko> ")
  (flush)
  (receive (result bindings) (SECD '() '() (compile `(,(read))) '() g-env)
    (print result)
    (REPL bindings))) ;loop with new global-environment

(define (main args)
  (print "Nadeko, version 1.0.0: https://github.com/ympbyc/Nadeko ^C to exit")
  ;(print (compile (read)))
  (REPL '()))
