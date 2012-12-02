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
  (REPL *prelude*))

(define *prelude* '())

(receive (result g-env) (SECD '() '() (compile '(
  (:= (true  t e) t)
  (:= (false t e) e)
  (:= (if bool then else) (bool then else))
  (:= (eq? a b) (** equal a b))
  (:= (cons head tail f) (f head tail))
  (:= (car head tail) head)
  (:= (cdr head tail) tail)
  (:= (map h t f)
    (if (eq? t 'nil)
      (cons (f h) 'nil)
      (cons (f h) (map (t car) (t cdr) f))))
  (:= (fold h t f init)
    (if (eq? t 'nil)
      (f h init)
      (t fold f (f h init))))
  (:= (+ a b) (** + a b))
  (:= (- a b) (** - a b))
  (:= (* a b) (** * a b))
  (:= (/ a b) (** / a b))
)) '() '())
  (set! *prelude* g-env))