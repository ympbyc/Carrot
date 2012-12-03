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
  (:= (car lst) (lst (-> (h t) h)))
  (:= (cdr lst) (lst (-> (h t) t)))
  (:= (map f lst)
    (if (eq? lst 'nil)
      'nil
      (cons (f (car lst)) (map (cdr lst) f))))
  (:= (fold f init lst)
    (if (eq? lst 'nil)
      init
      (fold f (f (car lst) init) (cdr lst))))
  (:= (append lst1 lst2)
    (if (eq? lst1 'nil) 
      lst2
      (cons (car lst1)
            (append (cdr lst1) lst2))))
  (:= (reverse lst1 lst2)
    (if (eq? lst1 'nil) 
      lst2
      (reverse (cdr lst1) (cons (car lst1) lst2))))
  (:= (+ a b) (** + a b))
  (:= (- a b) (** - a b))
  (:= (* a b) (** * a b))
  (:= (/ a b) (** / a b))
)) '() '())
  (set! *prelude* g-env))