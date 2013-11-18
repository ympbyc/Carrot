(define (consify xs) (if (null? xs) 'nil `(cons ,(car xs) ,(consify (cdr xs)))))
(define-macro (lizt . xs) (consify xs))


(define (splat f n xs)
  (if (null? xs)
      '()
      (cons (f (take xs n))
            (splat f n (drop xs n)))))

(define-macro (bind binding expr)
  (let ([syms (splat car 2 binding)]
        [vals (splat cadr 2 binding)])
    `((^ ,@syms ,expr) ,@vals)))
