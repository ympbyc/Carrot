(define (consify xs) (if (null? xs) 'nil `(cons ,(car xs) ,(consify (cdr xs)))))
(define-macro (lizt . xs) (consify xs))
