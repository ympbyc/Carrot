(define-module nadeko-primitive
  (export PRIMITIVE)
  (extend Krivine)

  (define (PRIMITIVE args code env stack c-stack)
    (print (car args))
    (print c-stack)
    (newline)
    (let ([subr (car args)]
          [true  `((,ACCESS true)  (,CONTINUE))]
          [false `((,ACCESS false)  (,CONTINUE))])

      (cond
        [(null? c-stack)
         (print "ぬるぬる1")
         (if (find (lambda (x) (eq? x subr)) '(string? number? num->str))
          (Krivine- false env (cdr stack) '())
          (Krivine- false env (cddr stack) '()))]

        [(eq? subr 'string?)
          (Krivine- (if (string? (car c-stack)) true false)
           env stack (cdr c-stack))]
        [(eq? subr 'number?)
          (Krivine- (if (number? (car c-stack)) true false)
           env stack (cdr c-stack))]
        [(eq? subr 'num->str)
          (Krivine- code env stack
            (cons (number->string (car c-stack)) (cdr c-stack)))]

        [(null? (cdr c-stack))
          (print "ぬるぬる2")
          (Krivine- false env (cdr stack) '())]

        [(eq? subr '+)
          (Krivine- code env stack
            (cons (+ (cadr c-stack) (car c-stack)) (cddr c-stack)))]
        [(eq? subr '-)
          (Krivine- code env stack
            (cons (- (cadr c-stack) (car c-stack)) (cddr c-stack)))]
        [(eq? subr '*)
          (Krivine- code env stack
            (cons (* (cadr c-stack) (car c-stack)) (cddr c-stack)))]
        [(eq? subr '/)
          (Krivine- code env stack
            (cons (/ (cadr c-stack) (car c-stack)) (cddr c-stack)))]
        [(eq? subr '%)
          (Krivine- code env stack
            (cons (% (cadr c-stack) (car c-stack)) (cddr c-stack)))]
        [(eq? subr '++)
          (Krivine- code env stack
            (cons (string-append (cadr c-stack) (car c-stack)) (cddr c-stack)))]

        [(eq? subr '=)
          (Krivine- (if (= (cadr c-stack) (car c-stack)) true false)
           env stack (cddr c-stack))]
        [(eq? subr 'equal)
          (Krivine- (if (equal? (cadr c-stack) (car c-stack)) true false)
           env stack (cddr c-stack))]
        [(eq? subr '<)
          (Krivine- (if (< (cadr c-stack) (car c-stack)) true false)
           env stack (cddr c-stack))]
        [(eq? subr '<=)
          (Krivine- (if (<= (cadr c-stack) (car c-stack)) true false)
           env stack (cddr c-stack))]
        [(eq? subr 'string?)
          (Krivine- (if (string? (car c-stack)) true false)
           env stack (cdr c-stack))]
        [(eq? subr 'number?)
          (Krivine- (if (number? (car c-stack)) true false)
           env stack (cdr c-stack))]

        [(eq? subr 'num->str)
          (Krivine- code env stack
            (cons (number->string (car c-stack)) (cdr c-stack)))]))))
