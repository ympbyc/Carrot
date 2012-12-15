(define-module nadeko-primitive
  (export PRIMITIVE)
  (extend Krivine)

  (define (PRIMITIVE args code env stack c-stack)
    (let ([subr (car args)]
          [true  `((,ACCESS true)  (,CONTINUE))]
          [false `((,ACCESS false)  (,CONTINUE))])
      (cond
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
           env stack c-stack)]
        [(eq? subr 'equal)
          (Krivine- (if (equal? (cadr c-stack) (car c-stack)) true false)
           env stack c-stack)]
        [(eq? subr '<)
          (Krivine- (if (< (cadr c-stack) (car c-stack)) true false)
           env stack c-stack)]
        [(eq? subr '<=)
          (Krivine- (if (<= (cadr c-stack) (car c-stack)) true false)
           env stack c-stack)]
        [(eq? subr 'string?)
          (Krivine- (if (string? (car c-stack)) true false)
           env stack c-stack)]
        [(eq? subr 'number?)
          (Krivine- (if (number? (car c-stack)) true false)
           env stack c-stack)]

        [(eq? subr 'num->str)
          (Krivine- code env stack
            (cons (number->string (car c-stack)) (cddr c-stack)))]))))