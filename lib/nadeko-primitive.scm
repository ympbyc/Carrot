(define-module nadeko-primitive
  (export PRIMITIVE)
  (export PMARK)
  (extend Krivine)

  (define (mark? x)
    (eq? x 'MARK))

  (define (PMARK args code env stack c-stack)
    (Krivine- code env stack (cons 'MARK c-stack)))

  (define (PRIMITIVE args code env stack c-stack)
    ;(print (car args))
    ;(print c-stack)
    ;(newline)
    (let ([subr (car args)]
          [true  `((,ACCESS true)  (,CONTINUE))]
          [false `((,ACCESS false)  (,CONTINUE))])

      (cond
        ;zero arg
        [(eq? subr 'read)
          (Krivine- code env stack (cons (read) (cdr c-stack)))]
        [(eq? subr 'read-line)
          (Krivine- code env stack (cons (read-line) (cdr c-stack)))]

        ;one arg
        [(mark? (car c-stack))
         (if (find (lambda (x) (eq? x subr)) '(string? number? num->str))
           (Krivine- false env (cddr  stack) (cdr c-stack))
           (Krivine- false env (cddr stack) (cddr c-stack)))] ;insufficient args

        [(eq? subr 'string?)
          (Krivine- (if (string? (car c-stack)) true false)
           env stack (cddr c-stack))]
        [(eq? subr 'number?)
          (Krivine- (if (number? (car c-stack)) true false)
           env stack (cddr c-stack))]
        [(eq? subr 'num->str)
          (Krivine- code env stack
            (cons (number->string (car c-stack)) (cddr c-stack)))]
        [(eq? subr 'print)
          (print (car c-stack)) (flush)
          (Krivine- code env stack (cons #|(car c-stack)|# "" (cddr c-stack)))]

        ;two args
        [(mark? (cadr c-stack))
          (Krivine- false env (cddr stack) (cddr c-stack))]

        [(eq? subr '+)
          (Krivine- code env stack
            (cons (+ (cadr c-stack) (car c-stack)) (cdddr c-stack)))]
        [(eq? subr '-)
          (Krivine- code env stack
            (cons (- (cadr c-stack) (car c-stack)) (cdddr c-stack)))]
        [(eq? subr '*)
          (Krivine- code env stack
            (cons (* (cadr c-stack) (car c-stack)) (cdddr c-stack)))]
        [(eq? subr '/)
          (Krivine- code env stack
            (cons (/ (cadr c-stack) (car c-stack)) (cdddr c-stack)))]
        [(eq? subr '%)
          (Krivine- code env stack
            (cons (mod (cadr c-stack) (car c-stack)) (cdddr c-stack)))]
        [(eq? subr '++)
          (Krivine- code env stack
            (cons (string-append (cadr c-stack) (car c-stack)) (cdddr c-stack)))]

        [(eq? subr '=)
          (Krivine- (if (= (cadr c-stack) (car c-stack)) true false)
           env stack (cdddr c-stack))]
        [(eq? subr 'equal)
          (Krivine- (if (equal? (cadr c-stack) (car c-stack)) true false)
           env stack (cdddr c-stack))]
        [(eq? subr '<)
          (Krivine- (if (< (cadr c-stack) (car c-stack)) true false)
           env stack (cdddr c-stack))]
        [(eq? subr '<=)
          (Krivine- (if (<= (cadr c-stack) (car c-stack)) true false)
           env stack (cdddr c-stack))]))))
