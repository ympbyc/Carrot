;;;; S-expression to SECD instruction Compiler ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here
;;;
;;; reference:
;;;   http://www.geocities.jp/m_hiroi/func/abcscm33.html
;;;

(load "./SECD.scm")

;;; Helpers ;;;
(define (atom? x)
  (cond
    [(string?  x) #t]
    [(number?  x) #t]
    [(boolean? x) #t]
    [(char?    x) #t]
    [else #f]))

;;compile :: Lisp -> SECD
(define (compile program)
  (fold-right compile- `((,stop)) program))

;;compile- :: Lisp -> code -> code
(define (compile- exp code)
  (print (format "exp : ~S" exp))
  (print (format "code: ~S" code))
  (newline)
  (cond
    [(atom? exp)
      ;;(stack-constant const)
      (cons `(,stack-constant ,exp) code)]
    
    [(symbol? exp)
      ;;(ref-arg symbol) (thaw) 
      (cons `(,ref-arg ,exp) (cons `(,thaw) code))]
    
    [(eq? (car exp) 'quote)
      ;;(stack-constant symbol)
      (cons `(,stack-constant ,(cadr exp)) code)]
    
    [(eq? (car exp) ':=)
      ;;(:= (foo bar baz) (bar baz))
      ;;bound (def symbol)
      (if (< (length (cadr exp)) 2)
       (compile- `(delay ,(caddr exp)) (cons `(,def ,(caadr exp)) code)) ;no param
       (compile- `(delay (-> ,(cdadr exp) ,(caddr exp))) (cons `(,def ,(caadr exp)) code)))]
    
    [(eq? (car exp) '??)
      ;;bool (sel ((code) (join)) ((code) (join)))
     (let ([t-clause (compile- (caddr exp)  `((,join)))]
           [f-clause (compile- (cadddr exp) `((,join)))])
      (compile- (cadr exp)
        (cons `(,sel ,t-clause ,f-clause) code)))]
    
    [(eq? (car exp) '->)
      ;;(-> (x y) z)
      ;;(stack-closure symbol ((code) (restore)))
     (letrec ([body (compile- (caddr exp) `((,restore)))]
       [curry (lambda (params) 
        (if (null? (cdr params))
          `(,stack-closure ,(car params) ,body)
          `(,stack-closure ,(car params) (,(curry (cdr params)) (,restore)))))])
     (cons (curry (cadr exp)) code))]

    
    [(eq? (car exp) 'delay)
     ;;(freeze ((code) (restore)))
     (cons `(,freeze ,(compile- (cadr exp) `((,restore)))) code)]
    
    [else
      ;;(foo 1 2 3) = (((foo 1) 2) 3)
      ;;arg closure (app)
      (complis (reverse (cdr exp))
        (compile- 
          (car exp) 
          (append (map (lambda (arg) `(,app)) (cdr exp)) code)))

      ;(let* ([closure-app (compile- (car exp) (cons `(,app) code))])
        ;(compile- `(delay ,(cadr exp)) closure-app))
    ]))

(define (complis exp code)
  (if (null? exp)
    code
    (compile- `(delay ,(car exp)) (complis (cdr exp) code))))


;;; experiment ;;;
(print (SECD '() '() (compile 
  
  '(
    ;(:= (d a b c x e) a)
    ;(d 6 7 8 9 10)
    (:= (cons- head tail f) (f head tail))
    (:= (car- head tail) head)
    (:= (cdr- head tail) tail)
    (:= (infinite5) (cons- 5 infinite5))
    ;(:= (map h t f)
    ;  (?? (eq? t nil)
    ;    (f h)
    ;    (cons- (f h) (map (t car-) (t cdr-) f))))
    ;((cons- 1 (cons- 2 (cons- 3 'nil))) map (-> (it) "ee"))
    ((cons- 1 (cons- 2 (cons- 3 'nil))) cdr- cdr- car-)
    (infinite5 (-> (x y) x))
  )

  ) '() '()))
