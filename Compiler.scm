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
  (fold-right nadekompile `((,stop)) program))

;;nadekompile :: Nadeko -> code -> code
(define (nadekompile exp code)
  (cond
    [(atom? exp)
     (cons `(,stack-constant ,exp) code)]

    [(symbol? exp)
     (cons `(,ref-arg ,exp) (cons `(,thaw) code))]

    [(eq? (car exp) 'quote)
     (cons `(,stack-constant ,(cadr exp)) code)]

    [(eq? (cadr exp) '=)
     (nadekompile `(delay ,(caddr exp)) (cons `(,def ,(car exp)) code))]

    [(eq? (cadr exp) '->)
     (let ([body (nadekompile (caddr exp) `((,restore)))])
      (cons `(,stack-closure ,(car exp) ,body) code))]

    [(eq? (car exp) 'delay)
     (cons `(,freeze ,(nadekompile (cadr exp) `((,restore)))) code)]

    [else 
     (let ([closure-app (nadekompile (car exp) (cons `(,app) code))])
      (nadekompile `(delay ,(cadr exp)) closure-app))]))


;;compile- :: Lisp -> code -> code
(define (compile- exp code)
  ;(print (format "exp : ~S" exp))
  ;(print (format "code: ~S" code))
  ;(newline)
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
    
    [(eq? (car exp) 'define)
      ;;bound (def symbol)
     (compile- `(delay ,(caddr exp)) (cons `(,def ,(cadr exp)) code))]
    
    [(eq? (car exp) 'if)
      ;;bool (sel ((code) (join)) ((code) (join)))
     (let ([t-clause (compile- (caddr exp)  `((,join)))]
           [f-clause (compile- (cadddr exp) `((,join)))])
      (compile- (cadr exp)
        (cons `(,sel ,t-clause ,f-clause) code)))]
    
    [(eq? (car exp) 'lambda)
      ;;(stack-closure symbol ((code) (restore)))
     (let ([body (compile- (caddr exp) `((,restore)))])
       (cons `(,stack-closure ,(cadr exp) ,body) code))]
    
    [(eq? (car exp) 'delay)
     ;;(freeze ((code) (restore)))
     (cons `(,freeze ,(compile- (cadr exp) `((,restore)))) code)]
    
    [else
      ;;arg closure (app)
      (let* ([closure-app (compile- (car exp) (cons `(,app) code))])
        (compile- `(delay ,(cadr exp)) closure-app))]))


;;; experiment ;;;
(print (SECD '() '() (compile 
  
  #|'(
    ;((lambda x x) 5)
    (define cons- (lambda head (lambda tail (lambda f ((f head) tail)))))
    (define infinite ((cons- 5) infinite)) ;infinit recursion creating a list
    (define car- (lambda lis (lis (lambda head (lambda tail head)))))
    (define cdr- (lambda lis (lis (lambda head (lambda tail tail)))))
    (car- (cdr- infinite))
  )|#

  '(
    (cons- = (head -> (tail -> (f -> ((f head) tail)))))
    (car-  = (head -> (tail -> head)))
    (((cons- 1) 2) car-)
  )

  ) '() '()))
