;;;; Three Instruction Machine in Scheme ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here
;;;
;;; reference:
;;;   www.cse.iitb.ac.in/~as/fpcourse/TIM.ps.gz
;;;   

;;; Notes ;;;
;; 

(use srfi-1)

;(define-module TIM
;  (export-all)

  (define (make-pred sym it)
    (eq? sym (car it)))

  ;; argument container
  (define (Arg n)
    (cons 'Arg n))
  (define (arg? item) (make-pred 'Arg item))
  (define (arg-n a) (cdr a))
  ;; combinator container
  (define (Comb code)
    (cons 'Comb code))
  (define (comb? item) (make-pred 'Comb item))
  (define (comb-c c)
    (cdr c))
  ;; label container
  (define (Label j)
    (cons 'Label j))
  (define (label? item) (make-pred 'Label item))
  (define (label-c lbl)
    (cdr lbl))
  ;; const container
  (define (Const k)
    (cons 'Const k))
  (define (const? item) (make-pred 'Const item))
  (define (const-v c)
    (cdr c))
  ;; self marker
  (define (Self) `())
  ;; closure
  (define (close c e)
    `(Closure ,c ,e))
  (define (closure-c cl)
    (cadr cl))
  (define (closure-e cl)
    (caddr cl))
  ;; primitive procedures
  (define (Prim op)
    (cons 'Prim op))
  (define (prim? item) (make-pred 'Prim item))
  (define (prim-op p)
    (cdr p))

  ;; determine an item to cons onto the stack on PUSH
  (define (select-item item heap)
    (cond
      [(arg?   item) (list-ref heap (arg-n item))]
      [(comb?  item) (close (comb-c item) '())]
      [(label? item) (close (label-c item) heap)]
      [(const? item) (close Self (const-v item))]))

  ;; determine the state to continue with
  (define (seelct-state item code heap stack global)
    (cond
      [(comb? item)
       (TIM (append (comb-c item) code) heap stack global)]
      
      [(label? item)
       (TIM (append (label-c item) code) heap stack global)]
      
      [(arg? item)
       (let ([En (list-ref heap (arg-n item))] [a (car stack)] [S (cdr stack)])
        (cond
          [(eq? (closure-c En) Self) (TIM (closure-c a) (closure-e a) (cons En S) global)]
          [(null? (closure-e En))    (TIM (append (closure-c En) code) '() stack  global)]
          [(label? (closure-c En))   (TIM (append (closure-c En) code) heap stack global)]))]

      [(prim? item)
       (let ([op (prim-op item)] [cl (caddr stack)] [S (cdddr stack)] [c-e closure-e])
        (cond
          [(eq? op '+)
           (TIM (closure-c cl) 
            (closure-e cl) 
            (cons (close Self (+ (c-e (cadr stack)) (c-e (car stack)))) S)
            global)]))]))



  ;TIM-Extended
  (define (TIME code global)
    (let ([g-env (if (hash-table? global) global (make-hash-table))])
      (TIM code '() '() g-env)))

  ;            PC   CF   SP
  (define (TIM code heap stack global)
    (print (format "code: ~S" code))
    (print (format "heap: ~S" heap))
    (print (format "stak: ~S" stack))
    (newline)
    ((caar code) (if (null? (cdar code)) '() (cadar code)) (cdr code) heap stack global))

  ;; Move n of arguments from the stack to heap
  (define (TAKE  n code heap stack global)
    (TIM
      code                      ;PC
      ;           n of args
      (take stack n)            ;CF
      (drop stack n)
      global)) ;SP

  ;; Push corresponding representation of a given item to the stack
  (define (PUSH  item code heap stack global)
    (TIM
      code
      heap
      (cons (select-item item heap) stack)
      global))


  (define (ENTER item code heap stack global)
    (seelct-state item code heap stack global));)


  (define (DEFN name code heap stack global)
    (hash-table-put! global name (car stack))
    (TIM
      code
      heap
      (cdr stack)
      global))

  (define (REFER name code heap stack global)
    (TIM
      code
      heap
      (cons (hash-table-get global name) stack)
      global))

  (define (HALT _ __ ___ stack ____)
    (closure-e (car stack)))

;(+ (+ 2 3) (+ 4 5))
(print (TIME `(
  (,PUSH ,(Comb `(
    (,TAKE 2)
    (,PUSH ,(Arg 1))
    (,PUSH ,(Arg 0))
    (,ENTER ,(Prim '+)))))
  (,DEFN +)
  (,PUSH ,(Label `(
    (,REFER +)
    (,TAKE 1)
    (,PUSH ,(Arg 0))
    (,REFER +23)
    (,TAKE 1)
    (,ENTER ,(Arg 0)))))
  (,DEFN ++23)
  (,PUSH ,(Label `(
    (,PUSH ,(Const 5))
    (,PUSH ,(Const 4))
    (,REFER +)
    (,TAKE 1)
    (,ENTER ,(Arg 0)))))
  (,DEFN +45)
  (,PUSH ,(Label `(
    (,PUSH ,(Const 3))
    (,PUSH ,(Const 2))
    (,REFER +)
    (,TAKE 1)
    (,ENTER ,(Arg 0)))))
  (,DEFN +23)
  (,PUSH  ,(Comb `((,HALT))))
  (,REFER  ++23)
  (,REFER +45)
  (,TAKE 1)
  (,ENTER ,(Arg 0))
  ) '()))
