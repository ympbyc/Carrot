;;;; Krivine's Machine in Scheme ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here
;;;
;;; reference:
;;;   http://pauillac.inria.fr/~xleroy/talks/zam-kazam05.pdf
;;;   http://pop-art.inrialpes.fr/~fradet/PDFs/HOSC07.pdf

;;; Notes ;;;
;; CLOSURE creates thunks that packs the continuation and environment together. 
;; To create closures(function objects), CLOSURE the GRAB and expression followed by CONTINUE.
;;

(use srfi-1)

(define-module Krivine
  (export-all)

  ;;; Helpers ;;;
  
  ;;get the value associated with the key symbol
  (define (assoc-ref env key)
    (let ((binding (assq key env)))
      (if binding (cdr binding) 'lookup-fail)))
  
  
  ;;; Krivine's Machine ;;;
  (define (Krivine code env stack g-env) 
    ;(print (format "code : ~S" code))
    ;(print (format "env  : ~S" env))
    ;(print (format "stack: ~S" stack))
    ;(print (format "g-env: ~S" g-env))
    ;(newline)
  
    ;inst        inst-arg    code-rest  env stack global
    ((caar code) (cdar code) (cdr code) env stack g-env))
  
  ;; refer a value associated with the character from either local-env or global-env
  (define (ACCESS args code env stack g-env)
    (let ([val (assoc-ref env (car args))])
      (Krivine
        code
        env
        (if (eq? val 'lookup-fail) 
          (cons (assoc-ref g-env (car args)) stack)
          (cons val stack))
        g-env)))
  
  ;; retrieves a thunk from the stack and replace the state with its.
  ;; thunks carry all the continuation therefore no need to worry about the "frame" or "return"
  (define (CONTINUE args code env stack g-env)
    (let* ([closure (car stack)]
           [c-code  (assoc-ref closure 'code)]
           [c-env   (assoc-ref closure 'env)])
    (Krivine
      c-code
      c-env
      (cdr stack)
      g-env)))
  
  ;; associate a stack-top value with the character and cons the pair onto the local-env
  (define (GRAB args code env stack g-env)
    (Krivine
      code
      (cons `(,(car args) . ,(car stack)) env)
      (cdr stack)
      g-env))
  
  ;; creates a thunk that is a data carrying continuation + environment
  (define (CLOSURE args code env stack g-env)
    (Krivine
      code
      env
      (cons `((code . ,(car args)) (env . ,env)) stack)
      g-env))
  
  
  ;;;; Extension for nadeko ;;;;
  
  ;; returns what's on the top of the stack
  (define (STOP args code env stack g-env)
    (values (if (null? stack) '() (car stack)) g-env))
  
  ;; cons a self-evaluating value on to the stack
  (define (CONSTANT args code env stack g-env)
    (Krivine
      code
      env
      (cons (car args) stack)
      g-env))
  
  ;; creates a global binding
  (define (DEFINE args code env stack g-env)
    (Krivine
      code
      env
      (cdr stack)
      (cons `(,(car args) . ,(car stack)) g-env)))
  
  (define (PRIMITIVE args code env stack g-env)
    (define (get-constant code) ;dirty part
      (receive (result _) 
        (guard (exc
          (else (values 'closure '())))
          (Krivine code env '() g-env)) result))
    (let ([subr (car args)]
      [p-args (cdr args)]
      [true  `((,ACCESS true)  (,CONTINUE))]
      [false `((,ACCESS false) (,CONTINUE))])
    (cond
      [(eq? subr 'equal)
       (Krivine
        (append (if (equal? (get-constant (car p-args)) (get-constant (cadr p-args))) true false) code)
        env stack g-env)]
      [(eq? subr '+)
       (Krivine
        code env
        (cons (+ (get-constant (car p-args)) (get-constant (cadr p-args))) stack)
        g-env)]
      [(eq? subr '-)
       (Krivine
        code env
        (cons (- (get-constant (car p-args)) (get-constant (cadr p-args))) stack)
        g-env)]
      [(eq? subr '*)
       (Krivine
        code env
        (cons (* (get-constant (car p-args)) (get-constant (cadr p-args))) stack)
        g-env)]
      [(eq? subr '/)
       (Krivine
        code env
        (cons (/ (get-constant (car p-args)) (get-constant (cadr p-args))) stack)
        g-env)]
      [(eq? subr '%)
       (Krivine
        code env
        (cons (mod (get-constant (car p-args)) (get-constant (cadr p-args))) stack)
        g-env)]
      [(eq? subr '++)
       (Krivine
        code env
        (cons (string-append (get-constant (car p-args)) (get-constant (cadr p-args))) stack)
        g-env)]
      [(eq? subr 'num->str)
       (Krivine
        code env
        (cons (number->string (get-constant (car p-args))) stack)
        g-env)]
      [(eq? subr 'string?)
       (Krivine
        (append (if (string? (get-constant (car p-args))) true false) code)
        env stack g-env)]
      [(eq? subr 'number?)
       (Krivine
        (append (if (number? (get-constant (car p-args))) true false) code)
        env stack g-env)]
      [(eq? subr 'print)
       (print (get-constant (car p-args)))
       (Krivine
        code env stack g-env)]))))