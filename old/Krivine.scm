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

  ;;; global environment ;;;
  (define *global-env*
    (make-hash-table))

  ;;; Helpers ;;;

  ;;get the value associated with the key symbol
  (define (assoc-ref env key)
    (let ((binding (assq key env)))
      (if binding (cdr binding) 'lookup-fail)))
  
  (define (Krivine code env stack g-env)
    (if (hash-table? g-env) (set! *global-env* g-env)) ;side effect
    (Krivine- code env stack))

  ;;; Krivine's Machine ;;;
  (define (Krivine- code env stack) 
    ;(print (format "code : ~S" code))
    ;(print (format "env  : ~S" env))
    ;(print (format "stack: ~S" stack))
    ;(print (format "g-env: ~S"))
    ;(newline)
  
    ;inst        inst-arg    code-rest  env stack global
    ((caar code) (cdar code) (cdr code) env stack))
  
  ;; refer a value associated with the character from either local-env or global-env
  (define (ACCESS args code env stack)
    (let ([val (assoc-ref env (car args))])
      (Krivine-
        code
        env
        (if (eq? val 'lookup-fail) 
          (cons (hash-table-get *global-env* (car args) 'lookup-fail) stack)
          (cons val stack)))))
  
  ;; retrieves a thunk from the stack and replace the state with its.
  ;; thunks carry all the continuation therefore no need to worry about the "frame" or "return"
  (define (CONTINUE args code env stack)
    (let* ([closure (car stack)]
           [c-code  (assoc-ref closure 'code)]
           [c-env   (assoc-ref closure 'env)])
    (Krivine-
      c-code
      c-env
      (cdr stack))))
  
  ;; associate a stack-top value with the character and cons the pair onto the local-env
  (define (GRAB args code env stack)
    (Krivine-
      code
      (cons `(,(car args) . ,(car stack)) env)
      (cdr stack)))
  
  ;; creates a thunk that is a data carrying continuation + environment
  (define (CLOSURE args code env stack)
    (Krivine-
      code
      env
      (cons `((code . ,(car args)) (env . ,env)) stack)))
  
  
  ;;;; Extension for nadeko ;;;;
  
  ;; returns what's on the top of the stack
  (define (STOP args code env stack)
    (values (if (null? stack) '() (car stack)) *global-env*))
  
  ;; cons a self-evaluating value on to the stack
  (define (CONSTANT args code env stack)
    (Krivine-
      code
      env
      (cons (car args) stack)))
  
  ;; creates a global binding
  (define (DEFINE args code env stack)
    (hash-table-put! *global-env* (car args) (car stack)) ;side effect
    (Krivine-
      code
      env
      (cdr stack)))
  
  (define (PRIMITIVE args code env stack)
    (define (get-constant code) ;dirty part
      (receive (result _) 
        (guard (exc
          (else (values 'closure '())))
          (Krivine- code env '())) result))
    (let ([subr (car args)]
      [p-args (cdr args)]
      [true  `((,ACCESS true)  (,CONTINUE))]
      [false `((,ACCESS false) (,CONTINUE))])
    (cond
      [(eq? subr 'equal)
       (Krivine-
        (append (if (equal? (get-constant (car p-args)) (get-constant (cadr p-args))) true false) code)
        env stack)]
      [(eq? subr '<)
       (Krivine-
        (append (if (< (get-constant (car p-args)) (get-constant (cadr p-args))) true false) code)
        env stack)]
      [(eq? subr '<=)
       (Krivine-
        (append (if (<= (get-constant (car p-args)) (get-constant (cadr p-args))) true false) code)
        env stack)]
      [(eq? subr '+)
       (Krivine-
        code env
        (cons (+ (get-constant (car p-args)) (get-constant (cadr p-args))) stack))]
      [(eq? subr '-)
       (Krivine-
        code env
        (cons (- (get-constant (car p-args)) (get-constant (cadr p-args))) stack))]
      [(eq? subr '*)
       (Krivine-
        code env
        (cons (* (get-constant (car p-args)) (get-constant (cadr p-args))) stack))]
      [(eq? subr '/)
       (Krivine-
        code env
        (cons (/ (get-constant (car p-args)) (get-constant (cadr p-args))) stack))]
      [(eq? subr '%)
       (Krivine-
        code env
        (cons (mod (get-constant (car p-args)) (get-constant (cadr p-args))) stack))]
      [(eq? subr '++)
       (Krivine-
        code env
        (cons (string-append (get-constant (car p-args)) (get-constant (cadr p-args))) stack))]
      [(eq? subr 'num->str)
       (Krivine-
        code env
        (cons (number->string (get-constant (car p-args))) stack))]
      [(eq? subr 'string?)
       (Krivine-
        (append (if (string? (get-constant (car p-args))) true false) code) env stack)]
      [(eq? subr 'number?)
       (Krivine-
        (append (if (number? (get-constant (car p-args))) true false) code)
        env stack)]
      [(eq? subr 'print)
       (print (get-constant (car p-args)))
       (Krivine-
        code env stack)]
      [(eq? subr 'time)
       (time (get-constant (car p-args)))
       (Krivine-
        code env stack)]))))