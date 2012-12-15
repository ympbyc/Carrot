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
  
  (define (Krivine code g-env)
    (if (hash-table? g-env) (set! *global-env* g-env)) ;side effect
    (Krivine- code '() '() '()))

  ;;; Krivine's Machine ;;;
  (define (Krivine- code env stack c-stack) 
    ;(print (format "code : ~S" code))
    ;(print (format "env  : ~S" env))
    ;(print (format "stack: ~S" stack))
    ;(print (format "g-env: ~S"))
    ;(newline)
  
    ;inst        inst-arg    code-rest  env stack global
    ((caar code) (cdar code) (cdr code) env stack c-stack))
  
  ;; refer a value associated with the character from either local-env or global-env
  (define (ACCESS args code env stack c-stack)
    (let ([val (assoc-ref env (car args))])
      (Krivine-
        code
        env
        (if (eq? val 'lookup-fail) 
          (cons (hash-table-get *global-env* (car args) 'lookup-fail) stack)
          (cons val stack))
        c-stack)))
  
  ;; retrieves a thunk from the stack and replace the state with its.
  ;; thunks carry all the continuation therefore no need to worry about the "frame" or "return"
  (define (CONTINUE args code env stack c-stack)
    (let* ([closure (car stack)]
           [cl-code  (assoc-ref closure 'code)]
           [cl-env   (assoc-ref closure 'env)])
    (Krivine-
      cl-code
      cl-env
      (cdr stack)
      c-stack)))
  
  ;; associate a stack-top value with the character and cons the pair onto the local-env
  (define (GRAB args code env stack c-stack)
    (Krivine-
      code
      (cons `(,(car args) . ,(car stack)) env)
      (cdr stack)
      c-stack))
  
  ;; creates a thunk that is a data carrying continuation + environment
  (define (CLOSURE args code env stack c-stack)
    (Krivine-
      code
      env
      (cons `((code . ,(car args)) (env . ,env)) stack)
      c-stack))
  
  
  ;;;; Extension for nadeko ;;;;
  
  ;; returns what's on the top of the constant stack
  (define (STOP args code env stack c-stack)
    (values (if (null? c-stack) '() (car c-stack)) *global-env*))
  
  ;; cons a self-evaluating value on to the stack
  (define (CONSTANT args code env stack c-stack)
    (Krivine-
      code
      env
      stack
      (cons (car args) c-stack)))
  
  ;; creates a global binding
  (define (DEFINE args code env stack c-stack)
    (hash-table-put! *global-env* (car args) (car stack)) ;side effect
    (Krivine-
      code
      env
      (cdr stack)
      c-stack))
  
  (define (PRIMITIVE args code env stack c-stack)
    (cond
      [(eq? (car args) '+)
        (Krivine-
          code
          env
          stack
          (cons (+ (cadr c-stack) (car c-stack)) (cddr c-stack)))])))
