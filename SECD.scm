;;;; SECD Machine in Scheme ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here
;;;
;;; reference:
;;;   http://www.geocities.jp/m_hiroi/func/abcscm33.html
;;;   http://en.wikipedia.org/wiki/SECD_machine
;;;
;;; The description of each instruction is copied from wikipedia.org on 22 Nov 2012

;;; Spec to note ;;;
;; every function take exactly one argument.
;; curry your function manually if you want more than one argument.
;;
;; VM is responsible of looking up the environment unlike the original SECD
;;
;; VM is capable of handling `freeze` and `thaw` instruction which can be used to simulate lazy evaluation

(use srfi-1)

;;; Helpers ;;;

;;data structure for closure
(define (data-closure param code env)
  (lambda (f)
    (f param code env)))
(define (cls-param p c e) p)
(define (cls-code  p c e) c)
(define (cls-env   p c e) e)

;;data structure for thunk
(define (data-thunk code env)
  (lambda (f) (f code env)))
(define (thk-code c e) c)
(define (thk-env  c e) e)

;;get the value associated with the key symbol
(define (env-ref env key)
  (let ((binding (assq key env)))
    (if binding (cdr binding) 'lookup-fail)))

;;; SECD Machine ;;;
(define (SECD stack env code dump g-env)
  ;(print (format "stack: ~S" stack))
  ;(print (format "env  : ~S" env))
  (print (format "code : ~S" code))
  ;(print (format "dump : ~S" dump))
  ;(print (format "g-env: ~S" g-env))
  (newline)

  ;inst        args        stack env code       dump global-env
  (values '() '()));((caar code) (cdar code) stack env (cdr code) dump g-env))


;;; Instructions ;;;

;;ldc
;; pushes a constant argument onto the stack
(define (stack-constant args stack env code dump g-env)
  (SECD
    ;     constant
    (cons (car args) stack) ;S
    env                     ;E
    code                    ;C
    dump                    ;D
    g-env))                  

;;ld
;; pushes the value of a variable onto the stack. 
;; The variable is indicated by the argument, a symbol.
;; Try the local env first then g-env if failed
(define (ref-arg args stack env code dump g-env)
  (let ((val (env-ref env (car args))))
    (SECD
      (cons (if (eq? val 'lookup-fail)
        (env-ref g-env (car args))
        val) stack)                ;S
      env                          ;E
      code                         ;C
      dump                         ;D
      g-env)))

;;ldf
;; takes one list argument representing a function. 
;; It constructs a closure (a pair containing the function and the current environment)
;; and pushes that onto the stack.
(define (stack-closure args stack env code dump g-env)
  (SECD
    ;                   param      code
    (cons (data-closure (car args) (cadr args) env) stack) ;S
    env                                                    ;E
    code                                                   ;C
    dump                                                   ;D
    g-env))

;;ap
;; pops a closure and a list of parameter values from the stack. 
;; The closure is applied to the parameters by installing its environment as the current one, 
;; pushing the parameter list in front of that, clearing the stack, and setting C to the closure's function pointer. 
;; The previous values of S, E, and the next value of C are saved on the dump.
(define (app args stack env code dump g-env)
  (let* (
    (closure   (car stack))
    (clos-prm  (closure cls-param))
    (clos-code (closure cls-code)) ;code enclosed in the closure
    (clos-env  (closure cls-env))) ;enclosed environment
    (SECD
      '()                                          ;S
      ;        symbol      argument
      (cons `(,clos-prm . ,(cadr stack)) clos-env) ;E
      clos-code                                    ;C
      ;           stack-(closure+arg)
      (cons (list (cddr stack) env code) dump)     ;D
      g-env)))

;;ret
;; pops one return value from the stack, 
;; restores S, E, and C from the dump, and pushes the return value onto the now-current stack.
(define (restore args stack env code dump g-env)
  (let* (
    (frame (car dump))
    (restoring-stack (car frame))
    (restoring-env   (cadr frame))
    (restoring-code  (caddr frame)))
    (SECD
      ;     value returned
      (cons (car stack) restoring-stack) ;S
      restoring-env                      ;E
      restoring-code                     ;C
      (cdr dump)                         ;D
      g-env)))

;;def
;; push stack top to g-env
(define (def args stack env code dump g-env)
  (SECD
    (cdr stack) ;S
    env         ;E
    code        ;C
    dump        ;D
    (cons `(,(car args) . ,(car stack)) g-env)))

;;freeze
;; delays the evaluation of the code until thawing
;; creates a thunk (or promice) and stack it
(define (freeze args stack env code dump g-env)
  (SECD
    (cons (data-thunk (car args) env) stack) ;S
    env                                      ;E
    code                                     ;C
    dump                                     ;D
    g-env))

;;thaw
;; evaluates the code inside the thunk in its environment.
;; creates a call frame like app
(define (thaw args stack env code dump g-env)
  (let* (
    (thunk (car stack))
    (thunk-code (thunk thk-code))
    (thunk-env  (thunk thk-env)))
   (SECD
     '()                                     ;S
     thunk-env                               ;E
     thunk-code                              ;C
     (cons (list (cdr stack) env code) dump) ;D
     g-env)))

;;stop
;; stops the Machine and return the value at the top of the stack
(define (stop args stack env code dump g-env)
  (values (if (null? stack) stack (car stack)) g-env)) ;;return `values`. useful for REPL



;;; primitives ;;;
(define (primitive args stack env code dump g-env)
  (let ((subr (car args)))
  (cond
    [(eq? subr 'equal) 
      (let ([bool (if (equal? (cadr stack) (car stack)) 'true 'false)])
        (SECD 
          (cddr stack)
          env 
          (cons `(,ref-arg ,bool) (cons `(,thaw) code)) 
          dump g-env))]
    [(eq? subr '+)
      (SECD
        (cons (+ (cadr stack) (car stack)) (cddr stack))
        env code dump g-env)]
    [(eq? subr '-)
      (SECD
        (cons (- (cadr stack) (car stack)) (cddr stack))
        env code dump g-env)]
    [(eq? subr '*)
      (SECD
        (cons (* (cadr stack) (car stack)) (cddr stack))
        env code dump g-env)]
    [(eq? subr '/)
      (SECD
        (cons (/ (cadr stack) (car stack)) (cddr stack))
        env code dump g-env)])))

