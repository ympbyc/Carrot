;;;; Krivine's Machine in Scheme ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here
;;;
;;; reference:
;;;   http://pauillac.inria.fr/~xleroy/talks/zam-kazam05.pdf
;;;   http://pop-art.inrialpes.fr/~fradet/PDFs/HOSC07.pdf

;;; Notes ;;;
;; CLOSURE creates thunks that packs the continuation and environment together.
;; To create closures(function objects), CLOSURE the GRAB and expression followed by CONTINUE.
;; CONSTANT does not create thunks (for efficiency sake)
;; Stack may only contain CONSTANT inclusive-or CLOSURE


(define-module Krivine
  (export-all)
  (use srfi-1)
  (use Util)

  ;;; Helpers ;;;

  (define (lookup-fail? x)
    (eq? x 'lookup-fail))


  (define (nadeko-closure? x)
    (and
     (pair? x)
     (eq? (assoc-ref x 'type) 'closure)))


  (define (Krivine binding)
    ;;(print-code " | ~S" (assoc-ref (ref binding 'main) 'code))
    (guard (exc
            [else (print (string-append "***EXCEPTION*** " (ref exc 'message))) '()])
           (let ([res (time (Krivine- `((,ACCESS main) (,CONTINUE)) '() '() binding))])
             (format #t " | The program took total of ~D steps to compute.\n\n" *step*)
             (set! *step* 0)
             res)))


  (define *step* 0)

  ;;; Krivine's Machine ;;;
  (define (Krivine- code env stack g-env)
    #|(print-code "code : ~S" code)
    (print-code "env  : ~S" env)
    (print-code "stack: ~S" stack)
    (newline)|#

    (set! *step* (+ *step* 1))

    (if (null? code)
        (car stack)

        ;;inst        inst-arg    next-code  env stack global
        ((caar code) (cdar code) (cdr code) env stack g-env)))

  ;; refer a value associated with the character from either local-env or global-env
  (define (ACCESS args next-code env stack g-env)
    (let* ([sym (car args)]
           [val (assoc-ref env sym)]
           [val (if (lookup-fail? val) (hash-table-get g-env sym 'lookup-fail) val)])
      (if (lookup-fail? val)
          (raise-error/message (format "The symbol ~S is unbound" sym))
          (Krivine-
           next-code
           env
           (cons val stack)
           g-env))))

  ;; retrieves a thunk from the stack and replace the state with its.
  ;; thunks carry all the continuation therefore no need to worry about the "frame" or "return"
  ;; continue indicates that there are no following instruction
  (define (CONTINUE args _ env stack g-env)
    (cond [(nadeko-closure? (car stack))
           (let* ([closure  (car stack)]
                  [cl-code  (assoc-ref closure 'code)]
                  [cl-env   (assoc-ref closure 'env)])
             (Krivine-
              cl-code
              cl-env
              (cdr stack)
              g-env))]
          [#t
           (Krivine-
            '()
            env
            stack
            g-env)]
          [else
           (raise-error/message (format "Can't enter ~S" (car stack)))]))

  ;; associate a stack-top value with the character and cons the pair onto the local-env
  (define (GRAB args next-code env stack g-env)
    (if (null? stack) ;;function is remaining partially applied
        (Krivine-
         '() env (list 'partially-applied-function) g-env)
        (Krivine-
         next-code
         (alist-cons (car args) (car stack) env)
         (cdr stack)
         g-env)))

  ;; creates a thunk that is a data carrying continuation + environment
  (define (CLOSURE args next-code env stack g-env)
    (Krivine-
     next-code
     env
     (cons (nadeko-closure (car args) env)
           stack)
     g-env))


  ;;;; Extension for nadeko ;;;;

  ;; cons a self-evaluating value on to the stack
  (define (CONSTANT args next-code env stack g-env)
    (Krivine-
     next-code
     env
     (cons (car args) stack)
     g-env))


  (define (NATIVE args next-code env stack g-env)
    (let ([f (car args)]
          [arg-n (cadr args)])
      (Krivine-
       next-code
       env
       (cons (native-procedure f (reverse
                                  (map (fn [x]
                                           (Krivine- `((,CONTINUE)) env (list x) g-env)) ;;Cheatish
                                       (take stack arg-n)))
                               env)
             (drop stack arg-n))
       g-env)))


  (define (timed f)
    (let ([cache (make-hash-table 'eq?)])
      (lambda args
        (let* ([time (car args)]
               [cached (hash-table-get cache time #f)])
          (if cached
              cached
              (let ([res (apply f args)])
                (hash-table-put! cache time res)
                res))))))


  (define procedures
    (alist->hash-table
     (list
      (cons 'string?  string?)
      (cons 'number?  number?)
      (cons 'num->str number->string)
      (cons 'print (timed (fn [time str] (print str) (flush) (+ time 1))))
      (cons 'read  (timed (fn [time] (read))))
      (cons '+  +)
      (cons '-  -)
      (cons '*  *)
      (cons '/  /)
      (cons '%  mod)
      (cons '++ string-append)
      (cons '=? equal?)
      (cons '<  <)
      (cons '<= <=))))

  (define (native-procedure f args env)
    (let* ([res   (apply (ref procedures f) args)])
      (cond [(boolean? res)
             (if res
                 (nadeko-closure `((,ACCESS true)  (,CONTINUE)) env)
                 (nadeko-closure `((,ACCESS false)  (,CONTINUE)) env))]
            [(null? res) 'nil]
            [else res]))))
