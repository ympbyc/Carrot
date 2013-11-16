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
;; Stack may only contain Constant(WHNF), or Marker that points to CLOSURE in the heap


(define-module Krivine
  (export-all)
  (use srfi-1)
  (use util.match)
  (use Util)
  (extend K-Compiler)

  ;;; Helpers ;;;

  (define (lookup-fail? x)
    (eq? x 'lookup-fail))


  (define (nadeko-closure? x)
    (and
     (pair? x)
     (eq? (assoc-ref x 'type) 'closure)))

  (define (marker loc)
    `((type . marker)
      (location . ,loc)))

  (define (marker? x)
    (and
     (pair? x)
     (eq? (assoc-ref x 'type) 'marker)))


  (define *global-env* (make-hash-table 'eq?))


  (define (Krivine binding)
    (print-code " | ~S" (clos-expr (ref binding 'main)))
    (set! *global-env* binding)
    (guard (exc
            [else (print (string-append "***EXCEPTION*** " (ref exc 'message))) '()])
           (let ([res (time (Krivine- (ref binding 'main) '()))])
             (format #t " | The program took total of ~D steps to compute.\n\n" *step*)
             (set! *step* 0)
             res)))


  (define *step* 0)

  ;;; Krivine's Machine ;;;
  (define (Krivine- closure stack)

    (set! *step* (+ *step* 1))

    (let ([expr (clos-expr closure)]
          [env  (clos-env  closure)])
      ;;(print-code "expr: ~S" expr)
      ;;(print-code "stak: ~S" stack)
      ;;(newline)
      (cond
       [(symbol? expr)
        (let* ([val (assoc-ref env expr)]
               [val (if (lookup-fail? val)
                        (hash-table-get *global-env* expr 'lookup-fail) val)])
          (Krivine- val stack))]

       [(or (atom? expr) (quote-expr? expr)) expr]

       [(lambda-expr? expr)
        (let ([param (cadr expr)]
              [body  (caddr expr)])
          (if (null? stack)
              '**partially-applied-function**
              (Krivine- (nadeko-closure body (acons param (car stack) env))
                        (cdr stack))))]

       [(native-expr? expr)
        (let ([res  (apply (eval (cadr expr) (interaction-environment))
                           (map (^x (Krivine- (nadeko-closure x env) '())) (cddr expr)))])
          (cond [(boolean? res)
                 (Krivine- (nadeko-closure (if res 'true 'false) env) stack)]
                [(pair? res)
                 (Krivine- (nadeko-closure (expand-expr (consify res)) env) stack)]
                [else res]))]

       [(pair? expr)
        (let ([f (car expr)]
              [arg (cadr expr)])
          (Krivine- (nadeko-closure f env)
                    (cons (nadeko-closure arg env) stack)))])))

  (define (consify xs) (if (null? xs) 'nil `(cons ,(car xs) ,(consify (cdr xs))))))

(define (timed f)
  (let ([cache (make-hash-table 'eq?)])
    (lambda [time . args]
      (let ([v (hash-table-get cache time #f)])
        (if v v
            (let ([v (apply f args)])
              (hash-table-put! cache time v)
              (list (+ time 1) v)))))))

(define timed-print (timed print))

(define timed-read (timed read))
