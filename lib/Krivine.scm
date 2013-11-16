;;;; Krivine's Machine in Scheme ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here
;;;
;;; reference:
;;;   http://pauillac.inria.fr/~xleroy/talks/zam-kazam05.pdf
;;;   http://pop-art.inrialpes.fr/~fradet/PDFs/HOSC07.pdf
;;;   Improving the Lazy Krivine Machine

;;; Notes ;;;
;; CLOSURE creates thunks that packs the continuation and environment together.
;; To create closures(function objects), CLOSURE the GRAB and expression followed by CONTINUE.
;; CONSTANT does not create thunks (for efficiency sake)
;; Stack may only contain Constant(WHNF), or Marker that points to CLOSURE in the heap

;;todo: make heap a hash-table
;;      GC heap

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

  (define (marker-loc mark)
    (assoc-ref mark 'location))

  (define (marker? x)
    (and
     (pair? x)
     (eq? (assoc-ref x 'type) 'marker)))


  (define *global-env* (make-hash-table 'eq?))


  (define (Krivine binding)
    (print-code " | ~S" (clos-expr (ref binding 'main)))
    (set! *global-env* binding)
    (guard (exc
            [else (print (string-append "***EXCEPTION*** " (ref exc 'message))) (raise exc)])
           (let ([res (time (Krivine- (ref binding 'main) '() '()))])
             (format #t " | The program took total of ~D steps to compute.\n\n" *step*)
             (set! *step* 0)
             res)))


  (define *step* 0)

  ;;; Krivine's Machine ;;;
  (define (Krivine- closure stack heap)

    (set! *step* (+ *step* 1))

    (if (not (pair? closure)) closure      ;;weak head normal
        (let ([expr (clos-expr closure)]
              [env  (clos-env  closure)])
          #|(print-code "expr: ~S" closure)
          (print-code "stak: ~S" stack)
          (print-code "heap: ~S" heap)
          (newline)|#
          (cond
           ;;VAR
           [(symbol? expr)
            (let* ([mark (assoc-ref env expr)]
                   [clos (if (lookup-fail? mark)
                             (hash-table-get *global-env* expr 'lookup-fail)
                             (assoc-ref heap (marker-loc mark)))])
              (cond
               [(lookup-fail? mark)
                (Krivine- clos stack heap)]
               [(clos-is-value? clos)
                (Krivine- (clos-expr clos) stack heap)] ;;VAR1 -- clos(atom) don't need env
               [else (Krivine- clos (cons mark stack) heap)]))] ;;VAR2

           ;;UPDATE
           [(and (pair? stack) (marker? (car stack)))
            (Krivine- closure (cdr stack) (acons (marker-loc (car stack)) expr heap))]

           ;;CALL
           [(lambda-expr? expr)
            (if (null? stack)
                :**partially-applied-function
                (let* ([param (cadr expr)]
                       [body  (caddr expr)]
                       [loc   (gensym)]
                       [mark  (marker loc)])
                  (Krivine- (nadeko-closure body (acons param mark env))
                            (cdr stack)
                            (acons loc (car stack) heap))))]

           [(native-expr? expr)
            (let ([res  (apply (eval (cadr expr) (interaction-environment))
                               (map (^x (Krivine- (nadeko-closure x env) '() heap))
                                    (cddr expr)))])
              (cond [(boolean? res)
                     (Krivine- (nadeko-closure (if res 'true 'false) env) stack heap)]
                    [(pair? res)
                     (Krivine- (nadeko-closure (expand-expr (consify res)) env) stack heap)]
                    [else
                     (Krivine- res stack heap)]))]

           ;;APP
           [(pair? expr)
            (let ([f (car expr)]
                  [arg (cadr expr)])
              (Krivine- (nadeko-closure f env)
                        (cons (nadeko-closure arg env) stack)
                        heap))]))))

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
