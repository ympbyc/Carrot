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
  (use srfi-9)
  (use util.match)
  (use Util)
  (extend K-Compiler)

  ;;; Helpers ;;;

  (define (lookup-fail? x)
    (eq? x 'lookup-fail))


  (define-record-type mark
    (marker loc)
    marker?
    (loc marker-loc))


  (define *global-env* (make-hash-table 'eq?))


  (define (Krivine binding)
    (print-code " | ~S" (clos-expr (ref binding 'main)))
    (set! *global-env* binding)
    (guard (exc
            [else (print (string-append "***EXCEPTION*** " (ref exc 'message))) (raise exc)])
           (let ([res (time (Krivine- (ref binding 'main) '() (make-hash-table 'eq?)))])
             (format #t " | The program took total of ~D steps to compute.\n\n" *step*)
             (set! *step* 0)
             res)))


  (define *step* 0)

  ;;; Krivine's Machine ;;;
  (define (Krivine- closure stack heap)

    (set! *step* (+ *step* 1))

    (let ([expr (clos-expr closure)]
          [env  (clos-env  closure)])
      #|(print-code "expr: ~S" expr)
      (print-code "stak: ~S" stack)
      (print-code "heap: ~S" (hash-table->alist heap))
      (newline)|#
      (cond
       ;;VAR
       [(symbol? expr)
        (let* ([mark (assoc-ref env expr)]
               [clos (if (lookup-fail? mark)
                         (hash-table-get *global-env* expr 'lookup-fail)
                         (ref heap (marker-loc mark)))])
          (cond
           [(lookup-fail? mark)
            (Krivine- clos stack heap)]
           [(clos-is-value? clos)
            (Krivine- clos stack heap)] ;;VAR1 -- clos(atom) don't need env
           [else (Krivine- clos (cons mark stack) heap)]))] ;;VAR2

       [(or (not (pair? expr)) (quote-expr? expr)) ;;weak head normal
        (begin
          (if (pair? stack)
              (hash-table-put! heap (marker-loc (car stack)) closure))
          expr)]

       ;;UPDATE
       [(and (pair? stack) (marker? (car stack)))
        (Krivine- closure (cdr stack) (hash-table-put-! heap (marker-loc (car stack)) closure))]

       ;;CALL
       [(lambda-expr? expr)
        (if (null? stack)
            :**partially-applied-function
            (let* ([param (cadr expr)]
                   [body  (caddr expr)]
                   [loc   (gensym)]
                   [mark  (marker loc)])
              (Krivine- (ndk-closure body (acons param mark env))
                        (cdr stack)
                        (hash-table-put-! heap loc (car stack)))))]

       [(native-expr? expr)
        (let ([res  (apply (eval (cadr expr) (interaction-environment))
                           (map (^x (Krivine- (ndk-closure x env) '() heap))
                                (cddr expr)))])
          (cond [(boolean? res)
                 (Krivine- (ndk-closure (if res 'true 'false) env) stack heap)]
                [(pair? res)
                 (Krivine- (ndk-closure (expand-expr (consify res)) env) stack heap)]
                [else
                 (Krivine- (ndk-closure res env) stack heap)]))]

       ;;APP
       [(pair? expr)
        (let ([f (car expr)]
              [arg (cadr expr)])
          (Krivine- (ndk-closure f env)
                    (cons (ndk-closure arg env) stack)
                    heap))])))

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
