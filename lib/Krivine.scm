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

;;      GC heap

(define-module Krivine
  (export-all)
  (use srfi-1)
  (use srfi-9)
  (use util.match)
  (use Util)

  ;;; Helpers ;;;

  (define (lookup-fail? x)
    (eq? x 'lookup-fail))


  (define (clos-is-value? closure)
    (eq? (car (clos-expr closure)) ATOM))


  (define-record-type mark
    (marker loc)
    marker?
    (loc marker-loc))

  (define (replace-marker-representation)
    (define (marker loc)
      `(marker ,loc))
    (define (marker? x)
      (and (pair? x)
           (eq? (car x) 'marker)))
    (define marker-loc cadr))

  (replace-closure-representation)
  (replace-marker-representation)


  (define *global-env* (make-hash-table 'eq?))


  (define (Krivine binding)
    (print-code " | ~S" (clos-expr (ref binding 'main)))
    (set! *global-env* binding)
    (guard (exc
            [else (print (string-append "***EXCEPTION*** " (ref exc 'message)))
                  (set! *step* 0)
                  (raise exc)])
           (let ([res (time (Krivine- (ref binding 'main) '() (make-hash-table 'eq?)))])
             (format #t " | The program took total of ~D steps to compute.\n\n" *step*)
             (set! *step* 0)
             res)))


  (define *step* 0)

  ;;; Krivine's Machine ;;;
  (define (Krivine- closure stack heap)

    (set! *step* (+ *step* 1))

    (let* ([expr (clos-expr closure)]
           [env  (clos-env  closure)]
           [inst (car expr)]
           [args  (cdr expr)])
      #|(print-code "expr: ~S" expr)
      (print-code "stak: ~S" stack)
      (print-code "heap: ~S" (hash-table->alist heap))
      (newline)|#

      (inst closure args env stack heap)))


  (define (REF closure args env stack heap)
    (let* ([mark (assoc-ref env (car args))]
           [clos (if (lookup-fail? mark)
                     (hash-table-get *global-env* (car args) 'lookup-fail)
                     (ref heap (marker-loc mark)))])
      (if (or (lookup-fail? mark) (clos-is-value? clos))
          ;;VAR1
          (Krivine- clos stack heap)
          ;;VAR2 + UPDATE done at the same time
          (Krivine- clos stack (hash-table-put-! heap (marker-loc mark) clos)))))

  ;; if it were just var2 then  (Krivine- clos (cons mark stack) heap)


  (define (FN closure args env stack heap)
    (if (null? stack)
        :**partially-applied-function
        (let* ([param (car args)]
               [body  (cadr args)]
               [loc   (gensym)]
               [mark  (marker loc)])
          (Krivine- (ndk-closure body (acons param mark env))
                    (cdr stack)
                    (hash-table-put-! heap loc (car stack))))))


  (define (ATOM closure args env stack heap)
    (if (pair? stack)
        (hash-table-put! heap (marker-loc (car stack)) closure))
    (car args))


  (define (PRIM closure args env stack heap)
    (let ([res  (apply (eval (car args) (interaction-environment))
                       (map (^x (Krivine- (ndk-closure x env) '() heap))
                            (cdr args)))])
      (cond [(boolean? res)
             (Krivine- (ndk-closure (list REF (if res 'true 'false)) env) stack heap)]
            [else
             (Krivine- (ndk-closure (list ATOM res) '()) stack heap)])))


  (define (APP closure args env stack heap)
    (let ([f (car args)]
          [v (cadr args)])
      (Krivine- (ndk-closure f env)
                (cons (ndk-closure v env) stack)
                heap))))
