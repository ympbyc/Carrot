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


  (define (ignore f) #f)


  (define-record-type mark
      (marker loc)
      marker?
      (loc marker-loc))


  #|(define (marker loc)
    `(marker ,loc))
  (define (marker? x)
    (and (pair? x)
         (eq? (car x) 'marker)))
  (define marker-loc cadr)|#



  (define *global-env* (make-hash-table 'eq?))

  (define *ref-counts* (make-hash-table 'eq?))
  (define (inc-ref-count! ref-counts mark)
    (let ([loc (marker-loc mark)])
      (hash-table-update! ref-counts loc
                          (fn [x] (+ x 1))
                          0)))
  (define (dec-ref-count! ref-counts heap mark)
    (let* ([loc (marker-loc mark)]
           [cnt (- (hash-table-get ref-counts loc 0) 1)])
      (hash-table-put! ref-counts loc cnt)
      (when (< cnt 1)
            (hash-table-delete! ref-counts loc)
            (hash-table-delete! heap loc))))


  (define (Krivine binding)
    ;;(print-code " | ~S" (clos-expr (ref binding 'main)))
    (set! *global-env* binding)
    (set! *ref-counts* (make-hash-table 'eq?))
    (guard (exc
            [else (print (string-append "***EXCEPTION*** " (ref exc 'message)))
                  (set! *step* 0)
                  (raise exc)])
           (let ([res (Krivine- (ref binding 'main) '() (make-hash-table 'eq?))])
             ;;(format #t "ref-counts: ~D" (length (hash-table->alist *ref-counts*)))
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
      (
       (fn []
           ;;(print-code "expr: ~S" expr)
           ;;(print-code "env : ~S" env)
           ;;(print-code "stak: ~S" stack)
           ;;(print-code "heap: ~S" (hash-table->alist heap))
           ;;(newline)
           ))
      (inst closure args env stack heap)))


  (define (REF closure args env stack heap)
    (let* ([mark (assoc-ref env (car args))]
           [clos (if (lookup-fail? mark)
                     (hash-table-get *global-env* (car args) 'lookup-fail)
                     (ref heap (marker-loc mark)))])
      (if (or (lookup-fail? mark) (not (clos-is-value? clos)))
          ;;VAR1
          (Krivine- clos stack heap)
          ;;VAR2 + UPDATE done at the same time
          (begin (dec-ref-count! *ref-counts* heap mark)
                 ;;(format #t "~S to ~S\n" (marker-loc mark) clos)
                 (Krivine- clos stack (hash-table-put-! heap (marker-loc mark) clos))))))

  ;; if it were just var2 then  (Krivine- clos (cons mark stack) heap)


  ;;CALL
  (define (FN closure args env stack heap)
    (if (null? stack)
        :**partially-applied-function
        (let* ([param (car args)]
               [body  (cadr args)]
               [mark  (car stack)])
          (inc-ref-count! *ref-counts* mark)
          (Krivine- (ndk-closure body (acons param mark env))
                    (cdr stack)
                    heap))))


  ;;Weak Head Normal
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
    (let* ([M (car args)]
           [N (cadr args)]
           [loc (gensym)]
           [mark (marker loc)])
      (Krivine- (ndk-closure M env)
                (cons mark stack)
                (hash-table-put-! heap loc (ndk-closure N env)))))


  ;;(APPVAR M (REF x))
  (define (APPVAR closure args env stack heap)
    (let* ([M    (car args)]
           [x    (cadadr args)]
           [mark (assoc-ref env x)])
      (if (not (lookup-fail? mark))
          (Krivine- (ndk-closure M env) (cons mark stack) heap)

          ;;only above is the essence of this instruction
          ;;deref from global env in case the symbol is not in the env
          ;;FIX THIS PART SOOOOON
          (let* ([mark (marker x)]
                 [clos (hash-table-get *global-env* x)])
            (Krivine- (ndk-closure M env)
                      (cons mark stack)
                      (hash-table-put-! heap x clos)))))))


(define (timed-print time x)
  (print x)
  (+ time 1))
