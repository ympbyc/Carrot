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


  ;;weak head normal form?
  (define (clos-is-value? closure)
    (let ([inst (car (clos-expr closure))])
      (or (eq? inst ATOM)
          (eq? inst FN))))


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
  (define *heap* (make-hash-table 'eq?))

  (define *depth* 0)


  (define (Krivine binding)
    ;;(print-code " | ~S" (clos-expr (ref binding 'main)))
    (set! *global-env* binding)
    (set! *heap* (make-hash-table 'eq?))
    (guard (exc
            [else (print (string-append "***EXCEPTION*** " (ref exc 'message)))
                  (set! *step* 0)
                  '()])
           (let ([res (Krivine- (ref binding 'main) '())])
             (format #t " | The program took total of ~D steps to compute.\n\n" *step*)
             (set! *step* 0)
             res)))


  (define *step* 0)

  ;;; Krivine's Machine ;;;
  (define (Krivine- closure stack)

    (set! *step* (+ *step* 1))

    (let* ([expr (clos-expr closure)]
           [env  (clos-env  closure)]
           [inst (car expr)]
           [args  (cdr expr)])


      ;;(print-code "expr: ~S" expr)
      ;;(print-code "env : ~S" env)
      ;;(print-code "stak: ~S" (map (^x (if (marker? x) (marker-loc x) x)) stack))
      ;;(print-code "*heap*: ~S" (hash-table->alist *heap*))
      ;;(newline)

      (inst closure args env stack)))


  (define (REF closure args env stack)
    (let* ([mark (assoc-ref env (car args))]
           [clos (ref *heap* (marker-loc mark))])
      (if (clos-is-value? clos)
          ;;VAR2 + UPDATE done at the same time
          (begin (hash-table-put-! *heap* (marker-loc mark) clos)
                 (Krivine- clos stack))
          ;;VAR1
          (Krivine- clos stack))))

  ;; if it were just var2 then  (Krivine- clos (cons mark stack))


  (define (REFG closure args env stack)
    (let ([clos (hash-table-get *global-env* (car args))])
      (Krivine- clos stack)))


  ;;CALL
  (define (FN closure args env stack)
    (if (null? stack)
        closure                                   ;;whnf
        (let* ([param (car args)]
               [body  (cadr args)]
               [mark  (car stack)])
          (Krivine- (ndk-closure body (acons param mark env))
                    (cdr stack)))))


  ;;Weak Head Normal
  (define (ATOM closure args env stack)
    (unless (null? stack)
        (hash-table-put! *heap* (marker-loc (car stack)) closure)) ;;update
    (car args))


  (define (PRIM closure args env stack)
    (inc! *depth*)
    (let ([res  (apply (eval (car args) (interaction-environment))
                       (map (^x (Krivine- (ndk-closure x env) '()))
                            (cdr args)))])
      (dec! *depth*)
      (when (= 0 *depth*)
            (collect-garbage (fold (fn [x env] (if (marker? x) (acons (gensym "tmp") x env) env))
                                   env stack)))
      (cond [(boolean? res)
             (Krivine- (ndk-closure (list REFG (if res 'true 'false)) env) stack)]
            [else
             (Krivine- (ndk-closure (list ATOM res) '()) stack)])))


  (define (APP closure args env stack)
    (let* ([M (car args)]
           [N (cadr args)]
           [loc (gensym)]
           [mark (marker loc)])
      (hash-table-put-! *heap* loc (ndk-closure N env))
      (Krivine- (ndk-closure M env)
                (cons mark stack))))


  ;;(APPVAR M (REF x))
  (define (APPVAR closure args env stack)
    (let* ([M    (car args)]
           [x    (cadadr args)]
           [mark (assoc-ref env x)])
      (Krivine- (ndk-closure M env) (cons mark stack))))



  ;;GC
  ;; heap :: {sym (CLOS expr ((sym . mark)))}
  (define (collect-garbage env)
    (print "Running GC...")
    (hash-table-put! *heap* 'tmp (ndk-closure '() env)) ;;hack
    (print-code "HEAP SIZE BEFORE:: ~S" (hash-table-num-entries *heap*))
    (let ([new-heap
           (hash-table-fold
            *heap*
            (fn [k clos acc]
                (let ([env (clos-env clos)])
                  (for-each
                   (fn [x]
                       (let ([loc (marker-loc (cdr x))])
                         ;;(format #t "~S " loc)
                         (hash-table-put! acc loc (hash-table-get *heap* loc))))
                   env)
                  acc))
            (make-hash-table 'eq?))])
      (set! *heap* new-heap)
      (newline)
      (print-code "HEAP SIZE AFTER: ~S" (hash-table-num-entries *heap*))
      (newline))))


(define (timed-print time x)
  (print x)
  (+ time 1))
