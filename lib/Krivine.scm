;;;; Krivine's Machine in Scheme ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here
;;;
;;; reference:
;;;   http://pauillac.inria.fr/~xleroy/talks/zam-kazam05.pdf
;;;   http://pop-art.inrialpes.fr/~fradet/PDFs/HOSC07.pdf
;;;   Improving the Lazy Krivine Machine

;;; Notes ;;;
;; CLOSURE creates thunks that packs the continuation and environment together.
;; To create closures(function objects), CLOSURE the GRAB and expression followed by CONTINUE


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


  (define heap-size-default 200)
  (define *heap-size-limit* heap-size-default)

  (define *global-env* (make-hash-table 'eq?))

  (define (Krivine binding)
    ;;(print-code " | ~S" (clos-expr (ref binding 'main)))
    (set! *global-env* binding)
    (if (hash-table-get binding 'main #f)
        (guard (exc
                [else (print (string-append "***EXCEPTION*** " (ref exc 'message)))
                      (set! *step* 0)
                      '()])
               (let ([res (Krivine- (ref binding 'main) '() (make-hash-table 'eq?) '())])
                 (format #t " | The program took total of ~D steps to compute.\n\n" *step*)
                 (set! *step* 0)
                 res))
        '()))


  (define *step* 0)

  ;;; Krivine's Machine ;;;
  (define (Krivine- closure stack heap nprocs)

    (set! *step* (+ *step* 1))

    (let* ([expr (clos-expr closure)]
           [env  (clos-env  closure)]
           [inst (car expr)]
           [args  (cdr expr)])

      ;;(print-code "expr: ~S" expr)
      ;;(print-code "env : ~S" env)
      ;;(print-code "stak: ~S" (map (^m (clos-expr (ref heap (marker-loc m)))) stack))
      ;;(print-code "nprc: ~S" nprocs)
      ;;(print-code "heap: ~S" (hash-table->alist heap))
      ;;(newline)
      ;;(sys-sleep 1)

      (inst closure args env stack heap nprocs)))


  (define (REF closure args env stack heap nprocs)
    (let* ([mark (assoc-ref env (car args))]
           [clos (ref heap (marker-loc mark))])
      (if (clos-is-value? clos)
          ;;VAR2 + UPDATE done at the same time
          (begin
            (Krivine- clos stack (hash-table-put-! heap (marker-loc mark) clos) nprocs))
          ;;VAR1
          (Krivine- clos stack heap nprocs))))

  ;; if it were just var2 then  (Krivine- clos (cons mark stack))


  (define (REFG closure args env stack heap nprocs)
    (let ([clos (hash-table-get *global-env* (car args))])
      (Krivine- clos stack heap nprocs)))


  ;;CALL
  (define (FN closure args env stack heap nprocs)
    (if (or (null? stack))
        (Krivine- (ndk-closure `(,ATOM ,closure) '()) stack heap nprocs) ;;whnf
        (let* ([param (car args)]
               [body  (cadr args)]
               [mark  (car stack)])
          (Krivine- (ndk-closure body (acons param mark env))
                    (cdr stack)
                    heap
                    nprocs))))


  ;;Weak Head Normal
  (define (ATOM closure args env stack heap nprocs)
    (if (null? nprocs)
        (begin
          (format #t " | Heap Size: ~D/~D\n" (hash-table-num-entries heap) *heap-size-limit*)
          (car args))

        ;;TODO: break this down

        ;;native procedure call
        (let* ([val    (car args)]
               [v-clos (ndk-closure (list ATOM val) '())]
               [x      (car nprocs)]
               [proc   (car  x)]
               [m      (cadr x)]
               [stk    (cddr x)] ;;cddr?
               [res    (proc val)])
          ;;(format #t "~S ~S\n" (marker-loc m) (ref heap (marker-loc m)))
          ;;(print-code "~S\n" (map (^m (clos-expr (ref heap (marker-loc m)))) stk))

          (if (closure? res)
              (Krivine- (ndk-closure (list NATIVE res) '())
                        (append stk stack)
                        (hash-table-put-! heap (marker-loc m) v-clos)
                        (cdr nprocs))
              (let* ([-expr (cond
                             [(boolean? res) (list REFG (if res 'true 'false))]
                             [else           (list ATOM res)])]
                     [clos (ndk-closure -expr '())])
                (Krivine- clos
                          (append stk stack)
                          (if (boolean? res) heap
                              (hash-table-put-! heap (marker-loc m) v-clos))
                          (cdr nprocs)))))))


  ;;(native proc)
  ;;evacuate the stack, enter first closure
  (define (NATIVE closure args env stack heap nprocs)
    (let* ([m    (car stack)]
           [proc (eval (car args) (find-module 'nadeko-sandbox))])
      (Krivine- (ref heap (marker-loc m))
                '()
                (collect-garbage heap
                                 (append env (map (^m (cons (gensym "tmp") m))
                                                  (append stack
                                                          (apply append (map cdr nprocs))))))
                (cons (cons proc stack) nprocs))))


  (define (APP closure args env stack heap nprocs)
    (let* ([M (car args)]
           [N (cadr args)]
           [loc (gensym)]
           [mark (marker loc)])
      (Krivine- (ndk-closure M env)
                (cons mark stack)
                (hash-table-put-! heap loc (ndk-closure N env))
                nprocs)))


  ;;(APPVAR M (REF x))
  (define (APPVAR closure args env stack heap nprocs)
    (let* ([M    (car args)]
           [x    (cadadr args)]
           [mark (assoc-ref env x)])
      (Krivine- (ndk-closure M env) (cons mark stack) heap nprocs)))



  ;; Copy GC
  ;; heap :: {sym (CLOS expr ((sym . mark)))}
  (define (collect-garbage heap env)
    ;;(format #t ".") (flush)
    (if (> (hash-table-num-entries heap) *heap-size-limit*)
        (begin
          (hash-table-put! heap 'tmp (ndk-closure '() env)) ;;hack
          (let ([h (hash-table-fold
                    heap
                    (fn [k clos acc]
                        (for-each
                         (fn [x]
                             (let ([loc (marker-loc (cdr x))])
                               (hash-table-put! acc loc (hash-table-get heap loc))))
                         (clos-env clos))
                        acc)
                    (make-hash-table 'eq?))])
            (set! *heap-size-limit* (max heap-size-default (+ 50 (hash-table-num-entries h))))
            h))
        heap)))


(define-module nadeko-sandbox
  (export-all)
  (use Util)

  (define (c2 f)
    (fn [x] (fn [y] (f x y))))

  (define =? (c2 equal?))
  (define lt? (c2 <))
  (define gt? (c2 >))
  (define lte? (c2 <=))
  (define gte? (c2 >=))
  (define add (c2 +))
  (define sub (c2 -))
  (define mul (c2 *))
  (define div (c2 /))
  (define mo (c2 mod))
  (define ++ (c2 string-append))
  (define num->str number->string)
  (define str? string?)
  (define num? number?)

  (define (timed-print time)
    (fn [x]
        (print x)
        (+ time 1))))
