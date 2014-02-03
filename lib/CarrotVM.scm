;;;; Modified Krivine's Machine in Scheme ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here
;;;
;;; reference:
;;;   http://pauillac.inria.fr/~xleroy/talks/zam-kazam05.pdf
;;;   http://pop-art.inrialpes.fr/~fradet/PDFs/HOSC07.pdf
;;;   Improving the Lazy Krivine Machine

;;; Notes ;;;
;; CLOSURE creates thunks that packs the continuation and environment together.
;; To create closures(function objects), CLOSURE the GRAB and expression followed by CONTINUE


(define-module CarrotVM
  (export CarrotVM)
  (use srfi-1)
  (use srfi-9)
  (use DataTypes)
  (use util.match)
  (use Util)

  ;;; Helpers ;;;

  (define (lookup-fail? x)
    (eq? x 'lookup-fail))


  ;;weak head normal form?
  (define (clos-is-value? closure)
    (let ([inst (clos-expr closure)])
      (or (is-a? inst <crt-inst-atom>)
          (is-a? inst <crt-inst-lambda>))))

  (define-record-type mark
      (marker loc)
      marker?
      (loc marker-loc))


  (define heap-size-default 200)
  (define *heap-size-limit* heap-size-default)

  (define *global-env* (make-hash-table 'eq?))

  (define (CarrotVM exprs-ht main-name)
    (let* ([main (hash-table-get exprs-ht main-name #f)])
      (if main
          (begin
            ;;(print-code "closure: ~S" main)
            (set! *global-env* exprs-ht)
            (CarrotVM* main '() (make-hash-table 'eq?) '()))
          '())))


  (define *step* 0)

  ;;; Krivine's Machine ;;;
  (define (CarrotVM* closure stack heap nprocs)

    (set! *step* (+ *step* 1))

    (let* ([inst (clos-expr closure)]
           [env  (clos-env  closure)])

      ;;(print inst)
      ;;(print-code "env : ~S" env)
      ;;(print-code "stak: ~S" (map (^m (ref heap (marker-loc m))) stack))
      ;;(print-code "nprc: ~S" nprocs)
      ;;(print-code "heap: ~S" (hash-table->alist heap))
      ;;(newline)
      ;;(sys-sleep 1)

      (vm-step inst env stack heap nprocs)))


  (define-method vm-step [(refi <crt-inst-ref>) env stack heap nprocs]
    (let* ([mark (assoc-ref env (var refi))]
           [clos (ref heap (marker-loc mark))])
      (if (clos-is-value? clos)
          ;;VAR2 + UPDATE done at the same time
          (begin
            (CarrotVM* clos stack (hash-table-put-! heap (marker-loc mark) clos) nprocs))
          ;;VAR1
          (CarrotVM* clos stack heap nprocs))))

  ;; if it were just var2 then  (Krivine- clos (cons mark stack))


  (define-method vm-step [(refgi <crt-inst-refg>) env stack heap nprocs]
    (let1 clos (ref *global-env* (var refgi))
      (CarrotVM* clos stack heap nprocs)))


  ;;CALL
  (define-method vm-step [(lambdai <crt-inst-lambda>) env stack heap nprocs]
    (if (null? stack)
        (CarrotVM* (make <nadeko-closure>
                     :expr (make <crt-inst-atom> :val lambdai)
                     :env '())
                   stack heap nprocs) ;;whnf
        (let* ([param (parameter lambdai)]
               [body  (expression lambdai)]
               [mark  (car stack)])
          (CarrotVM* (make <nadeko-closure> :expr body :env (acons param mark env))
                     (cdr stack)
                     heap
                     nprocs))))


  (define (make-crt-bool b)
    (make <crt-inst-lambda>
      :parameter 'x
      :expression (make <crt-inst-lambda>
                    :parameter 'y
                    :expression (make <crt-inst-ref> :var (if b 'x 'y)))))

  ;;Weak Head Normal
  (define-method vm-step [(atomi <crt-inst-atom>) env stack heap nprocs]
    (if (null? nprocs) ;;no pending procedure call
        (val atomi)

        ;;TODO: break this down

        ;;native procedure call
        (let* ([val    (val atomi)]
               [v-clos (make <nadeko-closure>
                         :expr (make <crt-inst-atom> :val val) :env '())]
               [x      (car nprocs)]
               [proc   (car  x)]
               [m      (cadr x)]
               [stk    (cddr x)]
               [res    (proc val)])

          (if (closure? res)
              (CarrotVM* (make <nadeko-closure>
                           :expr (make <crt-inst-native> :procedure res) :env '())
                         (append stk stack)
                         (hash-table-put-! heap (marker-loc m) v-clos)
                         (cdr nprocs))
              (let* ([-expr (cond
                             [(boolean? res)  (make-crt-bool res)]
                             [else            (make <crt-inst-atom> :val res)])]
                     [clos (make <nadeko-closure> :expr -expr :env '())])
                (CarrotVM* clos
                           (append stk stack)
                           (if (boolean? res) heap
                               (hash-table-put-! heap (marker-loc m) v-clos))
                           (cdr nprocs)))))))


  ;;(native proc)
  ;;evacuate the stack, enter first closure
  (define-method vm-step ([nativei <crt-inst-native>] env stack heap nprocs)
    (let* ([m    (car stack)]
           [proc (eval (procedure nativei) (find-module 'nadeko-sandbox))])
      (CarrotVM* (ref heap (marker-loc m))
                 '()
                 (collect-garbage
                  heap
                  (append env (map (^m (cons (gensym "tmp") m))
                                   (append stack
                                           (apply append (map cdr nprocs))))))
                 (cons (cons proc stack) nprocs))))


  (define-method vm-step [(appi <crt-inst-app>) env stack heap nprocs]
    (let* ([M (operator appi)]
           [N (operand  appi)]
           [loc (gensym)]
           [mark (marker loc)])
      (CarrotVM* (make <nadeko-closure> :expr M :env env)
                (cons mark stack)
                (hash-table-put-! heap loc (make <nadeko-closure> :expr N :env env))
                nprocs)))


  ;;(APPVAR M (REF x))
  (define-method vm-step [(appvi <crt-inst-appvar>) env stack heap nprocs]
    (let* ([M    (operator appvi)]
           [x    (var (operand  appvi))]
           [mark (assoc-ref env x)])
      (CarrotVM* (make <nadeko-closure> :expr M :env env)
                 (cons mark stack) heap nprocs)))



  ;; Copy GC
  ;; heap :: {sym (CLOS expr ((sym . mark)))}
  (define (collect-garbage heap env)
    ;;(format #t ".") (flush)
    (if (> (hash-table-num-entries heap) *heap-size-limit*)
        (begin
          (hash-table-put! heap 'tmp (make <nadeko-closure> :expr '() :env env)) ;;hack
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

  (define c-equal? (c2 equal?))
  (define c-< (c2 <))
  (define c-> (c2 >))
  (define c-<= (c2 <=))
  (define c->= (c2 >=))
  (define c-+ (c2 +))
  (define c-- (c2 -))
  (define c-* (c2 *))
  (define c-/ (c2 /))
  (define c-mod (c2 mod))
  (define c-string-append (c2 string-append))
  (define (read- x) (read))

  (define (timed-print time)
    (fn [x]
        (print x)
        (+ time 1))))
