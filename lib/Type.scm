;;;; Type.scm
;;;; 2014 Minori Yamashita <ympbyc@gmail.com>
;;;; Type-check and resolve multimethod call

(add-load-path "../lib/" :relative)

(define-module Type
  (export acquire-checked-program)
  (use srfi-1)
  (use srfi-9)
  (use Util)
  (use util.match)
  (use DataTypes)

  (define *exprs-ht*  (make-hash-table 'eq?))
  (define *types-ht*  (make-hash-table 'eq?))
  (define *genmap-ht* (make-hash-table 'eq?))
  (define *checking* (atom '(main)))

  ;; type-check ({uniq-name    => expr}
  ;;             {uniq-name    => types}
  ;;             {generic-name => [uniq-name]}) -> ({uniq-name => expr} . main-t)
  (define (acquire-checked-program exprs*types*genmap)
    (set! *exprs-ht*  (fst exprs*types*genmap))
    (set! *types-ht*  (snd exprs*types*genmap))
    (set! *genmap-ht* (thd exprs*types*genmap))
    (reset! *checking* '(main))
    (let ([checked-exprs (make-hash-table 'eq?)]
          [mains  (hash-table-get *genmap-ht* 'main #f)])
      (hash-table-for-each
       *exprs-ht*
       (fn [name expr] ;;(format #t "~A ~A\n" name (ref *types-ht* name))
           (let1 t (ref *types-ht* name)
                 (if (require-check? t)
                     (hash-table-put! checked-exprs name
                                      (fst (type-toplevel expr t '())))
                     (hash-table-put! checked-exprs name expr)))))
      (if mains
          (let1 main-t (snd (type-toplevel (ref *exprs-ht* (car mains))
                                           (ref *types-ht* (car mains)) '()))
           (pair checked-exprs main-t))
          (pair checked-exprs 'Unit))))

  (define (print-exc exc)
    (format (standard-error-port) "~A: ~A\n" (deref *checking*) (ref exc 'message)))

  ;; (^ prams... expr) * <crt-function-type> * {types} -> (U expr #f)
  (define-method type-toplevel ((expr <list>) (t <crt-function-type>) env)
    (let* ([params (butlast (cdr expr))]
           [expr   (last expr)]
           [in-ts  (butlast (get-type t))]
           [out-t  (last (get-type t))])
      (guard  (exc [else (print-exc exc) #f])
        (let1 expr*type (type expr (append (zip params in-ts) env))
              (unify out-t (snd expr*type))
              (pair (append (cons '^ params) (list (fst expr*type)))
                    (snd expr*type))))))

  ;; expr * <crt-type> * {types} -> (U <crt-type> #f)
  (define-method type-toplevel (expr (t <crt-type>) env)
    (guard (exc [else (print-exc exc) #f])
      (let1 expr*type (type expr env)
            (unify t (snd expr*type))
            expr*type)))


  (define (gen-type-var)
    (make <crt-type-var> :type (gensym "t_var")))

  (define (prim-type x)
    (make <crt-primitive-type> :type x))

  ;; expr * {types} -> (expr * <crt-type>)
  (define-method type ((x <string>)  _) (pair x (prim-type 'String)))
  (define-method type ((x <number>)  _) (pair x (prim-type 'Number)))
  (define-method type ((x <char>)    _) (pair x (prim-type 'Char)))
  (define-method type ((x <keyword>) _) (pair x (prim-type 'Keyword)))
  (define-method type ((s <symbol>) env)
    (swap! *checking* (cut cons s <>))
    (let1 t (assoc s env)
          (if t
              (pair s (cadr t))
              (let* ([names (ref *genmap-ht* s)])
                (if (= (length names) 1)
                    (pair (ref *exprs-ht* (car names))
                          (ref *types-ht* (car names)))
                    (raise-error/message "Can't select method"))))))
  (define-method type ((xs <list>) env)
    (cond [(quote-expr? xs)  (pair xs (prim-type 'Symbol))]
          [(lambda-expr? xs) (type-lambda xs env)] ;;stub
          [(native-expr? xs) (pair xs (gen-type-var))]
          [else
           (type-app (car xs) (cdr xs) env)]))


  ;; (^ params... expr) -> (expr * <crt-type>)
  (define (type-lambda xs env)
    (let* ([paramts (cons (gen-type-var)
                          (map (^x (gen-type-var)) (butlast (cdr xs))))]
           [checked
            (type-toplevel xs
                           (make <crt-function-type> :type paramts :checked #t)
                           env)])
      (unless checked (raise-error/message "Type error inside of a lambda"))
      (pair xs (make <crt-function-type> :type (append paramts (list (snd checked)))))))


  ;; expr * [expr] * [<crt-type>] -> (expr * <crt-type>)
  (define-method type-app [(generic-name <symbol>) (argxs <list>) env]
    (if (guard (_ [else #f]) (type generic-name env)) ;;local fn call or non-gen?
        (pair (cons generic-name (map (compose fst (cut type <> env)) argxs))
              (type-of-app (snd (type generic-name env))
                           (map (compose snd (cut type <> env)) argxs)))

        (let* ([arg-expr*type (map (cut type <> env) argxs)]
               [arg-ts (map snd arg-expr*type)]
               [arg-xs (map fst arg-expr*type)]
               [selected-uniq-name*type
                (sort
                 (filter-map (fn [uniq-name]
                                 (let1 t (guarded-type-of-app
                                          (ref *types-ht* uniq-name)
                                          arg-ts)
                                       (and t (pair uniq-name t))))
                             (ref *genmap-ht* generic-name))
                 specificity-sorter)])

          (when (null? selected-uniq-name*type)
                (raise-error/message
                 (format "No applicable method ~A for ~S" generic-name argxs)))
          (when (> (length selected-uniq-name*type) 1)
                (format (standard-error-port)
                        "WARNING: ~A is ambiguous for ~S \n" generic-name argxs))

          (pair (cons (fst (car selected-uniq-name*type)) arg-xs)
                (snd (car selected-uniq-name*type))))))
  (define-method type-app [fx argxs env]
    (pair (cons fx argxs)
          (type-of-app (snd (type fx env))                            ;;type of fn
                       (map (compose snd (cut type <> env)) argxs)))) ;;types of args


  (define (replace-type-vars binding remaining-types)
    (fold (fn [b ft-]
              (replace-type-var ft- (fst b) (snd b)))
          remaining-types
          binding))


  (define-method type-of-app [(t <crt-primitive-type>) ts]
    (raise-error/message (format "Can not apply a ~S to ~S"
                                 (get-type t) (map get-type ts))))
  (define-method type-of-app [(t <crt-composite-type>) ts] t)
  (define-method type-of-app [(t <crt-type-var>) ts] t)
  (define-method type-of-app [(t <crt-function-type>) (_ <null>)] t)
  (define-method type-of-app [(ft <crt-function-type>) argts]
    (let* ([raw-ft  (get-type ft)]
           [binding (unify (car raw-ft) (car argts))]
           [rest-ft (replace-type-vars binding (cdr raw-ft))])
      (cond [(and (= 1 (length rest-ft))
                  (is-a? (car rest-ft) <crt-function-type>))
             (type-of-app (car rest-ft) (cdr argts))]
            [(and (= 1 (length rest-ft)))
             (car rest-ft)]
            [else
             (type-of-app (make <crt-function-type> :type rest-ft)
                          (cdr argts))])))


  (define (replace-type-var ft var t)
    (if (null? ft) '()
        (cons (cond [(equal? (car ft) var) t]
                    [(is-a? (car ft) <crt-composite-type>)
                     (make <crt-composite-type>
                       :name (type-name (car ft))
                       :type (replace-type-var (get-type (car ft)) var t))]
                    [(is-a? (car ft) <crt-function-type>)
                     (make <crt-function-type> :type (replace-type-var
                                                      (get-type (car ft)) var t))]
                    [else (car ft)])
              (replace-type-var (cdr ft) var t))))


  (define (guarded-unify t1 t2)
    (guard (exc [else #f])
           (unify t1 t2)))

  (define (guarded-type-of-app t ts)
    (guard (exc [else #f])
           (type-of-app t ts)))

  ;;a Number -> ((a Number))
  (define-method unify ((t1 <crt-primitive-type>) (t2 <crt-primitive-type>))
    (if (equal? t1 t2)
        (list (pair (gen-type-var) t2))
        (raise-error/message (format "Primitive type contradiction: ~S -><- ~S" t1 t2))))

  (define-method unify ((t1 <crt-composite-type>) (t2 <crt-composite-type>))
    (let ([t1- (get-type t1)]
          [t2- (get-type t2)])
      (if (eq? (type-name t1) (type-name t2))
          (apply append (map (fn [tx ty] (unify tx ty)) (get-type t1) (get-type t2)))
          (raise-error/message
           (format "Composite type container contradiction: ~S -><- ~S"
                   (type-name t1) (type-name t2))))))

  (define-method unify ((t1 <crt-type-var>) (t2 <crt-type>))
    (list (pair t1 t2)))

  (define-method unify ((t1 <crt-type>) (t2 <crt-type-var>))
    (list (pair t2 t1)))

  (define-method unify ((t1 <crt-function-type>) (t2 <crt-function-type>))
    (unify (make <crt-composite-type> :name 'Fn :type (get-type t1))
           (make <crt-composite-type> :name 'Fn :type (get-type t2))))

  (define-method unify ((t1 <crt-type>) (t2 <crt-type>))
    (raise-error/message (format "Type contradiction: ~S -><- ~S"
                                 (type->data t1)
                                 (type->data t2))))



  (define (specificity-sorter x y)
    (print x)
    (print y)
    (specific (snd x) (snd y)))

  (define-method specific ((t1 <crt-type-var>) (t2 <crt-type-var>))
    0)
  (define-method specific ((t1 <crt-composite-type>) (t2 <crt-composite-type>))
    (specific (get-type t1) (get-type t2)))
  (define-method specific ((t1 <crt-type-var>) (t2 <crt-type>))
    1)
  (define-method specific ((t1 <crt-type>) (t2 <crt-type-var>))
    -1)
  (define-method specific ((t1 <crt-type>) (t2 <crt-type>))
    0))
