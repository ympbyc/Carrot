(add-load-path "../lib/" :relative)

(define-module Check
  (export type-program)
  (use srfi-1)
  (use srfi-9)
  (use Util)
  (use util.match)


  (define (proper-def? def)
    (and (pair? def) (eq? (car def) '=)))

  ;;e.g. (= (main Number) (car (cons 1 (nil 0))))
  ;;  -> [([car . ((List a) a)]
  ;;       [([cons . ( (List a) (List a))]
  ;;         [1 . Number]
  ;;         [([nil . (a (List a))] [0 . Number]) . (List Number)]) . (List Number)]) . Number]
  ;;expr -> ({types} . typed-expr)
  (define (type-program program types)
    (let* ([types
            (fold (fn [def types]
                      (let* ([def    (if (proper-def? def) def `(= (main a) ,def))]
                             [name   (caadr def)]
                             [sig    (cdadr def)]
                             [params (drop-right (cddr def) 1)]
                             [expr   (last def)] ;;((xs true) a)
                             [generic-cell (or (hash-table-get types name #f) '())])
                        (hash-table-put! types name
                                         (cons (typed-expr `(^ ,@params ,expr) (cons 'Fn sig))
                                               generic-cell))
                        types))
                  types
                  program)]
           [main (hash-table-get types 'main #f)])
      (cons types (if main
                      (let* ([fn (tx-expr (car main))]
                             [typ (tx-type (car main))]
                             [params (zip  (drop-right (cdr fn) 1) typ)]
                             [expr (list (last fn) (last typ))]
                             [t-expr (check-fn (cons expr params) types)])
                        (format #t "typed: ~S\n" (show-typed-expr t-expr))
                        t-expr)
                      (typed-expr 0 'Number)))))



  ;; atom    -> 'Name
  ;; symbol  -> x | [signature]
  ;; (^ x y) -> [(Fn 'a 'b)]
  ;; (f x)   -> [signature]
  (define (check-fn fn types)
    (let* ([expr (caar fn)] ;;(xs true)
           ;;[rett (typed-expr expr (cadar fn))] ;;a
           [env  (cdr fn)]
           [tx (type expr env types)])
      (show-typed-expr tx)
      tx))


  (define (expand-app f args)
    (if (null? args)
        f
        (let ([ag (car args)])
          (expand-app (list f ag) (cdr args)))))


  (define (type expr env types)
    (cond
     [(string?  expr) (typed-expr expr 'String)]
     [(number?  expr) (typed-expr expr 'Number)]
     [(char?    expr) (typed-expr expr 'Char)]
     [(keyword? expr) (typed-expr expr 'Keyword)]
     [(symbol?  expr)
      (let1 x (assoc expr env)
            (if x (typed-expr x x)
                (ref types expr)))]
     [(quote-expr? expr) (typed-expr expr 'Symbol)]
     [(lambda-expr? expr)
      (list (typed-expr expr  `(Fn ,(gensym) ,(gensym))))]
     [(pair?    expr)
      (type-generic-app expr env types)]))

  (define (wrap-list x)
    (if (pair? x) x (list x)))

  ;;e.g. (+ 2 3) -> [([+ . (Number Number Number)] [2 . Number] [2 . Number]) . Number]
  (define (type-generic-app expr env types)
    (let* ([expr (expand-app (car expr) (cdr expr))]
           [fxs  (type (car expr) env types)]
           [agxs (wrap-list (type (cadr expr) env types))]
           [fx   (find (fn [fx]
                           (find (^a (unify-app (tx-type fx) (tx-type a)))
                                 agxs))
                       fxs)] [_ (show-typed-expr fx)]
           [ag   (find-map (fn [fx] (find (^a (unify-app (tx-type fx) (tx-type a))) agxs))
                           fxs)]
           [fxt (cdr (tx-type fx))]) ;;cdr to remove 'Fn
      (if (= 2 (length fxt))
          (typed-expr (list fx ag) (cadr fxt))
          (list (typed-expr (list fx ag) (cons 'Fn (cdr fxt)))))))

  (define (primitive? t)
    (case t
      [(String Number Char Keyword Symbol) #t]
      [else #f]))
  (define (type-var? t)
    (and (symbol? t) (not (primitive? t))
         ;(char-lower-case? (string->list (symbol->string t)))
         ))
  (define (datatype? t)
    (pair? t))

  (define (replace-type-var sigs var t)
    (if (null? sigs) '()
        (cons (cond [(eq? (car sigs) var) t]
                    [(pair? (car sigs)) (replace-type-var (car sigs) var t)]
                    [else (car sigs)])
              (replace-type-var (cdr sigs) var t))))

  (define (f-type? x)
    (and (pair? x) (eq? (car x) 'Fn)))


  (define (unify-app t1 t2)
    (let* ([sigt (cdr t1)] ;;cdr to remove 'Fn
           [binding (call/cc (unify (car sigt) t2))])
      (and binding
           (let1 rest-sigt
                 (fold (fn [b sigt]
                           (replace-type-var sigt (car b) (cdr b)))
                       (cdr sigt) binding)
                 (cond [(null? rest-sigt) (last sigt)] ;;too many args
                       [(= 1 (length rest-sigt)) (last rest-sigt)] ;;fully applie
                       [else (list (typed-expr '() (cons 'Fn rest-sigt)))]))))) ;;too few args



  ;;a Number -> ((a Number))
  (define (unify t1 t2)
    (fn [cont]
        ;;(format #t "~14S ≡? ~S\n" (show-typed-expr t1) (show-typed-expr t2))
        (cond
         [(and (primitive? t1) (primitive? t2))
          (if (eq? t1 t2) (list (cons t1 t2)) (cont #f))]
         [(and (f-type? t1) (typed-expr? t2)) ;;match fn type as datatype
          (call/cc (unify t1 (tx-type t2)))]
         [(and (datatype? t1) (datatype? t2))
          (if (eq? (car t1) (car t2))
              (apply append (map (fn [tx ty] ((unify tx ty) cont)) (cdr t1) (cdr t2))))]
         [(type-var? t1) (list (cons t1 t2))]
         [(type-var? t2) (list (cons t2 t1))]
         [(and (typed-expr? t1) t2)
          (unify-app t1 t2)]
         [else (cont #f)]))))
