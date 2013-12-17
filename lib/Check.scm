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
                             [generic-cell (or (hash-table-get types name #f) '())]
                             [lam (if (null? params) expr `(^ ,@params ,expr))]
                             [typ (if (null? params) (last sig) (cons 'Fn sig))])
                        (show-typed-expr (typed-expr lam typ))
                        (hash-table-put! types name
                                         (cons (typed-expr lam typ)
                                               generic-cell))
                        types))
                  types
                  program)]
           [main (hash-table-get types 'main #f)])
      (cons types (if main
                      (type-fn (list (list (tx-expr (car main)) (tx-type (car main)))) types)
                      (typed-expr 0 'Number)))))

  (define (t-expr-fn? x)
    (and (typed-expr? x)
         (pair? x)
         (lambda-expr? (tx-expr x))))

  (define (check-fn fx type types)
    (p type)
    (let* ([fn    (tx-expr fx)]
           [params (zip (drop-right (cdr fn) 1) (wrap-list type))]
           [expr   (list fn type)]
           [t-expr (type-fn (cons expr params) types)])
      t-expr))


  (define (type-fn fn types)
    (let* ([expr (caar fn)]
           ;;[rett (typed-expr expr (cadar fn))]
           [env  (cdr fn)]
           [tx (type expr env types)])
      (format #t "typed: ~S\n" (show-typed-expr tx))
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
                (p (ref types expr))))]
     [(quote-expr? expr) (typed-expr expr 'Symbol)]
     [(lambda-expr? expr)
      (if (= 1 (length (drop-right (cdr expr) 1)))
          (typed-expr expr (gensym))
          (list (typed-expr expr  `(Fn ,(gensym) ,(gensym)))))]
     [(pair?    expr)
      (type-generic-app expr env types)]))

  (define (wrap-list x)
    (if (pair? x) x (list x)))

  ;;e.g. (+ 2 3) -> [([+ . (Number Number Number)] [2 . Number] [2 . Number]) . Number]
  (define (type-generic-app expr env types)
    (let* ([fxs    (type (car expr) env types)]
           [argsxs (map (^a (wrap-list (type a env types))) (cdr expr))]
           [argts  (map (cut map tx-type <>) argsxs)]
           [fx*ft  (find*map (fn [fx]
                                 (cons 'Fn (unify-app (tx-type fx) argts)))
                             fxs)] ;;unify-app::ft*argtyps->ft
           [ft (cdr fx*ft)]
           [fx     (typed-expr (tx-expr (car fx*ft)) ft)]
           [argxs (map (fn [t argx] (find (^a (call/cc (unify t (tx-type a)))) argx))
                       (drop-right (cdr ft) 1)
                       argsxs)]
           [argxs (append argxs (map car (drop argsxs (length argxs))))])
      (list (typed-expr (cons fx argxs) (last ft)))))

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


  ;;unify-app:: ft * argtyps -> ft
  (define (unify-app ft agts)
    (let1 ft (if (f-type? ft) ;;which it should be
                 (cdr ft)     ;;cdr to remove 'Fn
                 (list ft))   ;;workaround datatype problem
          (if (= 1 (length ft))
              (list (last ft)) ;;fully applied
              (let1 binding (find-map (^a (call/cc (unify (car ft) a))) (car agts))
                    (and binding
                         (let1 rest-ft
                               (fold (fn [b ft]
                                         (replace-type-var ft (car b) (cdr b)))
                                     (cdr ft) binding)
                               (cons (cond [(assoc (car ft) binding) => cdr]
                                           [else (car ft)])
                                     (unify-app (cons 'Fn rest-ft) (cdr agts)))))))))


  ;;a Number -> ((a Number))
  (define (unify t1 t2)
    (fn [cont]
        (format #t "~14S ≡? ~S\n" (show-typed-expr t1) (show-typed-expr t2))
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
