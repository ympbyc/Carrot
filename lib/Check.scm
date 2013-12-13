(add-load-path "../lib/" :relative)

(define-module Check
  (export type-program)
  (use srfi-1)
  (use srfi-9)
  (use Util)
  (use util.match)

  (define-record-type signature
    (sign typ expr)
    signature?
    (typ  sig-type)
    (expr sig-expr))

  (define (generics-cons/index f gen)
    (if gen
        (let1 xs (get-signatures gen)
              (generic-signatures (cons (f (length xs)) xs)))
        (generic-signatures (list (f 0)))))


  (define (proper-def? def)
    (and (pair? def) (eq? (car def) '=)))

  ;;e.g. (= (main Number) (car (cons 1 (nil 0))))
  ;;  -> [([car . ((List a) a)]
  ;;       [([cons . ( (List a) (List a))]
  ;;         [1 . Number]
  ;;         [([nil . (a (List a))] [0 . Number]) . (List Number)]) . (List Number)]) . Number]
  (define (type-program program types)
    (let* ([types
            (fold (fn [def types]
                      (let* ([def    (if (proper-def? def) def `(= (main a) ,def))]
                             [name   (caadr def)]
                             [sig    (cdadr def)]
                             [params (zip  (drop-right (cddr def) 1) sig)] ;;[(xs (List a))]
                             [expr   (list (last def) (last sig))] ;;((xs true) a)
                             [generic-cell (or (hash-table-get types name #f) '())])
                        (hash-table-put! types name
                                         (cons (sign (cons 'Fn sig) (cons expr params))
                                               generic-cell))
                        types))
                  types
                  program)]
           [main (hash-table-get types 'main #f)])
      (if main
          (begin (format #t "FIN: ~S\n" (check-fn (sig-expr (car main)) types))
                 types)
          types)))

  (define-record-type typed-expression
    (typed-expr e t)
    typed-expr?
    (e tx-expr)
    (t tx-type))

  (define (show-typed-expr x)
    (cond [(typed-expr? x) (cons (show-typed-expr (tx-expr x)) (show-signature (tx-type x)))]
          [(null? x) '()]
          [(pair? x) (cons (show-typed-expr (car x)) (show-typed-expr (cdr x)))]
          [else x]))


  ;; atom    -> 'Name
  ;; symbol  -> x | [signature]
  ;; (^ x y) -> [(Fn 'a 'b)]
  ;; (f x)   -> [signature]
  (define (check-fn fn types)
    (let* ([expr (caar fn)] ;;(xs true)
           ;;[rett (typed-expr expr (cadar fn))] ;;a
           [env  (cdr fn)])
      (show-typed-expr (type expr env types))))


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
            (typed-expr expr (if x x (ref types expr))))]
     [(quote-expr? expr) (typed-expr expr 'Symbol)]
     [(lambda-expr? expr)
      (typed-expr expr (sign `(Fn ,(gensym) ,(gensym)) '()))]
     [(pair?    expr)
      (type-generic-app expr env types)]))

  (define (wrap-list x)
    (if (pair? x) x (list x)))

  ;;e.g. (+ 2 3) -> [([+ . (Number Number Number)] [2 . Number] [2 . Number]) . Number]
  (define (type-generic-app expr env types)
    (let* ([expr (expand-app (car expr) (cdr expr))]
           [fx   (type (car expr) env types)]
           [agx  (type (cadr expr) env types)]
           [fts  (wrap-list (tx-type fx))]
           [agts (wrap-list (tx-type agx))]
           [retts (filter-map (fn [ft]
                                  (find-map (^a (unify-app ft a))
                                            agts))
                              fts)]) ;; redundent
      (cond [(find (^x (not (signature? x))) retts)
             => (fn [rett]
                    ;;redundent
                    (let* ([ft  (find (fn [ft]
                                         (find (^a (call/cc (unify ft a))) agts)) fts)]
                           [agt (find-map (fn [ft]
                                             (find (^a (call/cc (unify ft a))) agts)) fts)])
                      (typed-expr (list (typed-expr (tx-expr fx) ft)
                                        (typed-expr (tx-expr agx) agt)) rett)))]
            [else (p "noooo") (typed-expr expr retts)])))

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
    (let* ([sigt (cdr (sig-type t1))] ;;cdr to remove 'Fn
           [binding (call/cc (unify (car sigt) t2))])
      (and binding
           (let1 rest-sigt
                 (fold (fn [b sigt]
                           (replace-type-var sigt (car b) (cdr b)))
                       (cdr sigt) binding)
                 (cond [(null? rest-sigt) (last sigt)] ;;too many args
                       [(= 1 (length rest-sigt)) (last rest-sigt)] ;;fully applie
                       [else (p rest-sigt) (sign (cons 'Fn rest-sigt) '())]))))) ;;too few args


  (define (show-signature s)
    (if (signature? s) `(sig . ,(sig-type s)) s))

  ;;a Number -> ((a Number))
  (define (unify t1 t2)
    (fn [cont]
        (format #t "~14S ≡? ~S\n" (show-signature t1) (show-signature t2))
        (cond
         [(and (primitive? t1) (primitive? t2))
          (if (eq? t1 t2) (list (cons t1 t2)) (cont #f))]
         [(and (f-type? t1) (signature? t2)) ;;match fn type as datatype
          (call/cc (unify t1 (sig-type t2)))]
         [(and (datatype? t1) (datatype? t2))
          (if (eq? (car t1) (car t2))
              (apply append (map (fn [tx ty] ((unify tx ty) cont)) (cdr t1) (cdr t2))))]
         [(type-var? t1) (list (cons t1 t2))]
         [(type-var? t2) (list (cons t2 t1))]
         [(and (signature? t1) t2)
          (unify-app t1 t2)]
         [else (cont #f)]))))
