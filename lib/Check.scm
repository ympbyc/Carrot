(add-load-path "../lib/" :relative)

(define-module Check
  (export type-check)
  (use srfi-1)
  (use srfi-9)
  (use Util)
  (use util.match)

  (define-record-type signature
    (sign sig expr)
    signature?
    (sig sig-sig)
    (expr sig-expr))

  (define-record-type signature-list
    (sigs xs)
    signature-list?
    (xs get-sigs))

  (define (unwrap-sigs sigs)
    (if (signature-list? sigs)
        (caar (get-sigs sigs))
        sigs))

  (define (proper-def? def)
    (and (pair? def) (eq? (car def) '=)))

  ;;[expr] -> {'name => [signature]}
  ;;e.g. (= (car (List a) a) xs (xs true))
  (define (type-check program binding)
    (let* ([types (fold (fn [def binding]
                            (let* ([def    (if (proper-def? def) def `(= (main a) ,def))]
                                   [name   (caadr def)]
                                   [sig    (cdadr def)]
                                   [params (zip  (drop-right (cddr def) 1) sig)] ;;[(xs (List a))]
                                   [expr   (list (last def) (last sig))])        ;;((xs true) a)
                              (hash-table-put! binding name
                                               (cons-anyway
                                                (sign sig (cons expr params))
                                                (hash-table-get binding name #f)))
                              binding))
                        binding
                        program)]
           [main (hash-table-get types 'main #f)])
      (if main
          (if (format #t "FIN: ~S\n" (check-fn (sig-expr (car main)) types)) types)
          types)))


  (define (expand-app f args)
    (if (null? args)
        f
        (let ([ag (car args)])
          (expand-app (list f ag) (cdr args)))))


  ;; atom    -> 'Name
  ;; symbol  -> x | [signature]
  ;; (^ x y) -> [(Fn 'a 'b)]
  ;; (f x)   -> [signature]
  (define (check-fn fn types)
    (let* ([expr (caar fn)] ;;(xs true)
           [rett (cadar fn)] ;;a
           [env  (cdr fn)]
           [expt (type-of expr env types)]) ;;[(xs (List a))]
      (call/cc (unify rett (unwrap-sigs expt)))))

  (define (type-of expr env types)
    (cond
     [(string?  expr) 'String]
     [(number?  expr) 'Number]
     [(char?    expr) 'Char]
     [(keyword? expr) 'Keyword]
     [(symbol?  expr)
      (let1 x (assoc expr env)
            (if x x (sigs (map sig-sig (ref types expr)))))]
     [(quote-expr? expr) 'Symbol]
     [(lambda-expr? expr)
      `(Fn ,(gensym) ,(gensym))]
     [(pair?    expr)
      (let1 xs (check-app (expand-app (car expr) (cdr expr)) env types)
            (if (null? xs) (raise-error/message "No implementation") xs))]))

  (define (primitive? t)
    (case t
      [(String Number Char Keyword Symbol) #t]
      [else #f]))
  (define (type-var? t)
    (and (symbol? t) (not (primitive? t))))
  (define (datatype? t)
    (pair? t))

  (define (replace-type-var sigs var t)
    (if (null? sigs) '()
        (cons (cond [(eq? (car sigs) var) t]
                    [(pair? (car sigs)) (replace-type-var (car sigs) var t)]
                    [else (car sigs)])
              (replace-type-var (cdr sigs) var t))))

  ;;((cons 1) 2)
  (define (check-app expr env types)
    (let* ([fs (type-of (car expr) env types)]       ;;[signature]
          [ag (type-of (cadr expr) env types)]
          [ag (unwrap-sigs ag)]) ;;type | [signature]
      (sigs (filter-map (fn [sig]
                       (let1 binding (call/cc (unify (car sig) ag))
                             (and binding
                                  (fold (fn [b sig]
                                            (replace-type-var sig (car b) (cdr b)))
                                        (cdr sig)
                                        binding))))
                   (get-sigs fs)))))

  ;;a Number -> ((a Number))
  (define (unify t1 t2)
    ;;t1 can't be [signature]
    (fn [cont]
        (format #t "~14S ≡? ~S\n" t1 t2)
        (cond
         [(and (primitive? t1) (primitive? t2))
          (if (eq? t1 t2) '() (cont #f))]
         [(and (datatype? t1) (datatype? t2))
          (if (eq? (car t1) (car t2))
              (apply append (map (fn [x y] ((unify x y) cont)) (cdr t1) (cdr t2))))]
         [(type-var? t1) (list (cons t1 t2))]
         [(type-var? t2) (list (cons t2 t1))]
         ;;[(pair? t2) ((unify t1 (car t2)) cont)]
         ;;[(pair? t1) ((unify (car t1) t2) cont)]
         [else (cont #f)]))))
