(add-load-path "../lib/" :relative)

(define-module Check
  (export type-check)
  (use srfi-1)
  (use srfi-9)
  (use Util)
  (use util.match)

  (define-record-type signature
    (sign typ expr idx)
    signature?
    (typ  sig-type)
    (expr sig-expr)
    (idx  sig-idx))

  (define-record-type generics
    (generic-signatures xs)
    generic-signatures?
    (xs get-signatures))


  (define (generics-cons/index f gen)
    (if gen
        (let1 xs (get-signatures gen)
              (generic-signatures (cons (f (length xs)) xs)))
        (generic-signatures (list (f 0)))))


  (define (proper-def? def)
    (and (pair? def) (eq? (car def) '=)))

  ;;[expr] -> {'name => [signature]}
  ;;e.g. (= (car (List a) a) xs (xs true))
  (define (type-check program binding)
    (let* ([types
            (fold (fn [def binding]
                      (let* ([def    (if (proper-def? def) def `(= (main a) ,def))]
                             [name   (caadr def)]
                             [sig    (cdadr def)]
                             [params (zip  (drop-right (cddr def) 1) sig)] ;;[(xs (List a))]
                             [expr   (list (last def) (last sig))])        ;;((xs true) a)
                        (hash-table-put! binding name
                                         (generics-cons/index
                                          (cut sign (cons 'Fn sig) (cons expr params) <>)
                                          (hash-table-get binding name #f)))
                        binding))
                  binding
                  program)]
           [main (hash-table-get types 'main #f)])
      (if main
          (if (format #t "FIN: ~S\n"
                      (check-fn (sig-expr (car (get-signatures main))) types)) types)
          types)))


  ;; atom    -> 'Name
  ;; symbol  -> x | [signature]
  ;; (^ x y) -> [(Fn 'a 'b)]
  ;; (f x)   -> [signature]
  (define (check-fn fn types)
    (let* ([expr (caar fn)] ;;(xs true)
           [rett (cadar fn)] ;;a
           [env  (cdr fn)]
           [expt (type-of expr env types)]) ;;[(xs (List a))]
      (call/cc (unify rett expt))))

  (define (type-of expr env types)
    (let ([x (cond
              [(string?  expr) 'String]
              [(number?  expr) 'Number]
              [(char?    expr) 'Char]
              [(keyword? expr) 'Keyword]
              [(symbol?  expr)
               (let1 x (assoc expr env)
                     (if x x (ref types expr)))]
              [(quote-expr? expr) 'Symbol]
              [(lambda-expr? expr)
               (generic-signatures (list (sign `(Fn ,(gensym) ,(gensym)) '() 0)))]
              [(pair?    expr)
               (call/cc (unify (type-of (car expr) env types)
                               (map (^x (type-of x env types)) (cdr expr))))])])
      (if (not x) (format #t "type is false: ~S\n" expr))
      x))

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
    (let* ([gensigs (get-signatures t1)]
           [retts
            (filter-map
             (fn [sig]
                 (let* ([idx (sig-idx sig)]
                        [ftype (cdr (sig-type sig))] ;;cdr to remove 'Fn
                        [binding (call/cc (unify (car ftype) (car t2)))])
                   (and binding
                        (let1 rest-ftype
                              (fold (fn [b ftype]
                                        (replace-type-var ftype (car b) (cdr b)))
                                    (cdr ftype) binding)

                              (cond [(null? rest-ftype)
                                     (cons 'done (last ftype))] ;;too many args - do first
                                    [(= 1 (length rest-ftype))
                                     (cons 'done (last rest-ftype))] ;;return-type
                                    ;;too few args
                                    [(null? (cdr t2))
                                     (cons 'thunk (sign (cons 'Fn rest-ftype) '() idx))]
                                    [else
                                     (cons 'done
                                           (call/cc (unify (generic-signatures `(,(sign (cons 'Fn rest-ftype) '() idx))) (cdr t2))))])))))
             gensigs)])
      (p retts)
      (cond
       [(null? retts) (raise-error/message "no impl")]
       [(find (^s (eq? (car s) 'done)) retts) => cdr]
       [else (generic-signatures (map cdr retts))])))


  (define (show-generics x)
    (if (generic-signatures? x)
        (map sig-type (get-signatures x))
        x))

  ;;a Number -> ((a Number))
  (define (unify t1 t2)
    (fn [cont]
        (format #t "~14S ≡? ~S\n" (show-generics t1) (show-generics t2))
        (cond
         [(and (primitive? t1) (primitive? t2))
          (if (eq? t1 t2) '() (cont #f))]
         [(and (f-type? t1) (generic-signatures? t2)) ;;match fn type as datatype
          (call/cc (unify t1 (sig-type (car (get-signatures t2)))))]
         [(and (datatype? t1) (datatype? t2))
          (if (eq? (car t1) (car t2))
              (apply append (map (fn [tx ty] ((unify tx ty) cont)) (cdr t1) (cdr t2))))]
         [(type-var? t1) (list (cons t1 t2))]
         [(type-var? t2) (list (cons t2 t1))]
         [(and (generic-signatures? t1) (pair? t2))
          (unify-app t1 t2)]
         [else (cont #f)]))))
