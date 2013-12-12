(add-load-path "../lib/" :relative)

(define-module Check
  (export type-check)
  (use srfi-1)
  (use srfi-9)
  (use Util)
  (use util.match)

  (define-record-type signature
    (sign sig expr idx)
    signature?
    (sig sig-sig)
    (expr sig-expr)
    (idx index))

  (define-record-type signature-list
    (sigs xs)
    signature-list?
    (xs get-sigs))

  ;;ugly
  (define (show-sigs sigs . x)
    (if (signature-list? sigs)
        (get-sigs sigs)
        sigs))

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
                                         (cons-anyway/index
                                          (cut sign (cons 'Fn sig) (cons expr params) <>)
                                          (hash-table-get binding name #f)))
                        binding))
                  binding
                  program)]
           [main (hash-table-get types 'main #f)])
      (if main
          (if (format #t "FIN: ~S\n" (check-fn (sig-expr (car main)) types)) types)
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
                     (if x x (sigs (map sig-sig (ref types expr)))))]
              [(quote-expr? expr) 'Symbol]
              [(lambda-expr? expr)
               `(Fn ,(gensym) ,(gensym))]
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
    (let* ([signs (get-sigs t1)]
           [retts
            (filter-map
             (fn [sig]
                 (let1 binding (call/cc (unify (car sig) (car t2)))
                       (and binding
                            (let1 rest-sig
                                  (fold (fn [b sig]
                                            (replace-type-var sig (car b) (cdr b)))
                                        (cdr sig) binding)

                                  (cond [(null? rest-sig)
                                         (cons 'done (last sig))] ;;too many args - do first
                                        [(= 1 (length rest-sig))
                                         (cons 'done (last rest-sig))] ;;return-type
                                        ;;too few args
                                        [(null? (cdr t2))
                                         (cons 'thunk (cons 'Fn rest-sig))]
                                        [else
                                         (cons 'done
                                               (call/cc (unify (sigs `(,(cons 'Fn rest-sig))) (cdr t2))))])))))
             ;;remove 'Fn
             (map cdr signs))])
      (p retts)
      (cond
       [(null? retts) (raise-error/message "no impl")]
       [(find (^s (eq? (car s) 'done)) retts) => cdr]
       [else (sigs (map cdr retts))])))


  ;;a Number -> ((a Number))
  (define (unify t1 t2)
    (fn [cont]
        (format #t "~14S ≡? ~S\n" (show-sigs t1) (show-sigs t2))
        (cond
         [(and (primitive? t1) (primitive? t2))
          (if (eq? t1 t2) '() (cont #f))]
         [(and (datatype? t1) (datatype? t2))
          (if (eq? (car t1) (car t2))
              (apply append (map (fn [tx ty] ((unify tx ty) cont)) (cdr t1) (cdr t2))))]
         [(type-var? t1) (list (cons t1 t2))]
         [(type-var? t2) (list (cons t2 t1))]
         [(and (signature-list? t1) (pair? t2))
          (unify-app t1 t2)]
         [else (cont #f)]))))
