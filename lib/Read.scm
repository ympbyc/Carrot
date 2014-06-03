;;;; read.scm
;;;; 2014 Minori Yamashita <ympbyc@gmail.com>
;;;; transform a list of s-expressions into a tuple of three hashtables:
;;;; one mapping unique name to expression,
;;;; one mapping unique name to type,
;;;; one mapping generic name to unique names

(add-load-path "../lib/" :relative)

(define-module Read
  (export read-s-exprs read-s-exprs*)
  (use srfi-1)
  (use DataTypes)
  (use Util)


  (define *synonyms* (make-hash-table 'eq?)) ;;fmm

  (define (proper-def? def)
    (and (pair? def) (case (car def) [(= =u) #t] [else #f])))

  (define (synonym-definition? x)
    (and (pair? x) (eq? (car x) 'synonym)))

  (define (non-definition? x)
    (synonym-definition? x))

  (define (type-var? x)
    (char-upper-case? (string-ref (symbol->string x) 0)))


  ;;;; read-s-exprs :: [S-expr] -> ({uniq-name    => expr}
  ;;;;                              {uniq-name    => type}
  ;;;;                              {generic-name => [uniqname]})
  (define (read-s-exprs program exprs*types*genmap)
    (read-s-exprs* program
                   (fst exprs*types*genmap)
                   (snd exprs*types*genmap)
                   (thd exprs*types*genmap)))

  (define (read-s-exprs* program exprs-ht types-ht genmap-ht)
    (cond [(null? program)
           (triple exprs-ht types-ht genmap-ht)]

          [(synonym-definition? (car program))
           (register-synonym! (car program) *synonyms*)
           (read-s-exprs* (cdr program) exprs-ht types-ht genmap-ht)]

          [else
           (let* ([def          (car program)]
                  [def          (if (proper-def? def) def `(= (main a) ,def))]
                  [generic-name (caadr def)]
                  [uniqn        (length (hash-table-get genmap-ht generic-name '()))]
                  [uniq-name    (if (= uniqn 0) ;;use the symbol unchanged
                                    generic-name
                                    (string->symbol
                                     (string-append
                                      (symbol->string generic-name)
                                      (number->string (inc uniqn)))))])
             (read-s-exprs* (cdr program)
                            (register-function uniq-name def exprs-ht)
                            (register-type uniq-name def types-ht)
                            (ht-put-cons genmap-ht generic-name uniq-name)))]))


  ;; name * def-statement * {exprs} -> {exprs}
  (define (register-function uniq-name def exprs-ht)
    (let* ([params       (butlast (cddr def))]
           [body         (last def)])
      (if (null? params)
          (hash-table-put-! exprs-ht uniq-name body)
          (hash-table-put-! exprs-ht uniq-name `(^ ,@params ,body)))))

  ;; name * def-statement * {types} -> {types}
  (define (register-type uniq-name def types-ht)
    (let ([type (cdadr def)])
      (if (= 1 (length type))
          (hash-table-put-! types-ht uniq-name
                            (make-unknown-crt-type (car type)
                                                   (not (eq? (car def) '=u))))
          (hash-table-put-! types-ht uniq-name
                            (make <crt-function-type>
                              :type (map (cut make-unknown-crt-type <> #f) type)
                              :checked (not (eq? (car def) '=u)))))))

  ;; synonym-statement * {synonyms} -> ()
  (define (register-synonym! synonym synonyms-ht)
    (let* ([alias  (cadr synonym)]
           [actual (caddr synonym)]
           [unique-actual (car (uniquify-type-var actual '()))])
      (hash-table-put! synonyms-ht alias unique-actual)))


  ;; (Container a b) -> (Container tvar12 tvar34)
  (define (uniquify-type-var t syms)
    (cond [(and (pair? t) (eq? 'lambda (car t)))
           (cons (eval t (interaction-environment)) syms)]  ;;polymorphic synonym
          [(pair? t)
           (cons (cons (car t) (car (fold (fn [t acc]
                                            (let1 x (uniquify-type-var t (cdr acc))
                                                  (cons (cons (car x) (car acc))
                                                        (cdr x))))
                                      '(() . ())
                                      (cdr t))))
                 syms)]
          [(type-var? t)
           (cons t syms)]
          [else
           (let1 s (assq t syms)
                 (if s (cons (cdr s) syms)
                     (let1 s- (gensym "tvar")
                           (cons s- (acons t s- syms)))))]))


  ;; expr * boolean -> <crt-type>
  (define (make-unknown-crt-type x checked)
    (case x
      [(String Number Char Keyword Symbol)
       (make <crt-primitive-type> :type x :checked checked)]
      [else (cond [(and (pair? x) (eq? 'Fn (car x)))
                   (make <crt-function-type>
                     :type (map (cut make-unknown-crt-type <> #f) (cdr x))
                     :checked checked)]
                  [(pair? x)
                   (let1 alias (hash-table-get *synonyms* (car x) #f)
                         (cond [(and alias (closure? alias))
                                (make-unknown-crt-type (apply alias (cdr x)) checked)]
                               [alias (make-unknown-crt-type alias checked)]
                               [else (make <crt-composite-type> :name (car x)
                                           :type (map (cut make-unknown-crt-type <> #f) (cdr x))
                                   :checked checked)]))]
                  [(type-var? x)
                   (let1 alias (hash-table-get *synonyms* x #f)
                         (if alias (make-unknown-crt-type alias checked)
                             (make <crt-composite-type> :name x :type '() :checked checked)))]
                  [else (make <crt-type-var>  :type x :checked checked)])])))
