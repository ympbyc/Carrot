(define-module Util
  (export-all)
  (use srfi-1)

  ;;h1 > h2
  (define (hash-table-union! h1 h2)
    (hash-table-for-each h2 (lambda [k v]
                              (hash-table-put! h1 k v)))
    h1)

  ;;get the value associated with the key symbol
  (define (assoc-ref env key)
    (let ((binding (assq key env)))
      (if binding (cdr binding) 'lookup-fail)))


  (define (atom? x)
    (or (string? x)
        (number? x)
        (char? x)
        (keyword? x)
        (undefined? x)))


  (define-syntax fn
    (syntax-rules ()
      ((_ (arg ...) exp ...)
       (lambda (arg ...) exp ...))
      ((_ arg exp ...)
       (lambda arg exp ...))))

  (define (p x) (print x) x)

  (define (nadeko-closure expr env)
    `([expr . ,expr]
      [env . ,env]
      [type . closure]))

  (define (clos-expr closure)
    (assoc-ref closure 'expr))

  (define (clos-env closure)
    (assoc-ref closure 'env))

  (define (clos-is-value? closure)
    (atom? (clos-expr closure)))


  (define (lambda-expr? exp)
    (eq? (car exp) '^))

  (define (quote-expr? x)
    (eq? (car x) 'quote))


  (define (native-expr? exp)
    (eq? (car exp) '**))


  (define (flatmap f x)
    (apply append (map f x)))

  (define (raise-error/message x)
    (raise (condition (<error> (message x)))))


  (define (print-code fmt code)
    (print
     (regexp-replace-all #/#<closure\s(.+?)>/ (format fmt code) "\\1"))))
