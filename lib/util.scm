(define-module Util
  (export-all)
  (use srfi-1)
  (use srfi-9)

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

  (define-record-type nadeko-closure
    (ndk-closure expr env)
    ndk-closure?
    (expr clos-expr)
    (env  clos-env))

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
     (regexp-replace-all #/#<closure\s(.+?)>/ (format fmt code) "\\1")))

  (define (hash-table-put-! ht k v)
    (hash-table-put! ht k v)
    ht))
