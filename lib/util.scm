(define-module Util
  (export-all)
  (use srfi-1)
  (use srfi-9)

  (define (butlast xs) (drop-right xs 1))


  (define (str . xs)
    (apply string-append (map show xs)))

  (define-method show [(x <string>)] x)
  (define-method show [(x <keyword>)] (string-append ":" (keyword->string x)))
  (define-method show [x] (format "~S" x))

  (define (separate x xs)
    (if (null? xs)
        '()
        (let1 tail (separate x (cdr xs))
              (cons (car xs)
                    (if (null? tail) tail (cons x tail))))))

  ;;h1 > h2
  (define (hash-table-union! h1 h2)
    (hash-table-for-each h2 (lambda [k v]
                              (hash-table-put! h1 k v)))
    h1)

  ;;get the value associated with the key symbol
  (define (assoc-ref env key)
    (let ((binding (assq key env)))
      (if binding (cdr binding) 'lookup-fail)))


  (define (find-map f xs)
    (cond [(null? xs) #f]
          [(f (car xs)) => identity]
          [else (find-map f (cdr xs))]))

  (define (find*map f xs)
    (cond [(null? xs) #f]
          [(f (car xs)) => (cut cons (car xs) <>)]
          [else (find*map f (cdr xs))]))

  (define (atom? x)
    (or (string? x)
        (number? x)
        (char? x)
        (keyword? x)))


  (define-syntax fn
    (syntax-rules ()
      ((_ (arg ...) exp ...)
       (lambda (arg ...) exp ...))
      ((_ arg exp ...)
       (lambda arg exp ...))))

  (define (p x) (print x) x)


  (define (lambda-expr? exp)
    (and (pair? exp) (eq? (car exp) '^)))

  (define (quote-expr? x)
    (and (pair? x) (eq? (car x) 'quote)))

  (define (native-expr? exp)
    (and (pair? exp) (eq? (car exp) '**)))


  (define (flatmap f x)
    (apply append (map f x)))

  (define (raise-error/message x)
    (raise (condition (<error> (message x)))))


  (define (print-code fmt code)
    (print
     (regexp-replace-all #/#<closure\s(.+?)>/ (format fmt code) "\\1")))



  (define (hash-table-put-! ht k v)
    (hash-table-put! ht k v)
    ht)


  (define (ht-put-cons ht key val)
    (let1 xs (hash-table-get ht key #f)
          (if xs
              (hash-table-put-! ht key (cons val xs))
              (hash-table-put-! ht key (list val)))))

  (define (genmap-merge! ht1 ht2)
    (hash-table-for-each
     ht2
     (lambda [k ys]
       (let* ([xs (hash-table-get ht1 k #f)]
              [xs (if xs xs '())])
         (hash-table-put! ht1 k (append ys xs)))))
    ht1)

  (define (inc x)
    (+ x 1))

  (define-record-type <atom>
    (atom val)
    atom*?
    (val deref reset!))

  (define (swap! atom f)
    (reset! atom (f (deref atom))))


  (define (get-main-name genmap)
    (let1 x (hash-table-get genmap 'main #f)
          (if x (car x) #f))))
