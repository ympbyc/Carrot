;;;; Check.scm
;;;; 2014 Minori Yamashita <ympbyc@gmail.com>
;;;; Find type errors

(add-load-path "../lib/" :relative)

(define-module Check
  (export type-check)
  (use srfi-1)
  (use srfi-9)
  (use Util)
  (use util.match)
  (use DataTypes)

  ;; type-check ({exprs} . {types}) -> boolean
  (define (type-check exprs*types)
    (let* ([exprs-ht  (car exprs*types)]
           [types-ht  (cdr exprs*types)]
           [main-expr (hash-table-get exprs-ht 'main #f)])
      (if main-expr
          (check-main main-expr types-ht)
          #t)))

  ;; expr * {types} -> boolean
  (define (check-main expr types-ht)
    ;;    (type-of expr types-ht)
    (guard
     (exc [else (p (ref exc 'message)) #f])
     (type-of expr types-ht)
     #t))


  ;; expr * {types} -> <crt-type>
  (define-method type-of ((_ <string>)  _) (make <crt-primitive-type> :type 'String))
  (define-method type-of ((_ <number>)  _) (make <crt-primitive-type> :type 'Number))
  (define-method type-of ((_ <char>)    _) (make <crt-primitive-type> :type 'Char))
  (define-method type-of ((_ <keyword>) _) (make <crt-primitive-type> :type 'Keyword))
  (define-method type-of ((s <symbol>) types-ht) (ref types-ht s))
  (define-method type-of ((xs <list>)  types-ht)
    (cond [(quote-expr? xs)  (make <crt-primitive-type> :type 'Symbol)]
          [(lambda-expr? xs) (make <crt-function-type> :type (list (gensym) (gensym)))] ;;stub
          [else (type-of-app (type-of (car xs) types-ht)
                             (map (cut type-of <> types-ht) (cdr xs))
                             types-ht)]))


  ;; <crt-type> * [<crt-type>] -> <crt-type>
  (define-method type-of-app ((t <crt-primitive-type>) ts types-ht)
    (raise-error/message (format "Can not apply a ~S to ~S" (get-type t) (map get-type ts))))
  (define-method type-of-app ((t <crt-composite-type>) ts types-ht) t)
  (define-method type-of-app ((t <crt-type-var>) ts types-ht) t)
  (define-method type-of-app ((t <crt-function-type>) (_ <null>) types-ht) t)
  (define-method type-of-app ((t <crt-function-type>) ts types-ht)
    (let* ([ft (get-type t)]
           [binding (unify (car ft) (car ts))]
           [rest-ft (fold (fn [b ft-]
                              (replace-type-var ft- (car b) (cdr b)))
                         (cdr ft)
                         binding)]
           [rest-ft (if (= 1 (length rest-ft))
                        (car rest-ft)
                        (type-of-app (make <crt-function-type> :type rest-ft) (cdr ts) types-ht))])))


  (define (replace-type-var ft var t)
    (if (null? ft) '()
        (cons (cond [(equal? (car ft) var) t]
                    [(is-a? (car ft) <crt-composite-type>)
                     (make <crt-composite-type>
                       :name (type-name (car ft))
                       :type (replace-type-var (get-type (car ft)) var t))]
                    [(is-a? (car ft) <crt-function-type>)
                     (make <crt-function-type> :type (replace-type-var (get-type (car ft)) var t))]
                    [else    (car ft)])
              (replace-type-var (cdr ft) var t))))


  ;;a Number -> ((a Number))
  (define-method unify ((t1 <crt-primitive-type>) (t2 <crt-primitive-type>))
    (if (equal? t1 t2)
        (list (cons (make <crt-type-var> :type '__) t2))
        (raise-error/message (format "primitive types ~S and ~S don't match" t1 t2))))

  (define-method unify ((t1 <crt-composite-type>) (t2 <crt-composite-type>))
    (let ([t1- (get-type t1)]
          [t2- (get-type t2)])
      (if (eq? (type-name t1) (type-name t2))
          (apply append (map (fn [tx ty] (unify tx ty)) (get-type t1) (get-type t2)))
          (raise-error/message "different datatype"))))

  (define-method unify ((t1 <crt-type-var>) t2)
    (list (cons t1 t2)))

  (define-method unify (t1 (t2 <crt-type-var>))
    (list (cons t1 t2)))

  (define-method unify ((t1 <crt-function-type>) (t2 <crt-function-type>))
    (unify (make <crt-composite-type> :name 'Fn :type (get-type t1))
           (make <crt-composite-type> :name 'Fn :type (get-type t2)))))
