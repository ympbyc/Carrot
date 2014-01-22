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

  (define *exprs-ht* (make-hash-table 'eq?))
  (define *types-ht* (make-hash-table 'eq?))
  (define *checking* (atom '(main)))

  ;; type-check ({exprs} . {types}) -> (U <crt-type> #f)
  (define (type-check exprs*types)
    (set! *exprs-ht* (car exprs*types))
    (set! *types-ht* (cdr exprs*types))
    (reset! *checking* '(main))
    (let1 main-expr (hash-table-get *exprs-ht* 'main #f)
          (if main-expr
              (check-fn main-expr (ref *types-ht* 'main) '())
              (make <crt-type> :type 'Unit))))


  (define (print-exc exc)
    (format #t "~A: ~A\n" (deref *checking*) (ref exc 'message)))

  ;; (^ prams... expr) * <crt-function-type> * {types} -> (U <crt-type> #f)
  (define-method check-fn ((expr <list>) (type <crt-function-type>) env)
    (let* ([params (butlast (cdr expr))]
           [expr   (last expr)]
           [in-ts  (butlast (get-type type))]
           [out-t  (last (get-type type))])
      (if (and (require-check? type) (not (check-prevented? type)))
          (guard (exc [else (print-exc exc) #f])
                 (set! (check-prevented? type) #t) ;;prevent loop
                 (let1 expr-t (type-of expr (append (zip params in-ts) env))
                       (unify out-t expr-t)
                       (set! (check-prevented? type) #f)
                       expr-t))
          out-t)))

  ;; expr * <crt-type> * {types} -> (U <crt-type> #f)
  (define-method check-fn (expr (type <crt-type>) env)
    (if (and (require-check? type) (not (check-prevented? type)))
        (guard (exc [else (print-exc exc) #f])
               (set! (check-prevented? type) #t) ;;prevent loop
               (let1 expr-t (type-of expr env)
                     (unify type expr-t)
                     (set! (check-prevented? type) #f)
                     expr-t))
        type))


  (define (gen-type-var)
    (make <crt-type-var> :type (gensym "t_var")))

  ;; expr * {types} -> <crt-type>
  (define-method type-of ((_ <string>)  _) (make <crt-primitive-type> :type 'String))
  (define-method type-of ((_ <number>)  _) (make <crt-primitive-type> :type 'Number))
  (define-method type-of ((_ <char>)    _) (make <crt-primitive-type> :type 'Char))
  (define-method type-of ((_ <keyword>) _) (make <crt-primitive-type> :type 'Keyword))
  (define-method type-of ((s <symbol>) env)
    (swap! *checking* (cut cons s <>))
    (let1 t (assoc s env)
          (if t (cadr t)
              (let* ([t  (ref *types-ht* s)]
                     [ex (hash-table-get *exprs-ht* s #f)])
                (when (and ex (not (check-fn ex t env)))
                      (raise-error/message
                       (format "Declared return type of `~S` doesn't agree with actual value." s)))
                t))))
  (define-method type-of ((xs <list>) env)
    (cond [(quote-expr? xs)  (make <crt-primitive-type> :type 'Symbol)]
          [(lambda-expr? xs)
           (type-of-lambda xs env)] ;;stub
          [(native-expr? xs) (gen-type-var)]
          [else (type-of-app (type-of (car xs) env)
                             (map (cut type-of <> env) (cdr xs)))]))


  ;; (^ params... expr) -> <crt-type>
  (define (type-of-lambda xs env)
    (let* ([paramts (cons (gen-type-var) (map (^x (gen-type-var)) (butlast (cdr xs))))]
           [expr-t  (check-fn xs
                              (make <crt-function-type> :type paramts :checked #t)
                              env)])
      (unless expr-t (raise-error/message "lambda"))
      (make <crt-function-type> :type (append paramts (list expr-t)))))


  ;; <crt-type> * [<crt-type>] -> <crt-type>
  (define-method type-of-app ((t <crt-primitive-type>) ts)
    (raise-error/message (format "Can not apply a ~S to ~S"
                                 (get-type t) (map get-type ts))))
  (define-method type-of-app ((t <crt-composite-type>) ts) t)
  (define-method type-of-app ((t <crt-type-var>) ts) t)
  (define-method type-of-app ((t <crt-function-type>) (_ <null>)) t)
  (define-method type-of-app ((t <crt-function-type>) ts)
    (let* ([ft (get-type t)]
           [binding (unify (car ft) (car ts))]
           [rest-ft (fold (fn [b ft-]
                              (replace-type-var ft- (car b) (cdr b)))
                         (cdr ft)
                         binding)])
      (cond [(and (= 1 (length rest-ft))
                  (is-a? (car rest-ft) <crt-function-type>))
             (type-of-app (car rest-ft) (cdr ts))]
            [(and (= 1 (length rest-ft)))
             (car rest-ft)]
            [else
             (type-of-app (make <crt-function-type> :type rest-ft) (cdr ts))])))


  (define (replace-type-var ft var t)
    (if (null? ft) '()
        (cons (cond [(equal? (car ft) var) t]
                    [(is-a? (car ft) <crt-composite-type>)
                     (make <crt-composite-type>
                       :name (type-name (car ft))
                       :type (replace-type-var (get-type (car ft)) var t))]
                    [(is-a? (car ft) <crt-function-type>)
                     (make <crt-function-type> :type (replace-type-var
                                                      (get-type (car ft)) var t))]
                    [else (car ft)])
              (replace-type-var (cdr ft) var t))))


  ;;a Number -> ((a Number))
  (define-method unify ((t1 <crt-primitive-type>) (t2 <crt-primitive-type>))
    (if (equal? t1 t2)
        (list (cons (gen-type-var) t2))
        (raise-error/message (format "Primitive type contradiction: ~S -><- ~S" t1 t2))))

  (define-method unify ((t1 <crt-composite-type>) (t2 <crt-composite-type>))
    (let ([t1- (get-type t1)]
          [t2- (get-type t2)])
      (if (eq? (type-name t1) (type-name t2))
          (apply append (map (fn [tx ty] (unify tx ty)) (get-type t1) (get-type t2)))
          (raise-error/message
           (format "Composite type container contradiction: ~S -><- ~S"
                   (type-name t1) (type-name t2))))))

  (define-method unify ((t1 <crt-type-var>) (t2 <crt-type>))
    (list (cons t1 t2)))

  (define-method unify ((t1 <crt-type>) (t2 <crt-type-var>))
    (list (cons t2 t1)))

  (define-method unify ((t1 <crt-function-type>) (t2 <crt-function-type>))
    (unify (make <crt-composite-type> :name 'Fn :type (get-type t1))
           (make <crt-composite-type> :name 'Fn :type (get-type t2))))

  (define-method unify ((t1 <crt-type>) (t2 <crt-type>))
    (raise-error/message (format "Type contradiction: ~S -><- ~S" t1 t2))))
