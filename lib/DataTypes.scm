;;;; datatypes.scm
;;;; 2014 Minori Yamashita <ympbyc@gmail.com>
;;;; define data structures used throughout the system


(define-module DataTypes
  (export-all)
  (use srfi-9)
  (use Util)


  (define-class <carrot-serializable> () ())

  (define-method write-object [(x <carrot-serializable>) out]
    (let* ([class (class-of x)]
           [serializable-slots (filter (^s (get-keyword :init-keyword (cdr s) #f))
                                       (class-slots class))]
           [slot-names (map car serializable-slots)]
           [init-kws (filter-map (^s (get-keyword :init-keyword (cdr s)))
                                 serializable-slots)])
      (apply (pa$ format out (str "[ ~A" ;"#,(crt-serializable ~A "
                                  (apply str (separate " ~A " init-kws))
                                  " ~A]")
                  (class-name class))
             (map (compose show (cut slot-ref x <>)) slot-names))))

  (define-reader-ctor 'crt-serializable
    (lambda [class . xs]
      (apply (pa$ make (eval class (interaction-environment))) xs)))


  ;;;; Carrot Types

  (define-class <crt-type> (<carrot-serializable>)
    ((typ :accessor get-type
          :init-keyword :type)
     (checked :init-value #f
              :accessor   require-check?
              :init-keyword :checked)
     (check-prevented :init-value #f
                      :accessor check-prevented?)))

  (define-class <crt-primitive-type> (<crt-type>) ())

  (define-class <crt-function-type> (<crt-type>)
    ([arity :accessor get-arity
            :init-keyword :arity]
     [ret-t :accessor get-return-type
            :init-keyword :return-type]
     [param-ts :accessor get-param-types
               :init-keyword :param-types]
     [instanciator :accessor get-instanciator
                   :init-value (fn [self . _] self)
                   :init-keyword instanciator]
     [name :accessor get-name
           :init-value 'anonymous
           :init-keyword :name]))

  (define-class <crt-generic-function-type> (<crt-function-type>) ())

  (define-class <crt-type-var> (<crt-type>) ())

  (define-class <crt-composite-type> (<crt-type>)
    ((name :accessor type-name
           :init-keyword :name)))

  (define-method object-equal? ((x <crt-type>) (y <crt-type>))
    (equal? (get-type x) (get-type y)))

  (define-method object-equal? ((x <crt-composite-type>) (y <crt-composite-type>))
    (and (equal? (type-name x) (type-name y))
         (equal? (get-type x) (get-type y))))


  (define-method type->data ((t <crt-function-type>))
    (cons 'Fn (map type->data (get-type t))))
  (define-method type->data ((t <crt-composite-type>))
    (let1 s (get-type t)
          (if (null? s)
              (type-name t)
              (cons (type-name t) (map type->data (get-type t))))))
  (define-method type->data ((t <crt-type>))
    (get-type t))
  (define-method type->data (t) t)



  ;;;; CarrotVM closure

  (define-class <nadeko-closure> (<carrot-serializable>)
    ((expr :accessor clos-expr
           :init-keyword :expr)
     (env  :accessor clos-env
           :init-keyword :env)
     (name :init-value 'anonymous
           :accessor name
           :init-keyword :name)))


  ;;;; CarrotVM instructions
  (define-class <crt-inst> (<carrot-serializable>) ())

  (define-class <crt-inst-ref> (<crt-inst>)
    ((var :init-keyword :var
          :accessor var)))

  (define-class <crt-inst-refg> (<crt-inst-ref>) ())

  (define-class <crt-inst-atom> (<crt-inst>)
    ((val :init-keyword :val
          :accessor val)))

  (define-class <crt-inst-lambda> (<crt-inst>)
    ((param :init-keyword :parameter
            :accessor parameter)
     (expr :init-keyword :expression
           :accessor expression)))

  (define-class <crt-inst-native> (<crt-inst>)
    ((proc :init-keyword :procedure
           :accessor procedure)))

  (define-class <crt-inst-app> (<crt-inst>)
    ((operator :init-keyword :operator
               :accessor operator)
     (operand :init-keyword :operand
              :accessor operand)))

  (define-class <crt-inst-appvar> (<crt-inst-app>) ())


  ;;;;Tuples

  (define-class <crt-pair> (<carrot-serializable>)
    ((x :init-keyword :fst :accessor fst)
     (y :init-keyword :snd :accessor snd)))

  (define (pair x y)
    (make <crt-pair> :fst x :snd y))

  (define-class <crt-triple> (<carrot-serializable>)
    ((x :init-keyword :fst :accessor fst)
     (y :init-keyword :snd :accessor snd)
     (z :init-keyword :thd :accessor thd)))

  (define (triple x y z)
    (make <crt-triple> :fst x :snd y :thd z))



  ;;;; Hashtables

  (define (write-hash-table ht)
    (format "#,(hash-table ~S)" (hash-table->alist ht)))

  (define-reader-ctor 'hash-table alist->hash-table))
