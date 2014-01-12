;;;; datatypes.scm
;;;; 2014 Minori Yamashita <ympbyc@gmail.com>
;;;; define data structures used throughout the system

(define-module DataTypes
  (export-all)
  (use srfi-9)

  (define-class <crt-type> ()
    ((typ :accessor get-type
          :init-keyword :type)
     (checked :init-value #f
              :accessor   require-check?
              :init-keyword :checked)))

  (define-class <crt-primitive-type> (<crt-type>) ())

  (define-class <crt-function-type> (<crt-type>) ())

  (define-class <crt-type-var> (<crt-type>) ())

  (define-class <crt-composite-type> (<crt-type>)
    ((name :accessor type-name
           :init-keyword :name)))

  (define-method object-equal? ((x <crt-type>) (y <crt-type>))
    (equal? (get-type x) (get-type y)))



  (define-method type->data ((t <crt-function-type>))
    (cons 'Fn (map type->data (get-type t))))
  (define-method type->data ((t <crt-composite-type>))
    (let1 s (get-type t)
          (if (null? s)
              (type-name t)
              (cons (type-name t) (map type->data (get-type t))))))
  (define-method type->data ((t <crt-type>))
    (get-type t))


  (define-method write-object ((t <crt-type>) out)
    (format out "~S" (type->data t)))


  (define-class <nadeko-closure> ()
    ((expr :accessor clos-expr
           :init-keyword :expr)
     (env  :accessor clos-env
           :init-keyword :env)))

  (define-method write-object ((c <nadeko-closure>) out)
    (format out "{~S <= ~S}"  (clos-expr c) (map car (clos-env c)))))
