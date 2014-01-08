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
              :init-keyword :checked)))

  (define-class <crt-primitive-type> (<crt-type>) ())

  (define-class <crt-function-type> (<crt-type>) ())

  (define-class <crt-type-var> (<crt-type>) ())

  (define-class <crt-composite-type> (<crt-type>)
    ((name :accessor type-name
           :init-keyword :name)))

  (define (make-unknown-crt-type x checked)
    (case x
      [(String Number Char Keyword Symbol) (make <crt-primitive-type> :type x :checked checked)]
      [else (cond [(and (pair? x) (eq? 'Fn (car x)))
                   (make <crt-function-type>  :type (map (cut make-unknown-crt-type <> #f) (cdr x))
                                              :checked checked)]
                  [(pair? x)
                   (make <crt-composite-type> :name (car x)
                                              :type (map (cut make-unknown-crt-type <> #f) (cdr x))
                                              :checked checked)]
                  [(char-upper-case? (string-ref (symbol->string x) 0))
                   (make <crt-composite-type> :name x :type '() :checked checked)]
                  [else (make <crt-type-var>  :type x :checked checked)])]))

  (define-method object-equal? ((x <crt-type>) (y <crt-type>))
    (equal? (get-type x) (get-type y)))



  (define-method write-object ((t <crt-function-type>) out)
    (format out "~S" (cons 'Fn (get-type t))))
  (define-method write-object ((t <crt-composite-type>) out)
    (format out "~S" (cons (type-name t) (get-type t))))
  (define-method write-object ((t <crt-type>) out)
    (format out "~S" (get-type t)))




  (define-class <nadeko-closure> ()
    ((expr :accessor clos-expr
           :init-keyword :expr)
     (env  :accessor clos-env
           :init-keyword :env)))

  (define-method write-object ((c <nadeko-closure>) out)
    (format out "{~S <= ~S}" (clos-expr c) (clos-env c))))
