;;;; read.scm
;;;; 2014 Minori Yamashita <ympbyc@gmail.com>
;;;; transform a list of s-expressions into a tuple of two hashtables:
;;;; one mapping names to expressions,
;;;; one mapping names to types

(add-load-path "../lib/" :relative)

(define-module Read
  (export read-s-exprs)
  (use srfi-1)
  (use DataTypes)
  (use Util)


  (define (proper-def? def)
    (and (pair? def) (eq? (car def) '=)))

  ;;;; read-s-exprs :: [S-expr] -> ({name => expr} . {name => type})
  (define (read-s-exprs program)
    (let ([exprs
           (fold (fn [def exprs-ht]
                     (let* ([def    (if (proper-def? def) def `(= (main a) ,def))]
                            [name   (caadr def)]
                            [params (butlast (cddr def))]
                            [body   (last def)])
                       (if (null? params)
                           (hash-table-put-! exprs-ht name body)
                           (hash-table-put-! exprs-ht name `(^ ,@params ,body)))))
                 (make-hash-table 'eq?)
                 program)]

          [types
           (fold (fn [def types-ht]
                     (let* ([def  (if (proper-def? def) def `(= (main a) ,def))]
                            [name (caadr def)]
                            [type (cdadr def)])
                       (if (= 1 (length type))
                           (hash-table-put-! types-ht name (make-unknown-crt-type (car type)))
                           (hash-table-put-! types-ht name
                                             (make <crt-function-type> :type (map make-unknown-crt-type type))))))
                 (make-hash-table 'eq?)
                 program)])

      (cons exprs types))))
