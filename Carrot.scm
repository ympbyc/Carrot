#!/usr/local/bin/gosh

;;;; Carrot ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here

(add-load-path "./lib/" :relative)

(use Util)
(use K-Compiler)
(use Krivine)
(use Check)
(use gauche.parseopt)

;;; REPL ;;;
(define (REPL types ctr)
  (format #t "carrot ~S> " ctr)
  (flush)
  (let* ([expr  (read)]
         [res   (type-program (list expr) types)]
         [types (car res)]
         [expr  (cdr res)]
         [result  (Krivine (compile expr types))])
    (print result)
    (REPL types (+ ctr 1))))  ;loop with new global-environment

(define banner
"             ----------------------
             |    CARROT 2.1.2    |
             ----------------------
         https://github.com/ympbyc/Carrot\n")

(define (main args)
  (print banner)
  (format #t "Loading ~S ... done\n" (cdr args))
  (print "type `help` to get started")
  (load "standard-macros.scm")
  (let ([fnames (cons "examples/prelude.nadeko" (cdr args))])
    (REPL (fold (fn [fname types]
                    (let* ([res (load-file fname types)])
                      (hash-table-union! types (car res))))
                (make-hash-table 'eq?)
                fnames)
          0)))

;;string * {types} -> ({types} . typed-expr)
(define (load-file fname types)
  (call-with-input-file fname
    (fn [file-port]
        (let1 res (type-program (read-list file-port) types)
              (cons (car res) (compile (cdr res) (car res)))))))

(define (read-list port)
  (let ((exp (read port)))
    (if (eof-object? exp) '()
      (cons exp (read-list port)))))
