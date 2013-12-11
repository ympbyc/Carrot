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
(define (REPL binding types ctr)
  (format #t "carrot ~S> " ctr)
  (flush)
  (let* ([expr    (read)]
         [types-  (type-check (list expr) types)]
         [types   (if types- types- types)])
    (if types-
        (let* ([new-binding (compile (list expr))]
               [new-binding (hash-table-union! binding new-binding)]
               [result  (Krivine new-binding)])
          (print result)
          (REPL new-binding types (+ ctr 1)))
        (begin (print "Type error")
               (REPL binding types (+ ctr 1))))))  ;loop with new global-environment

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
    (REPL (fold (fn [fname binding] (hash-table-union! binding (pre-load fname)))
                (make-hash-table 'eq?)
                fnames)
          (fold (fn [fname types] (t-check fname types))
                (make-hash-table 'eq?)
                fnames)
          0)))

(define (t-check fname types)
  (call-with-input-file fname
    (fn [file-port]
        (type-check (read-list file-port) types))))


(define (pre-load fname)
  (call-with-input-file fname
    (lambda (file-port)
      (compile (read-list file-port)))))

(define (read-list port)
  (let ((exp (read port)))
    (if (eof-object? exp) '()
      (cons exp (read-list port)))))
