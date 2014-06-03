#!/usr/local/bin/gosh

;;;; Carrot ;;;;
;;; 2012 Minori Yamashita <ympbyc@gmail.com> ;;add your name here

(add-load-path "./lib/" :relative)
(add-load-path "./compilers/" :relative)

(use Util)
(use to-carrot-vm)
(use CarrotVM)
(use Type)
(use Read)
(use DataTypes)
(use gauche.parseopt)

;;; REPL ;;;
(define (REPL exprs*types*genmap ctr)
  ;;(p (hash-table->alist (caddr exprs*types*genmap)))
  (format #t "carrot ~S> " ctr)
  (flush)
  (let* ([expr  (read)] [_ (print expr)]
         [res (read-s-exprs (list expr) exprs*types*genmap)]
         [exprs-ht (fst res)]
         [types-ht (snd res)]
         [genmap   (thd res)]
         [checked-p*t  (acquire-checked-program res)]
         [checked-p (fst checked-p*t)]
         [main-t    (snd checked-p*t)])
    (unless checked-p
            (print "Skipping execution due to one or more type errors _(′︿‵｡_)")
            (hash-table-delete! exprs-ht (get-main-name genmap))
            (hash-table-delete! types-ht (get-main-name genmap))
            (hash-table-delete! genmap 'main)
            (REPL res (+ ctr 1)))
    (format #t "      ;=> ~A :: ~S\n\n"
            (fmt (CarrotVM (compile checked-p) (get-main-name genmap)))
            (type->data main-t))
    (hash-table-delete! exprs-ht (get-main-name genmap))
    (hash-table-delete! types-ht (get-main-name genmap))
    (hash-table-delete! genmap 'main)
    (REPL res (inc ctr))))  ;loop with new global-environment

(define banner
"             ----------------------
             |    CARROT 2.2.0    |
             ----------------------
         https://github.com/ympbyc/Carrot\n")

(define (main args)
  (print banner)
  (format #t "Loading ~S ... done\n" (cons "examples/prelude.nadeko" (cdr args)))
  (let* ([fnames (cdr args)]
         [exprs*types*genmap (triple (make-hash-table 'eq?)
                                     (make-hash-table 'eq?)
                                     (make-hash-table 'eq?))]
         [exprs*types*genmap
          (fold (fn [fname exprs*types*genmap]
                    (load-file fname exprs*types*genmap))
                exprs*types*genmap
                fnames)])
    (print (sort (map symbol->string (hash-table-keys (thd exprs*types*genmap)))))
    (newline)
    (REPL exprs*types*genmap 0)))

;;string * {types} -> ({types} . typed-expr)
(define (load-file fname exprs*types*genmap)
  (call-with-input-file fname
    (fn [file-port]
        (let* ([exprs*types*genmap
                (read-s-exprs (read-list file-port)
                              exprs*types*genmap)]
               ;;[checks? (type-check exprs*types*genmap)]
               )
          (acquire-checked-program exprs*types*genmap)
          exprs*types*genmap))))

(define (read-list port)
  (let ((exp (read port)))
    (if (eof-object? exp) '()
        (cons exp (read-list port)))))

;; avoid printing closures
(define (fmt data)
  (if (is-a? data <nadeko-closure>)
      (format "#,(function ~A)" (name data))
      data))
