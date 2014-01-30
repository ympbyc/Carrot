#!/usr/local/bin/gosh

(add-load-path "../lib/" :relative)
(add-load-path "/usr/local/share/Carrot/2.2.0/lib/" :absolute)

(use DataTypes)
(use Read)

(define banner
  "
             ----------------------
             |    CARROT 2.2.0    |
             ----------------------
         https://github.com/ympbyc/Carrot

Reading       your carrot...
")

(define (read-list port)
  (let ((exp (read port)))
    (if (eof-object? exp) '()
        (cons exp (read-list port)))))

(define (main _)
  (let* ([program (read-list (standard-input-port))]
         [_ (format (standard-error-port) banner)]
         [exprs*types*genmap
          (read-s-exprs* program
                         (make-hash-table 'eq?)
                         (make-hash-table 'eq?)
                         (make-hash-table 'eq?))])
      (display
       (triple (write-hash-table (fst exprs*types*genmap))
               (write-hash-table (snd exprs*types*genmap))
               (write-hash-table (thd exprs*types*genmap))))))
