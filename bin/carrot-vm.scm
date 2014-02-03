#!/usr/local/bin/gosh

(add-load-path "../lib/" :relative)
(add-load-path "/usr/local/share/Carrot/2.2.0/lib/" :absolute)

(use DataTypes)
(use CarrotVM)

(define message "Executing     your carrot on CarrotVM

;=> ")

(define (main args)
  (let* ([exprs-ht (read)])
    (format (standard-error-port) message)
    (print
     (CarrotVM exprs-ht 'main))))
