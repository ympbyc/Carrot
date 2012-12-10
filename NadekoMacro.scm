;;; Nadeko in scheme ;;;

(define-syntax ->
  (syntax-rules ()
    ((_ (arg0) expr)
      (lambda (arg0) expr))
    ((_ (arg0 arg1 ...) expr)
      (lambda (arg0 . rest)
        (let ((applied (-> (arg1 ...) expr)))
          (if (null? rest) applied
            (apply applied rest)))))))

(define-syntax =
  (syntax-rules ()
    ((_ (name a) expr)
      (define (name a) expr))
    ((_ (name a ...) expr)
     (define name (-> (a ...) expr)))))

