(add-load-path "../lib/" :relative)

(use gauche.test)
(test-start "Krivine")
(use Krivine)

(test-module 'Krivine)

(define default-global
  (let ([*gl-env* (make-hash-table)])
    (hash-table-put! *gl-env* 'true  `((code . ((,GRAB a) (,GRAB b) (,ACCESS a) (,CONTINUE))) (env . ())))
    (hash-table-put! *gl-env* 'false `((code . ((,GRAB a) (,GRAB b) (,ACCESS b) (,CONTINUE))) (env . ())))
    *gl-env*))

(define (K code)
  (receive (result g-env) (Krivine (append code `((,STOP))) `() `() default-global)))

(test-section "standard(closure, grab, access, continue) + constant + stop")
(test* "((-> (x) x) 'helloworld)" 
  'helloworld (K `((,CLOSURE ((,CONSTANT helloworld) (,STOP))) (,GRAB x) (,ACCESS x) (,CONTINUE))))
(test* "((-> (x y) y) 'true 'false)"
  'false (K `((,CLOSURE ((,CONSTANT false) (,STOP))) (,CLOSURE ((,CONSTANT true) (,STOP))) (,GRAB x) (,GRAB y) (,ACCESS y) (,CONTINUE))))
(test* "((-> (f) (f 1 2)) (-> (a b) a))"
  1 (K `((,CLOSURE ((,GRAB a) (,GRAB b) (,ACCESS a) (,CONTINUE))) 
      (,GRAB f) (,CLOSURE ((,CONSTANT 2) (,STOP))) (,CLOSURE ((,CONSTANT 1) (,STOP))) (,ACCESS f) (,CONTINUE))))

(test-section "define")
(test* "define 0"
  100 (K `((,CLOSURE ((,CONSTANT 100) (,STOP))) (,DEFINE foo) (,ACCESS foo) (,CONTINUE))))
(test* "define 1"
  100 (K `((,CLOSURE ((,GRAB x) (,ACCESS x) (,CONTINUE))) (,DEFINE foo) (,CLOSURE ((,CONSTANT 100) (,STOP))) (,ACCESS foo) (,CONTINUE))))
(test* "define 2"
  100 (K `((,CLOSURE ((,GRAB x) (,GRAB y) (,ACCESS y) (,CONTINUE))) (,DEFINE foo) (,CLOSURE ((,CONSTANT 100) (,STOP))) (,CLOSURE ((,CONSTANT 200) (,STOP))) (,ACCESS foo) (,CONTINUE))))

(test-section "primitive")
(test* "primitive +"
  100 (K `((,PRIMITIVE + ((,CONSTANT 60) (,STOP)) ((,CONSTANT 40) (,STOP))) (,STOP))))
(test* "primitive -"
  20 (K `((,PRIMITIVE - ((,CONSTANT 60) (,STOP)) ((,CONSTANT 40) (,STOP))) (,STOP))))
(test* "primitive *"
  24 (K `((,PRIMITIVE * ((,CONSTANT 6) (,STOP)) ((,CONSTANT 4) (,STOP))) (,STOP))))
(test* "primitive /"
  10 (K `((,PRIMITIVE / ((,CONSTANT 60) (,STOP)) ((,CONSTANT 6) (,STOP))) (,STOP))))
(test* "primitive ++"
  "Hello, world!" (K `((,PRIMITIVE ++ ((,CONSTANT "Hello, ") (,STOP)) ((,CONSTANT "world!") (,STOP))) (,STOP))))
(test* "primitive equal"
  'true (K `((,CLOSURE ((,CONSTANT false) (,STOP))) (,CLOSURE ((,CONSTANT true) (,STOP))) 
    (,PRIMITIVE equal ((,CONSTANT 60) (,STOP)) ((,CONSTANT 60) (,STOP))) (,STOP))))
(test* "primitive num->str"
  "100" (K `((,PRIMITIVE num->str ((,CONSTANT 100) (,STOP))) (,STOP))))
(test* "primitive number? 1"
  'true (K `((,CLOSURE ((,CONSTANT false) (,STOP))) (,CLOSURE ((,CONSTANT true) (,STOP))) 
    (,PRIMITIVE number? ((,CONSTANT 60) (,STOP))))))
(test* "primitive number? 2"
  'false (K `((,CLOSURE ((,CONSTANT false) (,STOP))) (,CLOSURE ((,CONSTANT true) (,STOP))) 
    (,PRIMITIVE number? ((,CONSTANT "60") (,STOP))))))
(test* "primitive string? 1"
  'true (K `((,CLOSURE ((,CONSTANT false) (,STOP))) (,CLOSURE ((,CONSTANT true) (,STOP))) 
    (,PRIMITIVE string? ((,CONSTANT "hello") (,STOP))))))
(test* "primitive string? 2"
  'false (K `((,CLOSURE ((,CONSTANT false) (,STOP))) (,CLOSURE ((,CONSTANT true) (,STOP))) 
    (,PRIMITIVE string? ((,CONSTANT 1234) (,STOP))))))

(test-end :exit-on-failure #t)
