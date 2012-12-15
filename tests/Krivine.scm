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
  (receive (result g-env) (Krivine (append code `((,STOP))) default-global)))

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
  100 (K `((,CONSTANT 60) (,CONSTANT 40) (,PRIMITIVE +) (,STOP))))

(test-section "compound")
(test* "((-> (x) (+ x 60)) 40)"
  100 (K `(
    (,CLOSURE ((,STOP))) ;always here
    (,CLOSURE ((,GRAB cont) (,PRIMITIVE +) (,ACCESS cont) (,CONTINUE))) ;this is the + "function" that wraps the + "primitive"
    (,DEFINE +)
    (,CLOSURE ((,CONSTANT 40) (,CONTINUE))) ;simple arguments always have CONTINUE with no preceeding ACCESS
    (,GRAB x)
    (,CONSTANT 60)
    (,ACCESS +) ;this is kept on the stack until the invocation of CONTINUE of the argument
    (,ACCESS x)
    (,CONTINUE))))

(test-end :exit-on-failure #t)
