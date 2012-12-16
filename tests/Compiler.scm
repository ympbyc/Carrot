(add-load-path "../lib/" :relative)

(use gauche.test)
(test-start "K-Compiler")
(use K-Compiler)

(test-module 'K-Compiler)

(test-section "constant")
(test* "constant 1" `((,CLOSURE ((,STOP))) (,CONSTANT 2) (,CONTINUE)) (compile `(2)))
(test* "constant 2" `((,CLOSURE ((,STOP))) (,CONSTANT "hello") (,CONTINUE)) (compile `("hello")))
(test* "constant 3" `((,CLOSURE ((,STOP))) (,CONSTANT 'atom) (,CONTINUE)) (compile `('atom)))

(test-section "variable")
(test* "variable 1" `((,CLOSURE ((,STOP))) (,ACCESS x) (,CONTINUE)) (compile `(x)))

(test-section "primitive")
(test* "primitive 0" `((,CLOSURE ((,STOP))) (,PMARK) (,CLOSURE ((,PRIMITIVE foo) (,CONTINUE))) (,CONTINUE)) (compile `((** foo))))
(test* "primitive 1" `((,CLOSURE ((,STOP))) (,PMARK) (,CLOSURE ((,PRIMITIVE one) (,CONTINUE))) (,CONSTANT 1) (,CONTINUE)) (compile `((** one 1))))
(test* "primitive 2" `((,CLOSURE ((,STOP))) (,PMARK) (,CLOSURE ((,PRIMITIVE two) (,CONTINUE))) (,CONSTANT 2) (,CONSTANT 1) (,CONTINUE)) (compile `((** two 1 2))))

(test-section "define")
(test* "define 0" `((,CLOSURE ((,STOP))) (,CLOSURE ((,CONSTANT 5) (,CONTINUE))) (,DEFINE five) (,CONTINUE)) (compile `((:= (five) 5))))
(test* "define 1" `((,CLOSURE ((,STOP))) (,CLOSURE ((,GRAB x) (,ACCESS x) (,CONTINUE))) (,DEFINE foo) (,CONTINUE)) (compile `((:= (foo x) x))))
(test* "define 2" `((,CLOSURE ((,STOP))) (,CLOSURE ((,GRAB x) (,GRAB y) (,ACCESS y) (,CONTINUE))) (,DEFINE foo) (,CONTINUE)) (compile `((:= (foo x y) y))))

(test-section "lambda")
(test* "lambda 1" `((,CLOSURE ((,STOP))) (,GRAB x) (,ACCESS x) (,CONTINUE)) (compile `((-> (x) x))))
(test* "lambda 1 1" `((,CLOSURE ((,STOP))) (,GRAB x) (,CONSTANT 5) (,CONTINUE)) (compile `((-> (x) 5))))
(test* "lambda 2" `((,CLOSURE ((,STOP))) (,GRAB x) (,GRAB y) (,ACCESS x) (,CONTINUE)) (compile `((-> (x y) x))))
(test* "lambda 3" `((,CLOSURE ((,STOP))) (,GRAB x) (,GRAB y) (,GRAB z) (,ACCESS y) (,CONTINUE)) (compile `((-> (x y z) y))))

(test-section "application")
(test* "app 1" `((,CLOSURE ((,STOP))) (,CLOSURE ((,CONSTANT 5) (,CONTINUE))) (,ACCESS id) (,CONTINUE)) (compile `((id 5))))
(test* "app 1 1" `((,CLOSURE ((,STOP))) (,CLOSURE ((,ACCESS x) (,CONTINUE))) (,ACCESS foo) (,CONTINUE)) (compile `((foo x))))
(test* "app 2" 
  `((,CLOSURE ((,STOP))) (,CLOSURE ((,CONSTANT 5) (,CONTINUE))) (,CLOSURE ((,CONSTANT 6) (,CONTINUE))) (,ACCESS foo) (,CONTINUE)) (compile `((foo 6 5))))
(test* "app 3" 
  `((,CLOSURE ((,STOP))) (,CLOSURE ((,CONSTANT 5) (,CONTINUE))) (,CLOSURE ((,CONSTANT 6) (,CONTINUE))) (,CLOSURE ((,CONSTANT 7) (,CONTINUE)))
    (,ACCESS foo) (,CONTINUE)) (compile `((foo 7 6 5))))
(test* "app-lambda 1"
  `((,CLOSURE ((,STOP))) (,CLOSURE ((,CONSTANT 5) (,CONTINUE))) (,GRAB x) (,ACCESS x) (,CONTINUE)) (compile `(((-> (x) x) 5))))
(test* "app-lambda 2"
  `((,CLOSURE ((,STOP))) (,CLOSURE ((,CONSTANT 5) (,CONTINUE))) (,CLOSURE ((,CONSTANT 6) (,CONTINUE))) (,GRAB x) (,GRAB y) (,ACCESS x) (,CONTINUE)) (compile `(((-> (x y) x) 6 5))))
(test* "partial"
  `((,CLOSURE ((,STOP))) (,CLOSURE ((,CONSTANT 5) (,CONTINUE))) (,CLOSURE ((,CONSTANT 6) (,CONTINUE))) (,GRAB x) (,GRAB y) (,ACCESS x) (,CONTINUE)) (compile `((((-> (x y) x) 6) 5))))
(test* "higher-oeder"
  `((,CLOSURE ((,STOP))) (,CLOSURE ((,GRAB x) (,GRAB y) (,ACCESS y) (,CONTINUE)))
    (,GRAB f) (,CLOSURE ((,CONSTANT 5) (,CONTINUE))) (,CLOSURE ((,CONSTANT 6) (,CONTINUE))) (,ACCESS f) (,CONTINUE))
  (compile `(((-> (f) (f 6 5)) (-> (x y) y)))))

(test-end :exit-on-failure #t)