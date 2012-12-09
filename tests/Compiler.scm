(add-load-path "../lib/" :relative)

(use gauche.test)
(test-start "Compiler")
(use Compiler)

(test-module 'Compiler)

(test-section "constant")
(test* "constant 1" `((,CONSTANT 2) (,STOP)) (compile `(2)))
(test* "constant 2" `((,CONSTANT "hello") (,STOP)) (compile `("hello")))
(test* "constant 3" `((,CONSTANT atom) (,STOP)) (compile `('atom)))

(test-section "variable")
(test* "variable 1" `((,ACCESS x) (,CONTINUE)) (compile `(x)))

(test-section "primitive")
(test* "primitive 0" `((,PRIMITIVE foo) (,STOP)) (compile `((** foo))))
(test* "primitive 1" `((,PRIMITIVE one ((,CONSTANT 1) (,STOP))) (,STOP)) (compile `((** one 1))))
(test* "primitive 2" `((,PRIMITIVE two ((,CONSTANT 1) (,STOP)) ((,CONSTANT 2) (,STOP))) (,STOP)) (compile `((** two 1 2))))

(test-section "define")
(test* "define 0" `((,CLOSURE ((,CONSTANT 5) (,STOP))) (,DEFINE five) (,STOP)) (compile `((:= (five) 5))))
(test* "define 1" `((,CLOSURE ((,GRAB x) (,ACCESS x) (,CONTINUE))) (,DEFINE foo) (,STOP)) (compile `((:= (foo x) x))))
(test* "define 2" `((,CLOSURE ((,GRAB x) (,GRAB y) (,ACCESS y) (,CONTINUE))) (,DEFINE foo) (,STOP)) (compile `((:= (foo x y) y))))

(test-section "lambda")
(test* "lambda 1" `((,GRAB x) (,ACCESS x) (,CONTINUE)) (compile `((-> (x) x))))
(test* "lambda 1 1" `((,GRAB x) (,CONSTANT 5) (,STOP)) (compile `((-> (x) 5))))
(test* "lambda 2" `((,GRAB x) (,GRAB y) (,ACCESS x) (,CONTINUE)) (compile `((-> (x y) x))))
(test* "lambda 3" `((,GRAB x) (,GRAB y) (,GRAB z) (,ACCESS y) (,CONTINUE)) (compile `((-> (x y z) y))))

(test-section "application")
(test* "app 1" `((,CLOSURE ((,CONSTANT 5) (,STOP))) (,ACCESS id) (,CONTINUE)) (compile `((id 5))))
(test* "app 1 1" `((,CLOSURE ((,ACCESS x) (,CONTINUE))) (,ACCESS foo) (,CONTINUE)) (compile `((foo x))))
(test* "app 2" `((,CLOSURE ((,CONSTANT 5) (,STOP))) (,CLOSURE ((,CONSTANT 6) (,STOP))) (,ACCESS foo) (,CONTINUE)) (compile `((foo 6 5))))
(test* "app 3" `((,CLOSURE ((,CONSTANT 5) (,STOP))) (,CLOSURE ((,CONSTANT 6) (,STOP))) (,CLOSURE ((,CONSTANT 7) (,STOP))) (,ACCESS foo) (,CONTINUE)) (compile `((foo 7 6 5))))
(test* "app-lambda 1"
  `((,CLOSURE ((,CONSTANT 5) (,STOP))) (,GRAB x) (,ACCESS x) (,CONTINUE)) (compile `(((-> (x) x) 5))))
(test* "app-lambda 2"
  `((,CLOSURE ((,CONSTANT 5) (,STOP))) (,CLOSURE ((,CONSTANT 6) (,STOP))) (,GRAB x) (,GRAB y) (,ACCESS x) (,CONTINUE)) (compile `(((-> (x y) x) 6 5))))
(test* "partial"
  `((,CLOSURE ((,CONSTANT 5) (,STOP))) (,CLOSURE ((,CONSTANT 6) (,STOP))) (,GRAB x) (,GRAB y) (,ACCESS x) (,CONTINUE)) (compile `((((-> (x y) x) 6) 5))))
(test* "higher-oeder"
  `((,CLOSURE ((,GRAB x) (,GRAB y) (,ACCESS y) (,CONTINUE))) (,GRAB f) (,CLOSURE ((,CONSTANT 5) (,STOP))) (,CLOSURE ((,CONSTANT 6) (,STOP))) (,ACCESS f) (,CONTINUE))
  (compile `(((-> (f) (f 6 5)) (-> (x y) y)))))

(test-end :exit-on-failure #t)