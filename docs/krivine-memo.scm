;;;
;; N[n] = ACCESS(n); CONTINUE
;; N[λa] = GRAB; N[a]
;; N[b a] = CLOSURE(N[a]); N[b]
;;
;; M := x | M_1 M_2 | λx.M
;; (M N, S, E)     -> (M, (S,(N,E)), E)
;; (λM, (S,N), E)  -> (M, S, (E,N))
;; (i+1, S, (E,N)) -> (i, S, E)
;; (0, S, (E_1 (M, E_2))) -> (M, S, E_2)
;;To evaluate an application M N, the K-machine builds a closure made of the argument N
;;and the current environment E in the stack and proceeds with the reduction of the function
;;M. This is the ﬁrst characteristic of the K-machine: a closure is built in constant time and includes the complete current environment.
;;The evaluation of a λ-abstraction places the argument (the stack’s top element) in the environment and proceeds with the body of the function. This is the second and more important characteristic of the K-machine: it strives not to build closures for functions. Other
;;abstract machines return functions as closures before applying them.
;;The evaluation of a variable i amounts to following i links to ﬁnd the corresponding closure in the environment. The closure’s components become the current code and environment.
;;;

;;; experiment ;;;
;; (((λx.λy.y) 5) 6)   CLOSURE
;;                6      CONSTANT 6; STOP
;;  ((λx.λy.y) 5)      CLOSURE
;;             5         CONSTANT 5; STOP
;;   (λx.λy.y)         GRAB x
;;       λy.y          GRAB y
;;          y          ACCESS y; CONTINUE

;;Normal function application
;(print (Krivine `(
;  (,CLOSURE (
;    (,CONSTANT 6)
;    (,STOP)))
;  (,CLOSURE (
;    (,CONSTANT 5)
;    (,STOP)))
;  (,GRAB x)
;  (,GRAB y)
;  (,ACCESS y)
;  (,CONTINUE)
;) '() '() '()))

;;Closures
;(print (Krivine `(
;  (,CLOSURE (
;    (,GRAB x)
;    (,ACCESS x)
;    (,CONTINUE))) ;;普通の意味のクロージャ
;  (,DEFINE my-closure)
;  (,CLOSURE (
;    (,CONSTANT 5)
;    (,STOP))) ;;Thunk
;  (,ACCESS my-closure)
;  (,CONTINUE)
;) '() '() '()))