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

;((-> (a b (** + a b))) 5 6)
(
  (,CLOSURE ((,STOP)))
  (,CLOSURE (
    (,GRAB a)
    (,GRAB b)
    (,PMARK)
    (,CLOSURE ((,PRIMITIVE +) (,CONTINUE)))
    (,ACCESS a)
    (,ACCESS b)
    (,CONTINUE)))
  (,DEFINE +)
  (,CLOSURE ((,CONSTANT 5) (,CONTINUE)))
  (,CLOSURE ((,CONSTANT 6) (,CONTINUE)))
  (,ACCESS +)
  (,CONTINUE)
)


;(
;  (:= (+ a b) (** + a b))
;  (:= (x) 7)
;  (+ x 6)
;)
(
  (,CLOSURE ((,STOP)))
  (,CLOSURE (
    (,GRAB a)
    (,GRAB b)
    (,PMARK)
    (,CLOSURE ((,PRIMITIVE +) (,CONTINUE)))
    (,ACCESS a)
    (,ACCESS b)
    (,CONTINUE)))
  (,DEFINE +)
  (,CLOSURE ((,CONSTANT 7) (,CONTINUE)))
  (,DEFINE x)
  (,CLOSURE ((,ACCESS x) (,CONTINUE)))
  (,CLOSURE ((,CONSTANT 6) (,CONTINUE)))
  (,ACCESS +)
  (,CONTINUE)
  )







x, y, z ∈ Var
M, N ∈ Exp
Exp := x | (M N) | λx.M


Krivine: State -> State
State = Clos * Stack
Env = Var -> Clos
Clos = Exp * Env
Stack = [Clos]


VAR Rule
--------

(defliteral (Clos x y)
  {x y})

(define (Krivine closure stack)
  (case (closure stack)
    [({x Env}, Stack)
     (Krivine (get Env x) Stack)]

    [({(M N) Env}, Stack)
     (Krivine {M Env} (cons {N Env} Stack))]

    [({(^ x M) Env} (c . Stack))
     (Krivine {M (cons (x . c) Env)} Stack)]))
