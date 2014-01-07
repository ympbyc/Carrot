Carrot
======

Dec 2012 Minori Yamashita <ympbyc@gmail.com>


Status:
programs in examples/ do not work except for prelude.nadeko


```lisp
;;./Carrot.scm examples/srfi-1.nadeko

(Y (^ f (cons 1 (zipWith f + (cdr f)) -cons 1))
   -take 10
   -reverse
   -fold (compose (compose ++ (++ " : ")) num->str) "")
```

<img src="https://rawgithub.com/ympbyc/Carrot/master/docs/carrot.png" width="360px" />


Installation
------------

+ Install gauche http://practical-scheme.net/gauche/index.html
+ Clone this repository

REPL
----

### To run the VM at the bleeding edge

```
./Carrot.scm examples/srfi-1.scm
```


The language
------------

This section specifies the language Carrot.
Carrot is a powerful functional programming language designed to be extremely simple and portable.

### Comments

Characters in a line after semicolon are treated as comments

**code 0** *comments*

```
;this is a comment
"this is not a comment" ;this is a comment
```

### Primitive values **(Expression)**

Primitive values are values that evaluates to itself. They include numbers, characters, strings, symbols and so on.
Implementations must provide at least the following primitives:

+ Strings
+ Numbers
+ Symbols

**code 1** *examples of primitive values*

```lisp
"abcd"    ;string
1         ;number
'foo      ;symbol
```

### Identifiers **(Expression)**

Identifiers are symbols that are or to be bound to another value. They are used to name functions, and as parameters of functions.
An identifier is consist of one or more non-whitespace charactors. Some combinations are reserved as they are syntactic forms.

**code 2** *examples of identifiers*

```lisp
aaa
-he-lo-
<hello>
@#=~
```

Implementations SHOULD allow every character that is not used for literals to be used to construct identifiers.

### Lambda expressions **(Expression)**

Lambda expressions create closures. Closures are objects that when applied to a value return another value. They are basically, functions carrying its own environment with it.

Lambda expressions have the syntactic form
(^ *identifier ...* *expression*)
Where *identifier ...* is replaced with an arbitrary number of identifiers, and *expression* is replaced with an actual expression.
*identifier* is a parameter that gets bound to a value(actualy a thunk as we see later) when the closure is applied to arguments.

**code 3** *lambda expressions*

```lisp
(^ x (* x x)) ;;A

(^ a b c (* a (+ b c))) ;;B
```

The expression A, when applied, computes the squared value of a given number.
The expression B, when applied, performs a simple arithmetic operations on three numbers.
Note that the expression B is semantically the same as the following expression.

**code 4** *explicit currying*

```lisp
(^ a (^ b (^ c (* a (+ b c))))) ;;C
```

In fact, Carrot interprets the code B as C.
We will discuss the significance of it later.


### Definition (Statement)

The expression
(=  (*name* *type* ...)  *[identifier ...]* *expression*)
Binds the *expression* to the *name*.
If one or more parameters( *identifier ...* ) are given, they can be used in the *expression* to refer to the values the function is applied to.

Multiple functions can share a name. In which case their type must differ.

**code 5** *defining a `map` function*

```lisp
(= (map (List a) (Fn a b) (List b))
  xs f
  (nil? xs nil
        (cons (f (car xs))
              (map (cdr xs) f))))
```

Implementations SHOULD implement tail call otimization.

### Application (Expression)

Functions(closures) can be applied to arguments. The application begins with an open paren, then the function, followed by a sequence of arguments, and ends with an corresponding close paren.

**code 6** *applying `map` function to a list*

```lisp
(map listX functionX) ;;A

(map (take integers 5) (+ 2)) ;;B
```

In the expression A, the `map` function defined in **code 5** is applied to two arguments; listX and functionX assuming they are defined elsewhere.
This fills the parameter slots of the `map` function meaning that `listX` gets bound to `lst` and `functionX` gets bound to `f`. The expression inside `map` is then evaluated using both `listX` and `functionX`. Eventually the evaluation completes; producing a value(in this case a new list).

+ Carrot uses call-by-need evaluation strategy (although the implementation is incomplete).
+ Every function is curried.

Implementations SHOLD implement "call-by-need" instead of "call-by-name".


Type System
-----------

Carrot's type system isn't really a type system.  It is aimed to check if the programme's intention was consistent.  The type system is unique and somewhat criptic at first sight:

### How to Type Expressions

The syntactic form `=` gives types to identifiers.

```lisp
;; typing primitive values
(= (name String) "Rikka")

;; typing composit values
(= (names (List String)) (cons "Rikka" (cons "Kozue" (cons "Kamome" nil))))

;; typing functions
(= (second (List a) a)               ;; (<name> <argument-type>... <return-type>)
   xs                                ;; parameter...
   (car (cdr xs)))                   ;; expression
```

### Twisted Algebraic Data Type

Carrot has no data structures except for closures, yet the type system is rich enough to express something like algebraic data types. No syntax has to be introduced.

```lisp
;; Lists
(= (cons a (List a) (List a))
   x xs f (f x xs))
(= (nil (List a)) 'nil)
(= (car (List a) a) (xs true))
(= (cdr (List a) (List a)) (xs false))

;; Tuples
(= (2-tuple a b (Tuple a b)) cons)
(= (fst (Tuple a b) a) car)
(= (snd (Tuple a b) a) cdr)
```

### Things Start to Get Odd

Carrot's runtime instructions are completely **untyped** (not even dynamicaly typed). As a consequence,  Carrot's type system is completely independent from underlying implementation.  The type checker only looks at type declarations and it is enough to find most of the type-related bugs one might make.

```lisp
(= (box a (Box a))
   x x)                              ;; a boxed value's internal representation is a value itself
(= (takeout (Box a) a)
   x x)                              ;; just return what gets passed in

(takeout 7)                          ;;=> TYPE ERROR!
(takeout (box 7))                    ;;=> 7
```

### No Fool Proof

Carrot's weird type system allows the following expression to get through it.

```lisp
(= (stupid-fn Number Number)
   x "I'm supporse to be a number")

(stupid-fn 1)                        ;;=> "I'm supporse to be a number"
```

This looks pretty bad, but in practice, this kind of type bug easily gets detected because there should be a function or two that depends upon the results of the bugged function.

```lisp
;; depending on the fact that `stupid-fn` returns a string
(= (greet String String) s
   (++ "Hello, " s))
(greet (stupid-fn 0))                ;;=> TYPE ERROR!

;; wrongly assuming `stupid-fn` would return a number
(= (add2 Number Number) (+ 2))
(add2 (stupid-fn 0))                 ;;=> Bad luck. RUNTIME ERROR!
```


Almost everything is a closure
-------------------------------

Carrot implementations does not have to provide lists as primitive data type because they can easily be simulated with closures.

Similarly, booleans are closures and `if` can be implemented as a function.

S-expression without paren hell
-------------------------------

Carrot's auto-currying feature togather with lazyness gave us an unexpected gift -- Reduced use  of parentheses.

Because the arguments are delayed automaticaly, we can implement booleans as functions.

```lisp
(= (true Bool)  t e t)
(= (false Bool) t e e)

;scheme
(if (eq? a b) "equal" "not equal")

;nadeko
(=? a b "equal" "not equal")
```

We can take the full advantage of the fact that lists are implemented as functions, by applying lists to functions.

```lisp
;scheme
(fold + 0 (map (lambda (x) (* x 2)) (take (iota 100) 5)))

;nadeko
(integers -take 5
          -map  (* 2)
          -fold + 0)
```

Note this is not a syntactic feature. All these `-take`, `-map` and `-fold` are ordinary functions.
See the source of this magic in examples/srfi-1.nadeko.


Pipeline operator in F#, and Synthread in Clojure are useful tool to avoid nesting of function calls. In Carrot, `->` can be used to compose functions right to left so it reads similarly to the synthread.

```lisp
(-> 1
 -> (+ 2)
 -> (* 3)
 -> num->str
 id (flip ++ " = nine"))

;;equivalent to
(print 0 (++ (num->str (* (+ 1 2) 3)) " = nine"))
```

I/O
---

 I/O in lazy languages are hard because the order of evaluation is not lexical, and if the return value of an I/O operation does not affect the overall result, the operation itself gets ommited. To go around this problem, Haskell uses whta's called an IO Monad. IO Monad is a way to construct a computation, and sideeffects don't happen while the program is executing.
 Carrot takes a saner approach than that. It uses what we call a timed-io. Every sideeffectful function F takes an additional argument time X  and F that usually return `()` instead return a time Y. By giving Y to another sideeffectful function G, a clear relation between F and G gets formed so Carrot can figure out the evaluation order.
 Every sideeffectful function may cache its results based on the time so it works fine even with call-by-name strategy.

```lisp
(-> (print 0 "Hi! What's your name?")
 -> read
 -> (^ name  (print 1 (++ (++ "Nice to meet you, " name) ". What do you like?"))))
 -> read
 id (^ thing (print 2 (++ thing "? I like it, too!"))))
```

Macros
------

 There is no macro mechanism built in to Carrot at this stage partly because there's no need for it.


Influenced by
--------------

Carrot is influenced by the following languages

+ Haskell      - for lazyness and currying
+ Scheme       - for syntax and actors
+ Io           - for simplicity
+ Smalltalk-72 - for messaging notation
+ Clojure


CHANGELOG
---------

### 1/4/2014

Integrated a type system.

### 11/17/2013

Krivine VM is replaced by S Machine described in http://www.cs.indiana.edu/cgi-bin/techreports/TRNNN.cgi?trnum=TR581

### 9/26/2013

A huge change in syntax.

+ `(:= (fname param ...) expr)` is replaced by `(= fname param ... expr)`
+ `(:= (name) const)` is replaced by `(= name const)`
+ `(-> (param ...) expr)` is replaced by `(^ param ... expr)`
+ `=` for equality check is replaced by `=?`

All the examples and tests are up to date with this change.
