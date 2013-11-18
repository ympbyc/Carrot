Nadeko
======

Dec 2012 Minori Yamashita <ympbyc@gmail.com>


```lisp
(Y (^ f (cons 1 (zipWith f + (cdr f)) -cons 1))
  -take 10
  -reverse
  -fold (compose (compose ++ (++ ",")) num->str) "")
```


Installation
------------

+ Install gauche http://practical-scheme.net/gauche/index.html
+ Clone this repository

REPL
----

### To run the VM at the bleeding edge

```
./Nadeko.scm -l=examples/srfi-1.scm
```


The language
------------

This section specifies the language Nadeko.
Nadeko is a powerful functional programming language designed to be extremely simple and portable.

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
An identifier consists of one or more non-whitespace charactors. Some combinations are reserved as they are syntactic forms.

**code 2** *examples of identifiers*

```lisp
aaa
-he-lo-
!#$%&-=^~|@{[]}+:*<>,_
```

Implementations SHOULD allow every character not used for literals to be used to construct identifiers.

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

In fact, Nadeko interprets the code B as C.
We will discuss the significance of it later.


### Definition (Statement)

The expression
(=  *name* *[identifier ...]* *expression*)
Binds the *expression* to the *name*.
If one or more parameters( *identifier ...* ) are given, they can be used in the *expression* to refer to the values the function is applied to.
(=  *name* *identifier ...* *expression*) is interpreted the same as
(= *name* (^ *identifier ...* *expression*)).

**code 5** *defining a `map` function*

```lisp
(= map lst f
  (nil? lst
        nil
        (cons (f (car lst))
              (map (cdr lst) f))))
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

The expression B is a bit more complicated and it introduces some important features. Lets look into it in detail.

**text 1** *reduction proccess*

> 1. First, the expression `(take integers 5)` is passed to the `map` along with the other expression `(+ 2)`.
> > Now the `lst` parameter of `map` is bound to `(take integers 5)`
> > and the `f` parameter of `map` is bound to `(+ 2)`
>
> 2. Next, `lst` is passed to the function `nil?`. `nil?` before evaluating itself, evaluates the expression bound to `lst` that is `(take integers 5)`.
> > `integers` is a infinite list of integers.
> > `take` is a function that take n number of items from a list
> > so the resulting value of `(take integers 5)` is a list containing 0, 1, 2, 3, and 4
>
> 3. `nil?` starts evaluating with the list we obtained in **Step 2**
> > `nil?` is a function to test if the first argument given is `nil`
> > since our list of integers are not `nil`, `nil?` returns `false`
>
> 4. `false` is applied to `nil` and `(cons (f ...`
> > `false` is a function that take two arguments, ignoring the first argument and return the second argument.
> > So we get `(cons (f ...`
>
> 5. `cons` gets two arguments; `(f (car lst))` and `(map (cdr ...`. `cons`, in order to produce the value of itself, first have to evaluate the expression `f` which is bound to `(+ 2)`
> > `+` is a function that take **two** arguments and returns the addition of those two values.
> > However `+` is applied to only **one** argument which is **2**.
> > The expression `(+ 2)` returns a function that  take **one** argument and add it to **2**.
> > That function is applied to `(car lst)`. `(car lst)` gets evaluated and produces 0 --- from **Step 2**
> > `(+ 2 0)` gets evaluated and produces `2`
>
> 6. evaluation continues

The proccess showed two important evaluation strategy Nadeko uses. Lazy evaluation and Currying.

The evaluation strategy that delays the evaluation of expressions until they are needed is called **lazy evaluation**. We have seen this in **Step 2**.
Scheme and most of other dialects of LISP use `call-by-value` strategy. Under such language, both `(take integers 5)` and `(+ 2)` are computed in **Step 1** before it gets applied to `map`.
Lazy evaluation allows us to handle infinit data easily as we saw in the example of `(take integers 5)`.

Another important idea we saw is **Currying**. `(+ 2)` inspite of shortage of arguments it did not cause an error. Instead it returned a closure that take an argument and + it to 2. This is made possible thanks to currying. As I mentioned in the **lambda expression** section, a function that take more than one arguments is implicitly turned into a function that "take one argument and returns a function that take another argument". Such function is called a *Curried function*.

Implementations SHOLD implement "call-by-need" instead of "call-by-name".


Almost everything is a closure
-------------------------------

Nadeko implementations does not have to provide lists as primitive data type because they can easily be simulated with closures.

Similarly, booleans are closures and `if` can be implemented as a function.

This does not necessarily mean inconvinience. We will see why in the next chapter.

S-expression without paren hell
-------------------------------

Nadeko's auto-currying feature togather with lazyness gave us an unexpected gift -- Reducutions of parentheses.

Because the arguments are delayed automaticaly, we can implement booleans as functions.

```lisp
(= true t e t)
(= false t e e)

;scheme
(if (eq? a b) "equal" "not equal")

;nadeko
(eq? a b "equal" "not equal")
```

Invention of suffix functions is mind-blowingly awesome.

```lisp
;scheme
(fold + 0 (map (lambda (x) (* x 2)) (take (iota 100) 5)))

;nadeko
(integers -take 5 -map (* 2) -fold + 0)
```

Note this is not a syntactic feature. All these `-take`, `-map` and `-fold` are ordinary functions.
See the source of this magic in examples/srfi-1.nadeko.

Influenced by
--------------

Nadeko is influenced by the following languages

+ Haskell      - for lazyness and currying
+ Scheme       - for syntax and actors
+ Io           - for simplicity
+ Smalltalk-72 - for messaging notation


CHANGELOG
---------

### 9/26/2013

A huge change in syntax.

+ `(:= (fname param ...) expr)` is replaced by `(= fname param ... expr)`
+ `(:= (name) const)` is replaced by `(= name const)`
+ `(-> (param ...) expr)` is replaced by `(^ param ... expr)`
+ `=` for equality check is replaced by `=?`

All the examples and tests are up to date with this change.
