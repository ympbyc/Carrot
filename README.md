Nadeko
======

Dec 2012 Minori Yamashita <ympbyc@gmail.com>


```lisp
(integers -take 5 -map (* 2) -fold + 0)
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
Nadeko does not specify what primitive values have to be provided by the implementation nor the literal notation of them.

**code 1** *examples of primitive values*

```lisp
"abcd"
1
'foo
```

### Identifiers **(Expression)**

Identifiers are symbols that are or to be bound to another value. They are used to name functions, and as a parameter of functions.
An identifier consists of one or more non-whitespace charactors. Some combinations are reserved as they are syntactic forms.

**code 2** *examples of identifiers*

```lisp
aaa
-he-lo-
!#$%&-=^~|@{[]};+:*<>,_
```

Implementations SHOULD allow every character not used for literals to be used to construct identifiers.

### Lambda expressions **(Expression)**

Lambda expressions create closures. Closures are objects that when applied to a value return another value. They are basically, functions carrying its own environment with it.

Lambda expressions have the syntactic form  
(-> ( *identifier ...* ) *expression*)  
Where *identifier ...* is replaced with an arbitrary number of identifiers, and *expression* is replaced with an actual expression.  
*identifier* is a parameter that gets bound to a value(actualy a thunk as we see later) when the closure is applied to arguments.

**code 3** *lambda expressions*

```lisp
(-> (x) (* x x)) ;;A

(-> (a b c) (* a (+ b c))) ;;B
```

The expression A, when applied, computes the squared value of a given number.  
The expression B, when applied, performs a simple alithmetic operations on three numbers.  
Note that the expression B is semantically the same as the following expression.

**code 4** *explicit currying*

```lisp
(-> (a) (-> (b) (-> (c) (* a (+ b c))))) ;;C
```

In fact, Nadeko interprets the code B as C.  
We will discuss the significance of it later.


### Definition (Statement)

The expression  
(:= (*name* *[identifier ...]*) *expression*)  
Binds the *expression* to the *name*.  
If one or more parameters( *identifier ...* ) are given, they can be used in the *expression* to refer to the values the function is applied to.  
(:= ( *name* *identifier ...*) *expression*) is interpreted the same as
(:= ( *name*) (-> ( *identifier ...*) *expression*)).

**code 5** *defining a `map` function*

```lisp
(:= (map lst f)
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

The expression B is a bit more complicated and it introduces some important feature. Lets look into it in detail.

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
> 5. `cons` gets two arguments; `(f (car lst))` and `(map (cdr ...`. `cons` in order to produce the value of itself, evaluates the expression `f` which is bound to `(+ 2)`
> > `+` is a function that take **two** arguments and returns the addition of those two values.
> > However `+` is applied to only **one** argument which is **2**.
> > The expression `(+ 2)` returns a function that  take **one** argument and add it to **2**.
> > That function is applied to `(car lst)`. `(car lst)` gets evaluated and produces 0 --- from **Step 2**
> > `(+ 2 0)` gets evaluated and produces `2`
>
> 6. evaluation continues

The proccess showed two important evaluation strategy Nadeko uses. Lazy evaluation and Currying.  

The evaluation strategy that delays the evaluation of expressions until they are needed is called **lazy evaluation**. We have seen this in **Step 2**.  
In Scheme and most of other dialects of LISP use `call-by-value` strategy. Under such language, both `(take integers 5)` and `(+ 2)` are computed in **Step 1** before it gets applied to `map`.  
Lazy evaluation allows us to handle infinit data easily as we saw in the example of `(take integers 5)`.

Another important idea we saw is **Currying**. `(+ 2)` inspite of shortage of arguments it did not cause an error. Instead it returned a closure that take an argument and + it to 2. This is made possible thanks to currying. As I mentioned in the **lambda expression** section, a function that take more than one arguments is implicitly turned into a function that "take one argument and returns a function that take another argument". Such function is called a *Curried function*.

Implementations SHOLD implement "call-by-need" instead of "call-by-name".

<br /> 
<br /> 
<br /> 
<br /> 
<br /> 

Sections below this line talks about the internal mechanism of the implementation written by Minori Yamashita. The internal mechanism may differ significantly between implementations as long as it maintain the specified external behaviour. 

VM - SECD Machine
-----------------

Nadeko uses a custum implementation of the SECD virtual machine. SECD machine is a VM designed specifically to host functional programming languages and well suits Nadeko.

### Stacks

SECD machine has four stacks. S, E, C, and D.

S - Stack       - is used as a workspace for the current computation.  
E - Environment - is used to store the current local environment.  
C - Code        - holds the pending instructions to be computed.  
D - Dump        - is used as a call frame. when applying a function, we store the current S, E, C to Dump.  

Nadeko adds one slot G which maintains the global environment.

### Instructions

#### ldc - stack-constant
Cons the argument onto the stack.

```
(S E ((_ const) . C) D G)
((const . S) E C D G)
```

#### ld - ref-arg
Cons the bound value of the argument onto the stack.

```
(S E ((_ var) . C) D G)
(((lookup var) . S) E C D G)
```

#### ldf - stack-closure
Creates a closure from its parameter name, code, and the current E.
Cons it onto the stack.

```
(S E ((_ param code) . C) D)
(((closure param code E) . S) E C D)
```

#### ap - app
Apply the closure at (car S) to the argument at (cadr S)
Dump current (cddr S), E, C. Empty S. Cons param-arg pair onto E. let C be the code inside the closure.

```
(((closure param code env) . arg . S) E ((_) . D) G)
(() ((param . arg) . env) code ((S E C) . D) G)
```

#### ret - restore
Restore S, E, C from D. cons previous (car S) onto S.

```
((retVal . S) E ((_) . C) ((s e c) . D) G)
((retVal . s) e c D G)
```

#### def - def
Cons the pair of the argument and (car S) onto G

```
((val . S) E ((_ symbol) . C) D G)
(S E C D ((symbol val) . G))
```

#### freeze - freeze
Creates a thunk and cons it onto the stack.

```
(S E ((_ code) . C) D G)
(((thunk code E) . S) E C D G)
```

#### thaw - thaw
Evaluates the code inside the thunk in its environment.
Creates a call frame like app.

```
(((thunk code env) . S) E ((_) . C) D G)
(() env code ((S E C) . D) G)
```

#### stop - stop
Stops the Machine and return the value at the top of the stack

```
((retVal . S) E C D G)
retVal
```

#### primitive - primitive
Apply host-language's function to the stack



Compiler
--------

Here are some examples showing SECD instructions that can be obtained by compiling Nadeko programs.

### function calls

**code 1** *Nadeko source*

```lisp
(integers -take 5 -map (* 2) -fold + 0)
```

This Nadeko code gets compiled into the following SECD instructions.

**code 2** *code 1 compiled into SECD instruction*

```lisp
(
  (freeze ((stack-constant 0) (restore)))     ;; 0
  (freeze ((ref-arg +) (thaw) (restore)))     ;; +
  (freeze ((ref-arg -fold) (thaw) (restore))) ;; -fold
  (freeze (
    (freeze ((stack-constant 2) (restore))) 
    (ref-arg *) 
    (thaw) 
    (app) 
    (restore)))                               ;; (* 2)
  (freeze ((ref-arg -map) (thaw) (restore)))  ;; -map
  (freeze ((stack-constant 5) (restore)))     ;; 5
  (freeze ((ref-arg -take) (thaw) (restore))) ;; -take
  (ref-arg integers)                          ;; integers
  (thaw) 
  (app) 
  (app) 
  (app) 
  (app) 
  (app) 
  (app) 
  (app) 
  (stop)
)
```

When applying a function to its argument, the argument and the function 
have to be consed onto the `Stack` in respective order. Then an application instruction appear and β-reduction occurs.

**code 3** *typical application instructions*

```lisp
(stack-constant 1) 
(stack-closure x ((ref-arg x) (restore))) 
(app)
```

Nadeko is a lazily evaluated language so the argument to the function have to be frozen(i.e. turned into a thunk or promise). `freeze` instruction performs such an operation.  
`thaw` reverses the `freeze` operation; it evaluates the code inside the frozen thunk in the environment it got frozen.

**code 4** *application instructions with lazy evaluation*

```lisp
(freeze ((stack-constant 1) (restore)))
(stack-closure x ((ref-arg x) (thaw) (restore)))
(app)
```

### defining functions

```lisp
(:= (add x y) (+ x y))
```

```lisp
(
  (freeze (
    (stack-closure x (
      (stack-closure y (
        (freeze ((ref-arg y) (thaw) (restore))) 
        (freeze ((ref-arg x) (thaw) (restore))) 
        (ref-arg +) 
        (thaw) 
        (app) 
        (app) 
        (restore))) 
      (restore))) 
    (restore))) 
  (def add) 
  (stop)
)
```

### currying
Currying can be achieved by recursively compiling the lambda expression created with its (cdr params).

Multi-arg application can be achieved by recursively compiling the arguments and delaying them, stack them in reverse order, stack the closure, and add the same number of (app) as arguments.