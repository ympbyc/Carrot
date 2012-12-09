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