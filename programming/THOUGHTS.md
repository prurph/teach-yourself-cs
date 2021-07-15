# Thoughts

These aren't notes, rather some reflections.

## 1.3.4 Procedures as Returned Values

### Abstractions and first-class procedures

Definition of _first-class_ elements

- May be named by variables
- May be passed as arguments to procedures
- May be returned as results of procedrues
- May be included in data structures

Footnote 66 exposes the major implementation cost of first-class functions as "reserving storage for a procedure's free variables even while the procedure is not executing." Basically, we need closures to have first-class functions.

## CS61a Week 2

### Y Combinator

Super cool way to define a recursive procedure without naming it (eg with `define`). Wouldn't be used in practice but basically shows lambda alone is enough to create recursion.

```scheme
;; The Y Combinator
(lambda (f) (lambda (n) (f f n)))
```

- Call Y Combinator with a function `f`
- Returns another function
  - That function takes a parameter `n`
  - Invokes `f` with `n` _and a reference to the function itself_, making recursion possible

## CS61a Week 3

### Continuation Passing for Tail Recursion

[Solution to extra problems](https://people.eecs.berkeley.edu/~bh/61a-pages/Solutions/week3)

Have a recursive procedure that isn't tail recursive because the final call combines two recursions:

```scheme
(define (rec a b)
  (+ (rec (- a 1) b) (rec a (- b 1)))
```

Convert to iterative tail-rec with _continuation passing_; basically add a parameter to the recursive function that takes a function `next` that specifies what to do with the argument.

```scheme
(define (rec a b next)
  ;; First call's lambda passes result to second rec call. Its lambda adds
  ;; to the second result.
  (rec (- a 1) b (lambda (x) (rec a (- b 1) (lambda (y) (+ x y))))))
```

## CS61a Week 4

### Tail-recursion and foldLeft/foldRight

Just some thoughts that are probably at least partially wrong.

I was trying to implement stuff like `filter` using a tail-recursive approach,
however a naive implementation ends up reversing the list, because we operate
on the car of the input list, but then must prepend to the accumulator.

```scheme
(define (filter f l)
  (define (iter l acc)
    (cond ((null? l) acc)
          ((f (car l)) (iter (cdr l) (cons (car l) acc)))
          (else (iter (cdr l) acc))))
  (iter l '()))
```

The other option, of course, is to append to the accumulator, but that then is
effectively not tail-recursive.

This is basically a `foldRight`, which cannot be made tail-recursive for mutable
data-structures (so Scala can do this for `ArrayLike`). Remember `foldRight`
"starts" from the right, so recursive calls go down until the tail element, then
come back up to the head of the list.

TL;DR naive `foldLeft` can be made tail recursive, but `foldRight` cannot.
Haskell has laziness that I think can avoid stack allocations, but isn't technically tail-recursion.

Summary from [Stack Overflow](https://stackoverflow.com/questions/4085118/why-foldright-and-reduceright-are-not-tail-recursive):

> As others have noted, a random-access structure such as `ArrayLike` allows the `foldRight` to be rearranged into a `foldLeft` operation, and so becomes eligible for TCE.

## CS61a Week 5

Semantic note: flat map can also be seen as accumulate (fold) with append and a map built in.

> The combination of mapping and accumulating with `append` is so common in this sort of problem that we isolate it as a separate procedure [`flatMap`]

```scheme
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
```

- `map proc seq` is obviously the map step
- `accumulate append nil` is just flattening

## 3.1.1 Local State Variables

Scheme uses the exclamation point naming convention for operations that change variables or mutate datastructures, just like Ruby!

This example of a "bank-account" object is pretty cool; it shows the connection between message-passing, classes, and local state through closures

- Our constructor returns a procedure that responds to messages
- These messages are equivalent to method calls on an object in an OOP language like Java
- The response to messages is itself a procedure that gets called, so you can think of it like looking up the method on the object and then calling it

```scheme
;; The "constructor"
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! alance (+ balance amount))
    balance)
  ;; Dispatch is the "instance" returned
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw))
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" m)))
  dispatch)

(define acc (make-account 100)) ; Instantiating an account
((acc 'withdraw) 50)            ; Look up the withdraw procecure, and apply with 50 as argument
```

## CS61a Lecture 21: Assignment and State

Super-cool "expansion" of procedure-definition and `let` forms into lambda expressions

```scheme
;; Procedure definition `(define (my-proc))` is shorthand for a lambda def
(define (my-proc)
  ...)
;; Like saying:
(define my-proc
  (lambda () ...)) ; zero-arg lambda b/c my-proc doesn't take any

;; `let` form is shorthand for a lambda definition and invocation with parameters
(define (make-count)
  (let ((result 0))
    (lambda ()
      (set! result (+ result 1))
      result)))
;; Like saying:
(define make-count
  ;; "long-form" procedure definition as above
  (lambda ()
    ;; A let form is like two sets of parentheses!
    ;; 1. Inner parens are the actual lambda
    ;; 2. Then invoke, in this case with argument 0!
    ((lambda (result)
        (lambda ()
          (set! result (+ result 1))
          result))
      0))))
```

I think we are going to learn that the "environment model" of computation is basically just closing an environment (local state, variables, etc) inside lambdas/procedures. This makes me think of how environments can be shared (e.g. with pointer references to the heap, mutable state) or copied (e.g. bash subshells, copy by value, threads getting copies of stack variables, etc).

In the Scheme counter example, two different "instances" point to the same lambda--returned by the constructor--but with different environments, in this case distinct local `result` variables. Quote:

> Objects are just closures...a way of representing which variable you are referencing with which name in which place...an environment.

You can think of `myObject.myMethod` as looking up the `myMethod` reference in the myObject environment. Wow.

## 3.2.1 The Rules for Evaluation

Amazing way to think about `define` and `set!` and the relationship with environments

- `define` creates a binding in the current environment frame (if it doesn't already exist) and assigns to the symbol the indicated value
- `set!` locates the binding of the variable in the enviornment (moving up enclosing frames as required) and changes that binding to indicate the new value
  - Note the frame being modified is the _frame with the binding_, not necessarily the current
    - This is effectively pass by reference
    - Pass by value would instead _create a new shadow binding in the current frame_ when the procedure is evaluated

## CS61a Lecture 21: Environments

Environment consists of:

1. Frame: bindings of identifiers to values
2. Pointer to _enclosing (parent) environment_

## CS61a Lecture 31: Concurrency

For concurrency, need the notion of instructions which are guaranteed to occur together

- Hardware typically provides atomic test and set (aka CAS) in some form
- Operating system uses that to provide primitives like mutex that enable critical sections
- Language uses this to provide more abstract serialization (eg `ConcurrentHashMap`)

## 3.5.1 Streams Are Delayed Lists

Obvious but elegant statement of streams vs lists:

> As a data abstraction, streams are the same as lists. The difference is the time at which the elements are evaluated. With ordinary lists, both the `car` and the `cdr` are evaluated at construction time. With streams, the `cdr` is evaluated at selection time.

This is emphasized by the following equivalence:

```scheme
;; These two are equivalent. `delay` is Scheme's promise constructor.
(cons-stream <a> <b>)
(cons <a> (delay <b>))
```

Also notable:

- `cons-stream` must be a special form, otherwise evaluating it would require evaluating its second argument, which defeats the entire purpose of streams.
- `delay` must also be a special form, but it can be implemented as a thunk 
- `force` materializes promises and then just calls the above thunk

This is just a simplification, in reality *promises memoize their result* to avoid forcing the same delayed object many times

```scheme
;; Delay as a special form syntactic sugar returning a thunk
(delay <exp>) === (lambda () <exp>)

;; Force is an ordinary procedure call of that lambda
(define (force delayed-object) (delayed-object))
```
