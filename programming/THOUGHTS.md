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
