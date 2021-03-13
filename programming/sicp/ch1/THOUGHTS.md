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
