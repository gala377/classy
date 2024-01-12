## BUGS

This example yields wrong type

```
methods for Option(a) {
  map: ((a) -> b) -> Option(b)
  map = ...
}
```

A type for `map` is (@g(2, 0)) -> @g(0, 0)) -> Option(@g(0, 0))

I don't know where the 2 comes from as it should be obviously 1

What's to figure out

1. Allocation strategy for the old space

## To do with typechecker

- Remove redundant types
  - For example, 2 structs, even with the same fields are always distinct
  - But two tuples with the same fields are the same
  - As such a context like:
    1 => (int, int)
    2 => (int, int)
  - Should just retain 1 and treat 2 as an alias for 1, then remove it.
  - We could rewrite duplicates as aliases of each other, then just remove aliases again.
  - The problem is how many times should we do this, for example

type A = (Int, (Int, (Int, Int)))
type B = (Int, (Int, Int))
type D = (Int, Int)

creates context in form
0 => (Int, Alias(1))
1 => (Int, Alias(2))
2 => (Int, Int)
3 => (Int Alias(4))
4 => (Int, Int)

in the first pass we will deduce that 2 and 3 are the same
but not that 1 and 4 are.

## To move

1. Old space would be nice, not necessary yet.
2. Class universe aka vm runtime
   - We need more classes, like int, bool, bytes and so on.
   - Also we somehow need to be able to search for classes.
   - And some classes won't be in the runtime. Nor in the permament heap
3. Test cases
4. Vm logic
   - Evaluation, dispatch loop and so on
5. Parser and lexer
