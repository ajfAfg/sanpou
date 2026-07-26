# Expressions

## Operators

- Integer arithmetic: `+ - * / %` (`/` is integer division)
- Comparison: `< > <= >= == !=`
- Boolean: `&& || !`
- Set membership: `in` (`x in S`)

## Subscript and field reads

Read a sequence or map element with `a[i]` and a record field with `r.f`; the two compose freely (`grid[i].tag`). For assignment targets built from the same paths, see [Statements, steps, and atomicity](statements.md#assignment).

## Quantifiers

`forall (x in S) { p }` and `exists (x in S) { p }` are boolean expressions ranging over the set `S`. The domain may be any set expression.

## If expressions

`if (cond) { e1 } else { e2 }` is an expression; the `else` branch is mandatory.

### Atomicity

Unlike the statement `if`, whose condition check compiles to its own action (an interleaving point), an assignment step containing an if expression is a single atomic action:

```sanpou
x = if (x == 0) { 1 } else { x };  // atomic test-and-set
```

Caveat: this atomicity guarantee only holds when the condition is call-free. A procedure call in the condition is hoisted into its own preceding steps (the call runs first, binding a temporary the condition then reads), so other processes can interleave between the call returning and the update. When you rely on the test and the update being one step, keep the condition free of procedure calls. Calls in a branch are rejected at compile time, since hoisting them would run them unconditionally.
