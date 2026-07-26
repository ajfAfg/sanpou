# Statements, steps, and atomicity

## Statements

Procedure bodies are built from these statements:

- Assignment, including nested subscripts (`a[i][j] = e`) — see
  [Assignment](#assignment)
- Procedure calls, with return values; recursion is supported
- `await e` — block until the guard holds
- `assert e` — checked by TLC; failing is an error
- `return e` / `return ()` — see
  [Procedures and processes](procedures-and-processes.md#returns-are-explicit)
- `break`, `continue`
- `if` / `else if` / `else`
- `while`
- `either { } or { }` — non-deterministic branch; only arms whose guards
  hold are offered
- `with (x in S) { }` — non-deterministic choice of `x` from the set `S`
  (any set expression), executed as one atomic step

## Assignment

An assignment target is a variable followed by any mix of `[i]` and `.f`
steps (`grid[i].tag = e`), compiling to a TLA+ `EXCEPT` update; several
writes to one variable in a step merge into one `EXCEPT`.

## Steps and atomicity

Statements joined by commas and ended with `;` form one atomic action; block
statements (`if`, `while`, ...) evaluate their condition in an action of its
own; a bare `;` is an explicit yield point.

If *expressions* are the exception: an assignment step containing an if
expression stays a single atomic action — see
[Expressions](expressions.md#atomicity).

## Reads within a step see the pre-state

The statements of one step execute *simultaneously*, not in sequence — every
expression (an assignment's right-hand side, an `await`, an `assert`) reads
the state from before the step. So in `x = 1, y = x;` the new `y` is the
*old* `x`, and `x = 1, await x == 1;` blocks forever when `x` starts at 0.

This is TLA+'s (and multi-assignment's) semantics but the *opposite* of
PlusCal, where a read after an assignment in the same step sees the new
value — split the step with `;` when you want sequencing.

Guards are also checked before asserts regardless of their order in the
step, so a disabled step never fires its asserts.
