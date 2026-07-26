# Modules and scoping

## Module structure

A sanpou file contains modules. A module groups the declarations that make up
one specification:

```sanpou
mod example {
  def n = 3;                 // constant definition
  def inc(x) = x + 1;        // function definition (pure expression)

  var count = 0;             // mutable variable declaration
  var start in 1..n;         // non-deterministic initial value (from a set)

  procedure worker() { ... }
  fair process workers(self in 1..n) = worker;

  property counted = finally(count > 0);
}
```

## Definitions: `def`

`def name = e;` binds a constant, and `def name(x, ...) = e;` a function whose
body is a pure expression. Definitions cannot mutate state; they are the
place for constants, derived values, and helper functions.

## Variables: `var`

`var name = e;` declares a mutable state variable with initial value `e`.
`var name in S;` gives it a non-deterministic initial value drawn from the set
`S` — TLC explores every choice. The domain `S` may be any set expression,
not just a range.

Variables are the mutable state of the specification; they may only be
assigned inside procedures (see
[Statements, steps, and atomicity](statements.md)).

## Scoping

Name resolution is sequential and lexical everywhere, module level included —
a later `def`/`var`/`procedure`/`process` of the same name shadows the
earlier one from its point onward (the compiler renames the shadowed
declarations apart in the emitted TLA+).

Built-in functions (see [Values](values.md#built-in-functions)) are lexically
shadowed by module definitions of the same name.

Atom literals live in their own syntactic namespace and never clash with
declarations; a declaration whose name coincides with a used atom's text is
renamed apart in the emitted TLA+ (the atom keeps its name — it is the model
value's identity).
