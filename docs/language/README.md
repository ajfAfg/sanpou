# The sanpou language

sanpou is a small specification language that compiles to TLA+. A program is a set of modules containing pure definitions, mutable variables, procedures, processes, and temporal properties; the compiler translates it into a TLA+ specification that TLC can model-check.

## Contents

- [Modules and scoping](modules-and-scoping.md) — module structure, `def`, `var`, and name resolution
- [Values](values.md) — integers, booleans, strings, tuples, sequences, sets, maps, records, atoms, and the built-in functions
- [Expressions](expressions.md) — operators, if expressions, quantifiers, and subscript/field reads
- [Statements, steps, and atomicity](statements.md) — statements, step structure, and the simultaneous-read semantics
- [Procedures and processes](procedures-and-processes.md) — procedure definitions, explicit returns, process instantiation, and fairness
- [Temporal properties](properties.md) — `property`, `globally`/`finally`, and invariants

## A complete example

```sanpou
mod example {
  def n = 3;                       // constant definition
  def inc(x) = x + 1;              // function definition (pure expression)
  def abs(x) = if (x < 0) { -x } else { x };   // if expression
  def pair = (1, true);            // tuple value
  def queue = [1, 2, 3];           // sequence value
  def label = "idle";              // string value
  def msg = {kind: "req", src: 1}; // record value; msg.kind reads a field
  def ids = {1, 2, 3};             // set value
  def evens = { i in 1..n : i % 2 == 0 };  // set comprehension (filter)
  def table = { i in 1..n -> 0 };  // map with domain 1..n (any set works)

  var count = 0;                   // mutable variable declaration
  var start in 1..n;               // non-deterministic initial value (from a set)
  var grid = { i in 1..n -> { j in 1..n -> 0 } };

  // temporal properties: globally/finally are allowed only here
  // (list property names in the sidecar config)
  property counted = finally(count > 0);
  property bounded = globally(forall (i in 1..n) { grid[i][1] >= 0 });

  procedure worker() {             // procedure definition
    while (count < n) {            // while loop
      either {                     //   non-deterministic branch;
        await count % 2 == 0,      //   only arms whose guards hold
        count = count + 1;         //   are offered
      } or {
        await count % 2 == 1,
        count = count + 1;
      }
      with (v in 1..n) {           //   non-deterministic choice of v,
        grid[v][1] = count;        //   one atomic step; nested
      }                            //   subscript assignment
      assert count <= n;           //   checked by TLC; failing is an error
      ;                            //   empty step (yield point)
    }
    return ();
  }

  fair process workers(self in 1..n) = worker;   // process definition
}
```
