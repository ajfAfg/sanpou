# Procedures and processes

## Procedures

`procedure name(params) { ... }` defines a procedure. Bodies are made of
statements (see [Statements, steps, and atomicity](statements.md));
procedures may call each other, return values, and recurse.

## Returns are explicit

Every finishing path of a procedure must end in `return` (`return ();` when
there is nothing to return) — the compiler rejects a body that can fall off
its end.

A procedure that never finishes, such as a `while (true)` loop with no
`break`, needs none. The return type of such a never-finishing procedure is
left fully unconstrained (polymorphic), so binding its "result" (`x = p();`)
typechecks at any type; the statements after that call are simply
unreachable, and the compiler does not diagnose the dead code. This is a
deliberate trade-off — see the discussion in
[#153](https://github.com/ajfAfg/sanpou/issues/153).

## Processes

`process name(self in S) = proc;` instantiates the procedure `proc` once per
id in the set `S`; inside the procedure the instance's id is read as `self`,
which is exactly the binding the head introduces.

`S` may be any set — integers, strings, or model values — and `self` takes
its element type; all processes in a module share one id type. For a single
instance, write a one-element set: `process p(self in {1}) = f;`.

The id sets of different processes must be pairwise disjoint; the compiler
emits `ASSUME` disjointness checks, so TLC fails fast at startup if they
overlap.

## Fairness

`fair process ... ;` adds weak fairness, `fair+ process ... ;` strong
fairness.
