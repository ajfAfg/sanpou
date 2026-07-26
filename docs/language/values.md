# Values

## Integers and booleans

Integer arithmetic uses `+ - * / %`, where `/` is integer division. Booleans support `&& || !`. See [Expressions](expressions.md#operators) for the full operator list.

## Strings

Strings `"idle"` support only equality (`==` / `!=`); there is no concatenation or indexing.

## Tuples

Tuples `(a, b)` may mix types.

## Sequences

Sequences `[a, b, c]` are homogeneous.

## Sets

Sets `{a, b, c}` (and the empty set `{}`) are homogeneous.

`lo..hi` is the set of integers in that range, and a first-class value like any other set.

Set comprehension `{ x in S : p }` keeps the elements of `S` satisfying `p`.

Any binder domain — quantifiers, `with`, `var`, `process`, and map/comprehension initializers — is an arbitrary set expression, not just a range.

Set membership is tested with `x in S`.

## Maps

`{ x in S -> e }` builds a map with domain `S` and value `e` for each key. The domain may be any set — ints, strings, or model values — and subscript keys take the domain's element type.

## Records

Records `{f1: e1, f2: e2}` have fixed named fields (field types may differ); read a field with `r.f`. Records are structural: two record types match only when their field sets are identical (no row polymorphism).

## Atoms

`` `red `` is an opaque constant that compares unequal to everything but itself — the idiomatic way to write sentinels and enumeration-like tags without integer encodings.

Atoms are literals (Elixir-style): no declaration, their own syntactic namespace, usable anywhere (`` state = `red ``, `` {`red, `green} ``). They share one type and support only `==` / `!=`.

Every atom used in a module becomes a TLA+ `CONSTANT` of the same name, assigned a model value in the generated `.cfg` — the name is the value's identity in traces, so an atom whose text collides with a compiler-generated name is rejected.

Note that a typo makes a fresh atom, not an error: `` s == `redy `` is simply always false.

## Built-in functions

Sequence builtins: `head`, `tail`, `append`, `concat`, `len`.

Set builtins: `union`, `intersection`, `difference` (binary set operations), `cardinality` (element count), `subseteq` (subset test).

Builtins are lexically shadowed by module definitions of the same name (see [Modules and scoping](modules-and-scoping.md#scoping)).
