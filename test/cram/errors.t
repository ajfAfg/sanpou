CLI usage errors.

  $ sanpou
  Usage: sanpou <compile|trace> <file> [-o outdir]
    compile <file.snp>  Compile sanpou source to .tla and .sourcemap.json
                        If <file>.json exists, also generate .cfg
    trace <file.out>    Annotate TLC output with source info
    -o Output directory
    -help  Display this list of options
    --help  Display this list of options
  [1]

  $ sanpou frobnicate
  Unknown subcommand: frobnicate
  Usage: sanpou <compile|trace> <file> [-o outdir]
    compile <file.snp>  Compile sanpou source to .tla and .sourcemap.json
                        If <file>.json exists, also generate .cfg
    trace <file.out>    Annotate TLC output with source info
    -o Output directory
    -help  Display this list of options
    --help  Display this list of options
  [1]

  $ sanpou compile
  Error: missing file argument.
  Usage: sanpou <compile|trace> <file> [-o outdir]
    compile <file.snp>  Compile sanpou source to .tla and .sourcemap.json
                        If <file>.json exists, also generate .cfg
    trace <file.out>    Annotate TLC output with source info
    -o Output directory
    -help  Display this list of options
    --help  Display this list of options
  [1]

Compile diagnostics carry the file name and source position.

Type error:

  $ cat > add_bool.snp <<'EOF'
  > mod m {
  >   var x = 0;
  >   procedure f() {
  >     x = 1 + true;
  >   }
  >   process p(self in 1..1) = f;
  > }
  > EOF
  $ sanpou compile add_bool.snp -o out
  add_bool.snp:4:13: Type error: cannot unify bool with int
  [1]

Functions and procedures are second-class; first-class uses that would
previously crash the emitter or compile to invalid TLA+ are diagnosed:

  $ cat > call_fun.snp <<'EOF'
  > mod m {
  >   def f(x) = x;
  >   procedure g() { f(1); return (); }
  >   process p(self in 1..1) = g;
  > }
  > EOF
  $ sanpou compile call_fun.snp -o out
  call_fun.snp:3:19: f is not a procedure
  [1]

  $ cat > higher_order.snp <<'EOF'
  > mod m {
  >   def apply(g) = g(1);
  >   def inc(x) = x + 1;
  >   def y = apply(inc);
  > }
  > EOF
  $ sanpou compile higher_order.snp -o out
  higher_order.snp:2:18: g is not a function
  [1]

Temporal operators live in property items only; using one elsewhere, or
referencing a property from a runtime context, is rejected:

  $ cat > temporal.snp <<'EOF'
  > mod m {
  >   var x = 0;
  >   procedure f() {
  >     await globally(x == 0);
  >     return ();
  >   }
  >   process p(self in 1..1) = f;
  > }
  > EOF
  $ sanpou compile temporal.snp -o out
  temporal.snp:4:11: globally is a temporal operator and is only allowed in a property
  [1]

  $ cat > temporal_ref.snp <<'EOF'
  > mod m {
  >   var x = 0;
  >   property p = globally(x == 0);
  >   procedure f() {
  >     await p;
  >     return ();
  >   }
  >   process ps(self in 1..1) = f;
  > }
  > EOF
  $ sanpou compile temporal_ref.snp -o out
  temporal_ref.snp:5:11: p is a property and can only be referenced from another property
  [1]

Inside a property, temporal formulas live at TLA+'s temporal level: they
combine with the boolean connectives but cannot be operands of comparisons,
collection elements, or arguments:

  $ cat > temporal_eq.snp <<'EOF'
  > mod m {
  >   var x = 0;
  >   property q = globally(x >= 0) == true;
  >   procedure f() { x = 1; return (); }
  >   process p(self in 1..1) = f;
  > }
  > EOF
  $ sanpou compile temporal_eq.snp -o out
  temporal_eq.snp:3:16: a temporal formula may only be combined with && / || / ! here: TLA+ does not allow one inside comparisons, collections, or arguments
  [1]

Syntax error:

  $ cat > syntax.snp <<'EOF'
  > mod m {
  >   procedure f() {
  >     x =
  >   }
  > }
  > EOF
  $ sanpou compile syntax.snp -o out
  syntax.snp:4:3: Syntax error
  [1]

A map initializer or set comprehension whose binder is not a plain
`x in <set>` is diagnosed at the offending expression:

  $ cat > bad_map.snp <<'EOF'
  > mod m {
  >   def d = { 1 -> 2 };
  > }
  > EOF
  $ sanpou compile bad_map.snp -o out
  bad_map.snp:2:13: a map initializer requires a binder of the form `x in <set>`
  [1]

  $ cat > bad_comp.snp <<'EOF'
  > mod m {
  >   def d = { a + b in s : true };
  > }
  > EOF
  $ sanpou compile bad_comp.snp -o out
  bad_comp.snp:2:13: a set comprehension requires a binder of the form `x in <set>`
  [1]

A `key : value` brace form with a non-membership key is a record literal, so a
key that is not a plain field label is diagnosed as such:

  $ cat > bad_field.snp <<'EOF'
  > mod m {
  >   def d = { 1 : true };
  > }
  > EOF
  $ sanpou compile bad_field.snp -o out
  bad_field.snp:2:13: a record field label must be a plain name
  [1]

A whole-variable assignment combined with another assignment to the same
variable in one step is rejected (path updates alone compose into one
EXCEPT and stay legal):

  $ cat > conflict.snp <<'EOF'
  > mod m {
  >   var r = {a: 0, b: 0};
  >   procedure f() {
  >     r = {a: 9, b: 9}, r.a = 5;
  >     return ();
  >   }
  >   process p(self in 1..1) = f;
  > }
  > EOF
  $ sanpou compile conflict.snp -o out
  conflict.snp:4:23: conflicting assignments to r in one step: a whole-variable assignment cannot be combined with another assignment to the same variable
  [1]

A record with a repeated field is rejected:

  $ cat > dup_field.snp <<'EOF'
  > mod m {
  >   def d = { a: 1, a: 2 };
  > }
  > EOF
  $ sanpou compile dup_field.snp -o out
  dup_field.snp:2:19: duplicate record field: a
  [1]

Atoms are literals in their own syntactic namespace, so they cannot clash
with declarations; an atom whose text is an emitter-reserved name cannot be
renamed away (the text is the model value's identity), so it is rejected:

  $ cat > atom_reserved.snp <<'EOF'
  > mod m {
  >   def x = `defaultInitValue;
  > }
  > EOF
  $ sanpou compile atom_reserved.snp -o out
  atom_reserved.snp:2:11: defaultInitValue is reserved: it collides with a name in the emitted TLA+ module
  [1]

Names the emitter generates (or pulls in via EXTENDS) are reserved:

  $ cat > reserved.snp <<'EOF'
  > mod m {
  >   var pc = 0;
  > }
  > EOF
  $ sanpou compile reserved.snp -o out
  reserved.snp:2:3: pc is reserved: it collides with a name in the emitted TLA+ module
  [1]
A module with no processes has no behavior to check (and its ProcSet, Next,
and Spec would be degenerate), so it is diagnosed:

  $ cat > no_process.snp <<'EOF'
  > mod m {
  >   def x = 1;
  >   procedure f() { return (); }
  > }
  > EOF
  $ sanpou compile no_process.snp -o out
  no_process.snp:1:1: module m defines no processes; there is no behavior to check
  [1]

A process ID set must be constant — the emitted spec fixes ProcSet and the
domain of pc/stack at Init, so a domain reading mutable state would break
TLC at runtime the moment the state changes:

  $ cat > nc_domain.snp <<'EOF'
  > mod m {
  >   var n = 2;
  >   procedure f() {
  >     n = n + 1;
  >     return ();
  >   }
  >   process p(self in 1..n) = f;
  > }
  > EOF
  $ sanpou compile nc_domain.snp -o out
  nc_domain.snp:7:24: a process ID set must be constant, but n is mutable state (or depends on it)
  [1]
A step is one atomic action with a single control-transfer slot, so a call,
return, break, or continue must be the step's final statement — an earlier
one would be silently overwritten:

  $ cat > two_calls.snp <<'EOF'
  > mod m {
  >   procedure a() { return (); }
  >   procedure b() { return (); }
  >   procedure f() {
  >     a(), b();
  >     return ();
  >   }
  >   process p(self in 1..1) = f;
  > }
  > EOF
  $ sanpou compile two_calls.snp -o out
  two_calls.snp:5:5: a procedure call must be the last statement of its step: the following statements merge into the same atomic action and the transfer would discard them
  [1]
Procedure calls do not compose with await: a hoisted call would run before
the guard is tested, and a call inside the condition would be evaluated once
instead of re-tested. (A statement call after the await is fine — the guard
and the push share one action.)

  $ cat > await_call.snp <<'EOF'
  > mod m {
  >   var flag = false;
  >   procedure get() { return flag; }
  >   procedure waiter() {
  >     await get();
  >     return ();
  >   }
  >   process p(self in 1..1) = waiter;
  > }
  > EOF
  $ sanpou compile await_call.snp -o out
  await_call.snp:5:11: an await condition cannot contain a procedure call: the call would be evaluated once, not re-evaluated with the guard
  [1]

  $ cat > step_call.snp <<'EOF'
  > mod m {
  >   var lock = 0;
  >   var y = 0;
  >   procedure bump() { return 1; }
  >   procedure f() {
  >     await lock == 0, y = bump();
  >     return ();
  >   }
  >   process p(self in 1..1) = f;
  > }
  > EOF
  $ sanpou compile step_call.snp -o out
  step_call.snp:6:26: a procedure call cannot appear in the same step as an await: the hoisted call would run before the guard is tested
  [1]

Every finishing path of a procedure must end in a return; a body that can
fall off its end is rejected (a procedure that never finishes — while (true)
with no break — needs none):

  $ cat > fall_through.snp <<'EOF'
  > mod m {
  >   var a = 0;
  >   procedure helper() { a = 1; }
  >   procedure main() { helper(); a = 2; return (); }
  >   process p(self in 1..1) = main;
  > }
  > EOF
  $ sanpou compile fall_through.snp -o out
  fall_through.snp:3:3: procedure helper can fall off its end without a return: every finishing path must end in `return`
  [1]

  $ cat > if_no_else.snp <<'EOF'
  > mod m {
  >   var x = 0;
  >   procedure f() {
  >     if (x == 0) { return (); }
  >   }
  >   process p(self in 1..1) = f;
  > }
  > EOF
  $ sanpou compile if_no_else.snp -o out
  if_no_else.snp:3:3: procedure f can fall off its end without a return: every finishing path must end in `return`
  [1]
A process root procedure must take no parameters (the wrapper calls it
without arguments):

  $ cat > root_param.snp <<'EOF'
  > mod m {
  >   var x = 0;
  >   procedure f(n) { x = n + 1; return (); }
  >   process p(self in 1..1) = f;
  > }
  > EOF
  $ sanpou compile root_param.snp -o out
  root_param.snp:3:43: f takes 1 parameter; a process root procedure must take none (the process wrapper calls it without arguments)
  [1]
Assignment targets resolve lexically: a with-binder or parameter that
shadows a mutable variable is not itself assignable, so the write is
rejected instead of silently going to the wrong binding:

  $ cat > with_shadow.snp <<'EOF'
  > mod m {
  >   var x = 0;
  >   procedure p() {
  >     with (x in {1, 2}) { x = x + 1; }
  >     return ();
  >   }
  >   process ps(self in 1..1) = p;
  > }
  > EOF
  $ sanpou compile with_shadow.snp -o out
  with_shadow.snp:4:26: Cannot assign to x: not a mutable variable
  [1]

Lexical error:

  $ cat > lexical.snp <<'EOF'
  > mod m {
  >   procedure f() {
  >     ?
  >   }
  > }
  > EOF
  $ sanpou compile lexical.snp -o out
  lexical.snp:3:5: Unexpected character: '?'
  [1]

Unterminated string literal:

  $ cat > unterm.snp <<'EOF'
  > mod m {
  >   def x = "abc;
  > }
  > EOF
  $ sanpou compile unterm.snp -o out
  unterm.snp:2:11: Unterminated string literal (strings cannot span lines)
  [1]
An integer literal that does not fit the native int is a located lexical
error (it used to crash the compiler with an uncaught exception):

  $ cat > bigint.snp <<'EOF'
  > mod m {
  >   def c = 99999999999999999999999999;
  > }
  > EOF
  $ sanpou compile bigint.snp -o out
  bigint.snp:2:11: Integer literal out of range
  [1]
Two modules with the same name would write to the same output file, so the
duplicate is rejected:

  $ cat > dup_mod.snp <<'EOF'
  > mod m {
  >   def x = 1;
  > }
  > mod m {
  >   def y = 2;
  > }
  > EOF
  $ sanpou compile dup_mod.snp -o out
  dup_mod.snp:4:1: module m is already defined
  [1]

Invalid sidecar config:

  $ cat > cfg.snp <<'EOF'
  > mod m {
  >   procedure f() { return (); }
  >   process p(self in 1..1) = f;
  > }
  > EOF
  $ echo 'not json' > cfg.json
  $ sanpou compile cfg.snp -o out
  Error: invalid sanpou config 'cfg.json': Json.parse: unexpected char 'n' at 0
  [1]

Sidecar property/invariant names are validated against the module:

  $ cat > cfg_typo.snp <<'EOF'
  > mod m {
  >   var x = 0;
  >   property live = finally(x == 1);
  >   procedure f() { x = 1; return (); }
  >   fair process p(self in 1..1) = f;
  > }
  > EOF
  $ cat > cfg_typo.json <<'EOF'
  > { "checks": { "deadlock": true, "termination": false },
  >   "properties": ["liev"], "invariants": [] }
  > EOF
  $ sanpou compile cfg_typo.snp -o out
  cfg_typo.snp:1:1: the sidecar config lists property 'liev', but module m defines no such property item
  [1]

Trace without a compile output directory:

  $ sanpou trace missing.out -o nonexistent
  Error: directory 'nonexistent' not found.
  Run 'sanpou compile -o nonexistent' first to generate the source map.
  [1]

Trace without a matching source map:

  $ mkdir -p empty_out
  $ echo '@!@!@STARTMSG 2185:0 @!@!@' > orphan.out
  $ sanpou trace orphan.out -o empty_out
  Error: source map 'empty_out/orphan.sourcemap.json' not found for module 'orphan'.
  Run 'sanpou compile <file.snp> -o empty_out' first to generate it.
  [1]
