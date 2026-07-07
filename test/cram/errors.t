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
  >   process p = f in 1..1;
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
  >   process p = g in 1..1;
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
  >   process p = f in 1..1;
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
  >   process ps = f in 1..1;
  > }
  > EOF
  $ sanpou compile temporal_ref.snp -o out
  temporal_ref.snp:5:11: p is a property and can only be referenced from another property
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

A record with a repeated field is rejected:

  $ cat > dup_field.snp <<'EOF'
  > mod m {
  >   def d = { a: 1, a: 2 };
  > }
  > EOF
  $ sanpou compile dup_field.snp -o out
  dup_field.snp:2:13: duplicate record field: a
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

Invalid sidecar config:

  $ cat > cfg.snp <<'EOF'
  > mod m {
  >   procedure f() {}
  >   process p = f in 1..1;
  > }
  > EOF
  $ echo 'not json' > cfg.json
  $ sanpou compile cfg.snp -o out
  Error: invalid sanpou config 'cfg.json': Json.parse: unexpected char 'n' at 0
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
