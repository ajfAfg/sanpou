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
  >   fn f() {
  >     x = 1 + true;
  >   }
  >   process p = f in 1..1;
  > }
  > EOF
  $ sanpou compile add_bool.snp -o out
  add_bool.snp:4:13: Type error: cannot unify bool with int
  [1]

Temporal operators live in module-level defs only; using one (or a def
containing one) in a runtime context is rejected:

  $ cat > temporal.snp <<'EOF'
  > mod m {
  >   var x = 0;
  >   fn f() {
  >     await globally(x == 0);
  >     return ();
  >   }
  >   process p = f in 1..1;
  > }
  > EOF
  $ sanpou compile temporal.snp -o out
  temporal.snp:4:11: globally is a temporal operator and is only allowed in a module-level def
  [1]

  $ cat > temporal_ref.snp <<'EOF'
  > mod m {
  >   var x = 0;
  >   def p = globally(x == 0);
  >   fn f() {
  >     await p;
  >     return ();
  >   }
  >   process ps = f in 1..1;
  > }
  > EOF
  $ sanpou compile temporal_ref.snp -o out
  temporal_ref.snp:5:11: p is a temporal property and can only be referenced from another module-level def
  [1]

Syntax error:

  $ cat > syntax.snp <<'EOF'
  > mod m {
  >   fn f() {
  >     x =
  >   }
  > }
  > EOF
  $ sanpou compile syntax.snp -o out
  syntax.snp:4:3: Syntax error
  [1]

Lexical error:

  $ cat > lexical.snp <<'EOF'
  > mod m {
  >   fn f() {
  >     ?
  >   }
  > }
  > EOF
  $ sanpou compile lexical.snp -o out
  lexical.snp:3:5: Unexpected character: '?'
  [1]

Invalid sidecar config:

  $ cat > cfg.snp <<'EOF'
  > mod m {
  >   fn f() {}
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
