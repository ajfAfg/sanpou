The README workflow end-to-end: compile, model-check with TLC in tool mode,
then annotate the counterexample with `sanpou trace`. Requires SANPOU_JAVA
and SANPOU_TLA2TOOLS_JAR (provided by mise; see AGENTS.md).

A single process stuck on an await gives a short, deterministic trace.

  $ cat > dl.snp <<'EOF'
  > mod dl {
  >   var x = 0;
  >   fn f() {
  >     while (true) {
  >       await x == 1,
  >       x = 0;
  >     }
  >   }
  >   process p = f in 1..1;
  > }
  > EOF
  $ cat > dl.json <<'EOF'
  > { "checks": { "deadlock": true, "termination": false }, "properties": [] }
  > EOF
  $ sanpou compile dl.snp -o out
  $ "$SANPOU_JAVA" -cp "$SANPOU_TLA2TOOLS_JAR" -XX:+UseParallelGC tlc2.TLC \
  >   -tool -config out/dl.cfg -metadir out/states -workers 1 out/dl.tla \
  >   > out/dl.out 2>&1
  [11]

FIXME: action steps are missing from the rendered trace, and the deadlock
summary points at the process entry instead of the blocking await.
Trace_reader.parse_header only accepts "<Label(pid) line ...>" action
headers, but TLC 2.19 emits "<Label line ...>" (no process id), so every
action step is dropped and only the initial state survives.

  $ sanpou trace out/dl.out -o out
  Step 1: Initial state
    x = 0
  
  DEADLOCK — all processes blocked:
    process 1 (p): [process p starts f]  [line 8]

