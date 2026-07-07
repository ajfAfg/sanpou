The README workflow end-to-end: compile, model-check with TLC in tool mode,
then annotate the counterexample with `sanpou trace`. Requires SANPOU_JAVA
and SANPOU_TLA2TOOLS_JAR (provided by mise; see AGENTS.md).

A single process stuck on an await gives a short, deterministic trace.

  $ cat > dl.snp <<'EOF'
  > mod dl {
  >   var x = 0;
  >   procedure f() {
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

The annotated trace shows every step with the acting process and source
position, and the deadlock summary points at the blocking await.

  $ sanpou trace out/dl.out -o out
  Step 1: Initial state
    x = 0
  
  Step 2: p (process 1): [process p starts f]  [line 8]
    (no changes)
  
  Step 3: f (process 1): while (true) [check]  [line 4]
    (no changes)
  
  DEADLOCK — all processes blocked:
    process 1 (f): await x == 1, x = 0  [line 5]

Process ID sets need not be 1..n: with model-value ids, TLC prints pc/stack
in the function form ((Alice :> ... @@ Bob :> ...)); steps are attributed to
the domain element, frame-resident locals are annotated, and the deadlock
summary is keyed by the ids.

  $ cat > atoms.snp <<'EOF'
  > mod atoms {
  >   var turn = 0;
  >   procedure f() {
  >     var t = 10;
  >     t = t + 1;
  >     await turn == 1,
  >     turn = 2;
  >     return ();
  >   }
  >   process p = f in {`alice, `bob};
  > }
  > EOF
  $ cp dl.json atoms.json
  $ sanpou compile atoms.snp -o out
  $ "$SANPOU_JAVA" -cp "$SANPOU_TLA2TOOLS_JAR" -XX:+UseParallelGC tlc2.TLC \
  >   -tool -config out/atoms.cfg -metadir out/atoms_states -workers 1 \
  >   out/atoms.tla > out/atoms.out 2>&1
  [11]

  $ sanpou trace out/atoms.out -o out
  Step 1: Initial state
    turn = 0
  
  Step 2: p (process alice): [process p starts f]  [line 9]
    (frame: t = null)
  
  Step 3: f (process alice): var t = 10  [line 4]
    t = 10
  
  Step 4: f (process alice): t = t + 1  [line 5]
    t = 11
  
  Step 5: p (process bob): [process p starts f]  [line 9]
    (frame: t = null)
  
  Step 6: f (process bob): var t = 10  [line 4]
    t = 10
  
  Step 7: f (process bob): t = t + 1  [line 5]
    t = 11
  
  DEADLOCK — all processes blocked:
    process alice (f): await turn == 1, turn = 2  [line 6]
    process bob (f): await turn == 1, turn = 2  [line 6]
