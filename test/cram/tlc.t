Model checking through the sanpou CLI: compile with sanpou, then run TLC on
the result. Requires SANPOU_JAVA and SANPOU_TLA2TOOLS_JAR (provided by mise;
see AGENTS.md) — without them this test is skipped via enabled_if.

Only the TLC verdict is asserted; the full TLC output is dumped when the
verdict is neither a clean pass nor a deadlock, so failures stay debuggable.

  $ tlc() {
  >   "$SANPOU_JAVA" -cp "$SANPOU_TLA2TOOLS_JAR" -XX:+UseParallelGC tlc2.TLC \
  >     -config "$1/$2.cfg" -metadir "$1/states" -workers 1 "$1/$2.tla" \
  >     > "$1/$2.raw" 2>&1
  >   verdict=$(grep -Eo 'No error has been found|Deadlock reached|The first argument of Assert evaluated to FALSE' "$1/$2.raw" | head -n 1)
  >   if [ -n "$verdict" ]; then echo "$verdict"; else cat "$1/$2.raw"; fi
  > }

Every shipped example model-checks cleanly. rwlock is safety-only; the
others also check temporal properties (Termination and/or user-defined
ones), which exercises TLC's liveness checking against the generated
specs — including the defaultInitValue frame sentinel.

  $ sanpou compile ../../example/rwlock.snp -o rwlock
  $ tlc rwlock rwlock
  No error has been found

  $ sanpou compile ../../example/semaphore.snp -o semaphore
  $ tlc semaphore semaphore
  No error has been found

  $ sanpou compile ../../example/factorial.snp -o factorial
  $ tlc factorial factorial
  No error has been found

  $ sanpou compile ../../example/barrier.snp -o barrier
  $ tlc barrier bakery_algorithm
  No error has been found

  $ sanpou compile ../../example/conditional_variable.snp -o cv
  $ tlc cv conditional_variable
  No error has been found

Recursion with parameters, locals, and call-return temporaries, checked
end-to-end: the await unblocks only if fact(3) = 6 was computed correctly
through the stack frames.

  $ cat > fact.snp <<'EOF'
  > mod fact_check {
  >   var x = 0;
  >   procedure fact(n) {
  >     if (n == 0) {
  >       return 1;
  >     } else {
  >       var ans = n * fact(n - 1);
  >       return ans;
  >     }
  >   }
  >   procedure f() {
  >     x = fact(3);
  >     while (true) {
  >       await x == 6;
  >     }
  >   }
  >   process p = f in 1..1;
  > }
  > EOF
  $ cat > fact.json <<'EOF'
  > { "checks": { "deadlock": true, "termination": false }, "properties": [] }
  > EOF
  $ sanpou compile fact.snp -o fact
  $ tlc fact fact_check
  No error has been found

Blocked awaits are reported as deadlocks: a single process stuck on an await
that can never fire, and two processes each waiting on the other.

  $ cat > single_dl.snp <<'EOF'
  > mod single_dl {
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
  $ cp fact.json single_dl.json
  $ sanpou compile single_dl.snp -o single_dl
  $ tlc single_dl single_dl
  Deadlock reached

  $ cat > mutual_dl.snp <<'EOF'
  > mod mutual_dl {
  >   var a = false;
  >   var b = false;
  >   procedure fa() {
  >     while (true) {
  >       await b == true,
  >       a = true;
  >     }
  >   }
  >   procedure fb() {
  >     while (true) {
  >       await a == true,
  >       b = true;
  >     }
  >   }
  >   process pa = fa in 1..1;
  >   process pb = fb in 2..2;
  > }
  > EOF
  $ cp fact.json mutual_dl.json
  $ sanpou compile mutual_dl.snp -o mutual_dl
  $ tlc mutual_dl mutual_dl
  Deadlock reached

An either statement offers only its *enabled* arms. At every state below
exactly one arm's guard holds (x alternates parity), so the process always
proceeds and terminates; a translation that committed to an arm before
evaluating its guard would deadlock instead. Reaching x = 3 needs both arms
in a single behavior, so termination also proves both arms fire.

  $ cat > either_guard.snp <<'EOF'
  > mod either_guard {
  >   var x = 0;
  >   procedure f() {
  >     while (x < 3) {
  >       either {
  >         await x % 2 == 0,
  >         x = x + 1;
  >       } or {
  >         await x % 2 == 1,
  >         x = x + 2;
  >       }
  >     }
  >     return ();
  >   }
  >   fair process p = f in 1..1;
  > }
  > EOF
  $ cat > either_guard.json <<'EOF'
  > { "checks": { "deadlock": true, "termination": true } }
  > EOF
  $ sanpou compile either_guard.snp -o either_guard
  $ tlc either_guard either_guard
  No error has been found

A with statement fires for any binder value satisfying its guard. At each
state below only v = x + 1 passes the await, so the process walks x from 0
to 3 and terminates; if the binder choice ignored the guard, a wrong pick
would block the single fair process forever.

  $ cat > with_guard.snp <<'EOF'
  > mod with_guard {
  >   var x = 0;
  >   procedure f() {
  >     while (x < 3) {
  >       with (v in 1..3) {
  >         await v == x + 1,
  >         x = v;
  >       }
  >     }
  >     return ();
  >   }
  >   fair process p = f in 1..1;
  > }
  > EOF
  $ cp either_guard.json with_guard.json
  $ sanpou compile with_guard.snp -o with_guard
  $ tlc with_guard with_guard
  No error has been found

An assert that holds is silent; one that fails halts TLC with an error
(unlike await, which would just disable the action).

  $ cat > assert_ok.snp <<'EOF'
  > mod assert_ok {
  >   var x = 0;
  >   procedure f() {
  >     while (x < 3) {
  >       assert x >= 0,
  >       x = x + 1;
  >     }
  >     return ();
  >   }
  >   fair process p = f in 1..1;
  > }
  > EOF
  $ cp either_guard.json assert_ok.json
  $ sanpou compile assert_ok.snp -o assert_ok
  $ tlc assert_ok assert_ok
  No error has been found

  $ cat > assert_fail.snp <<'EOF'
  > mod assert_fail {
  >   var x = 0;
  >   procedure f() {
  >     while (x < 3) {
  >       assert x != 2,
  >       x = x + 1;
  >     }
  >     return ();
  >   }
  >   fair process p = f in 1..1;
  > }
  > EOF
  $ cp either_guard.json assert_fail.json
  $ sanpou compile assert_fail.snp -o assert_fail
  $ tlc assert_fail assert_fail
  The first argument of Assert evaluated to FALSE

Sets end-to-end: two processes drawn from a set literal each unblock only if
the set operations (union, comprehension, cardinality, difference, subseteq)
and membership all evaluate as expected, then run to completion. Termination
therefore proves the emitted set constructs check under TLC's FiniteSets.

  $ cat > sets_check.snp <<'EOF'
  > mod sets_check {
  >   def s = union({1, 2}, {2, 3});
  >   def evens = { y in s : y % 2 == 0 };
  >   var x = 0;
  >   procedure f() {
  >     await cardinality(s) == 3 && cardinality(evens) == 1
  >           && 2 in s && subseteq(difference(s, {1, 3}), evens),
  >     x = 1;
  >     return ();
  >   }
  >   fair process p = f in {7, 8};
  > }
  > EOF
  $ cp either_guard.json sets_check.json
  $ sanpou compile sets_check.snp -o sets_check
  $ tlc sets_check sets_check
  No error has been found

Strings end-to-end: a state variable toggles between two string tags, driven
by string-equality guards. The process never blocks (one guard always holds),
so deadlock checking passes and confirms the emitted string literals compare
correctly under TLC.

  $ cat > str_check.snp <<'EOF'
  > mod str_check {
  >   var s = "idle";
  >   procedure f() {
  >     while (true) {
  >       await s == "idle",
  >       s = "busy";
  >       await s == "busy",
  >       s = "idle";
  >     }
  >   }
  >   process p = f in 1..1;
  > }
  > EOF
  $ cat > str_check.json <<'EOF'
  > { "checks": { "deadlock": true, "termination": false }, "properties": [] }
  > EOF
  $ sanpou compile str_check.snp -o str_check
  $ tlc str_check str_check
  No error has been found

Records end-to-end: a record-valued variable is read in the loop guard and
two of its fields are updated in one step (a single EXCEPT). The process runs
to completion, so termination confirms the emitted record literal, field
access, and multi-field EXCEPT all check under TLC.

  $ cat > rec_check.snp <<'EOF'
  > mod rec_check {
  >   var s = {phase: "idle", n: 0};
  >   procedure f() {
  >     while (s.phase == "idle") {
  >       s.phase = "busy",
  >       s.n = s.n + 1;
  >     }
  >     return ();
  >   }
  >   fair process p = f in 1..1;
  > }
  > EOF
  $ cp either_guard.json rec_check.json
  $ sanpou compile rec_check.snp -o rec_check
  $ tlc rec_check rec_check
  No error has been found
