Model checking through the sanpou CLI: compile with sanpou, then run TLC on
the result. Requires SANPOU_JAVA and SANPOU_TLA2TOOLS_JAR (provided by mise;
see AGENTS.md) — without them this test is skipped via enabled_if.

Only the TLC verdict is asserted; the full TLC output is dumped when the
verdict is neither a clean pass nor a deadlock, so failures stay debuggable.

  $ tlc() {
  >   "$SANPOU_JAVA" -cp "$SANPOU_TLA2TOOLS_JAR" -XX:+UseParallelGC tlc2.TLC \
  >     -config "$1/$2.cfg" -metadir "$1/states" -workers 1 "$1/$2.tla" \
  >     > "$1/$2.raw" 2>&1
  >   verdict=$(grep -Eo 'No error has been found|Deadlock reached|The first argument of Assert evaluated to FALSE|Assumption line .* is false' "$1/$2.raw" | head -n 1)
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

Every finishing path of a procedure must end in an explicit return — a
fall-through used to jump straight to Done with every frame stacked,
silently skipping the caller's continuation while Termination passed. A
procedure that never finishes (while (true) with no break) needs none.
With the returns written, the caller resumes and the property holds:

  $ cat > fallthrough.snp <<'EOF'
  > mod fallthrough {
  >   var a = 0;
  >   procedure helper() { a = 1; return (); }
  >   procedure main() {
  >     helper();
  >     a = 2;
  >     return ();
  >   }
  >   fair process p = main in 1..1;
  >   property done2 = finally(a == 2);
  > }
  > EOF
  $ cat > fallthrough.json <<'EOF'
  > { "checks": { "deadlock": true, "termination": true },
  >   "properties": ["done2"] }
  > EOF
  $ sanpou compile fallthrough.snp -o fallthrough
  $ tlc fallthrough fallthrough
  No error has been found
Normal completion is not a deadlock: with the default config (deadlock
checking on, termination checking off) a program that simply runs to
completion passes — the Terminating stuttering disjunct is always part of
Next. (It used to be gated on the termination check, so this reported
"Deadlock reached".)

  $ cat > run_done.snp <<'EOF'
  > mod run_done {
  >   var x = 0;
  >   procedure f() {
  >     x = 1;
  >     return ();
  >   }
  >   fair process p = f in 1..1;
  > }
  > EOF
  $ cp fact.json run_done.json
  $ sanpou compile run_done.snp -o run_done
  $ tlc run_done run_done
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

Binders shadowing a module-level name: TLA+ allows no shadowing, so the fun
parameter and comprehension binder named like the global x are renamed on
emission; the spec parses and the guard computes through them.

  $ cat > shadow.snp <<'EOF'
  > mod shadow {
  >   def double(x) = x * 2;
  >   def s = { x in 1..3 : x > 0 };
  >   var x = 0;
  >   procedure f() {
  >     await double(3) == 6 && forall (x in s) { x >= 1 },
  >     x = 1;
  >     return ();
  >   }
  >   fair process p = f in 1..1;
  > }
  > EOF
  $ cp either_guard.json shadow.json
  $ sanpou compile shadow.snp -o shadow
  $ tlc shadow shadow
  No error has been found

Module-level shadowing end-to-end: the second `def limit` reads the first,
`probe` (defined between the two `var seen`) writes the first seen while
`main` reads and writes the second, and the assert only holds if every
reference resolved to the binding in scope at its position.

  $ cat > shadow_seq.snp <<'EOF'
  > mod shadow_seq {
  >   def limit = 1;
  >   def limit = limit + 1;
  >   var seen = 0;
  >   procedure probe() { seen = limit; return (); }
  >   var seen = 10;
  >   procedure main() {
  >     probe();
  >     seen = seen + limit;
  >     assert seen == 12;
  >     return ();
  >   }
  >   fair process p = main in 1..1;
  > }
  > EOF
  $ cp either_guard.json shadow_seq.json
  $ sanpou compile shadow_seq.snp -o shadow_seq
  $ tlc shadow_seq shadow_seq
  No error has been found
Overlapping process ID sets are rejected at TLC startup: Init's pc CASE
would silently give a shared id to the first process only, so the emitted
pairwise-disjointness ASSUME fails fast instead.

  $ cat > overlap.snp <<'EOF'
  > mod overlap {
  >   var b = 0;
  >   procedure noop() { return (); }
  >   procedure g() { b = b + 1; return (); }
  >   fair process pf = noop in 1..2;
  >   fair process pg = g in 2..3;
  > }
  > EOF
  $ cp fact.json overlap.json
  $ sanpou compile overlap.snp -o overlap
  $ tlc overlap overlap
  Assumption line 10, col 8 to line 10, col 32 of module overlap is false
Multiple awaits in one step conjoin: `await false` keeps the step disabled
even when a later `await true` follows it in the same comma list, so the
invariant that x never changes holds. (The guard used to be overwritten by
the last await, letting the step fire.)

  $ cat > await_conj.snp <<'EOF'
  > mod await_conj {
  >   var x = 0;
  >   def xzero = x == 0;
  >   procedure f() {
  >     await false, await true, x = 1;
  >     return ();
  >   }
  >   process p = f in 1..1;
  > }
  > EOF
  $ cat > await_conj.json <<'EOF'
  > { "checks": { "deadlock": false, "termination": false },
  >   "invariants": ["xzero"], "properties": [] }
  > EOF
  $ sanpou compile await_conj.snp -o await_conj
  $ tlc await_conj await_conj
  No error has been found
A nullary def is applied as a bare operator reference (TLA+ has no f()
application syntax; the call site used to emit one and fail SANY).

  $ cat > nullary.snp <<'EOF'
  > mod nullary {
  >   def f() = 1;
  >   var ok = 0;
  >   procedure main() {
  >     await f() == 1,
  >     ok = f() + 1;
  >     return ();
  >   }
  >   fair process p = main in 1..1;
  > }
  > EOF
  $ cp either_guard.json nullary.json
  $ sanpou compile nullary.snp -o nullary
  $ tlc nullary nullary
  No error has been found

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

Definitions are emitted in source order: a constant defined via an earlier
function must land after that function in the TLA+ module (grouping consts
before funs used to break define-before-use at SANY).

  $ cat > def_order.snp <<'EOF'
  > mod def_order {
  >   def double(k) = k * 2;
  >   def n = double(3);
  >   var ok = 0;
  >   procedure f() {
  >     await n == 6,
  >     ok = 1;
  >     return ();
  >   }
  >   fair process p = f in 1..1;
  > }
  > EOF
  $ cp either_guard.json def_order.json
  $ sanpou compile def_order.snp -o def_order
  $ tlc def_order def_order
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

String escaping end-to-end: sanpou strings are raw, so a backslash in a
literal (including a trailing one) and the quotes that `assert` embeds in
its message via the pretty-printed condition must all be escaped in the
emitted TLA+ — SANY would otherwise reject the spec.

  $ cat > str_esc.snp <<'EOF'
  > mod str_esc {
  >   var s = "a\b";
  >   procedure f() {
  >     assert s == "a\b";
  >     s = "x\";
  >     return ();
  >   }
  >   fair process p = f in 1..1;
  > }
  > EOF
  $ cp either_guard.json str_esc.json
  $ sanpou compile str_esc.snp -o str_esc
  $ tlc str_esc str_esc
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

Atoms and declarations share no namespace: a def named like a used atom is
renamed apart while the atom keeps its name, and both survive TLC.

  $ cat > atom_ns.snp <<'EOF'
  > mod atom_ns {
  >   def red = 999;
  >   var seen = 0;
  >   var tag = `nobody;
  >   procedure main() {
  >     seen = red,
  >     tag = `red;
  >     await seen == 999 && tag == `red,
  >     tag = `nobody;
  >     return ();
  >   }
  >   fair process p = main in 1..1;
  > }
  > EOF
  $ cp either_guard.json atom_ns.json
  $ sanpou compile atom_ns.snp -o atom_ns
  $ tlc atom_ns atom_ns
  No error has been found

Procedure calls in a record literal evaluate left to right in source order
(fields are canonicalized by label only in the type and the emitted TLA+,
never before call hoisting): pb's append lands before pa's.

  $ cat > rec_order.snp <<'EOF'
  > mod rec_order {
  >   var log = [];
  >   var r = {a: 0, b: 0};
  >   procedure pa() { log = append(log, 1); return 1; }
  >   procedure pb() { log = append(log, 2); return 2; }
  >   procedure f() {
  >     r = {b: pb(), a: pa()};
  >     assert log == [2, 1];
  >     return ();
  >   }
  >   fair process p = f in 1..1;
  > }
  > EOF
  $ cp either_guard.json rec_order.json
  $ sanpou compile rec_order.snp -o rec_order
  $ tlc rec_order rec_order
  No error has been found

Model values end-to-end: a state variable holds opaque atoms, transitions on
atom-equality guards and set membership, and runs to completion. TLC assigns
each atom a model value (from the generated .cfg), so termination confirms the
emitted CONSTANTs compare as distinct opaque values.

  $ cat > mv_check.snp <<'EOF'
  > mod mv_check {
  >   def states = {`idle, `busy};
  >   var state = `idle;
  >   procedure f() {
  >     while (state == `idle) {
  >       await state in states,
  >       state = `busy;
  >     }
  >     return ();
  >   }
  >   fair process p = f in 1..1;
  > }
  > EOF
  $ cp either_guard.json mv_check.json
  $ sanpou compile mv_check.snp -o mv_check
  $ tlc mv_check mv_check
  No error has been found

Maps keyed by a process ID set: per-process state (the idiomatic PlusCal
pattern) works for non-integer IDs too — each string-ID process bumps its
own slot and waits until all slots are set, then the module terminates.

  $ cat > strmap.snp <<'EOF'
  > mod strmap {
  >   def ids = {"a", "b"};
  >   var t = { x in ids -> 0 };
  >   procedure f() {
  >     t[self] = t[self] + 1;
  >     await forall (i in ids) { t[i] == 1 };
  >     return ();
  >   }
  >   fair process p = f in ids;
  > }
  > EOF
  $ cp either_guard.json strmap.json
  $ sanpou compile strmap.snp -o strmap
  $ tlc strmap strmap
  No error has been found

Processes over a non-integer ID set: the process ranges over a set of strings,
so ProcSet, pc, and stack are all keyed by strings and self has string type.
Both processes run to completion, so termination confirms the emitted spec
model-checks with non-integer process identities.

  $ cat > client_ids.snp <<'EOF'
  > mod client_ids {
  >   def clients = {"alice", "bob"};
  >   var count = 0;
  >   procedure client() {
  >     await self == self,
  >     count = count + 1;
  >     return ();
  >   }
  >   fair process cs = client in clients;
  > }
  > EOF
  $ cp either_guard.json client_ids.json
  $ sanpou compile client_ids.snp -o client_ids
  $ tlc client_ids client_ids
  No error has been found
