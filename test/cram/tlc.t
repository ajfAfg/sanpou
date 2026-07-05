Model checking through the sanpou CLI: compile with sanpou, then run TLC on
the result. Requires SANPOU_JAVA and SANPOU_TLA2TOOLS_JAR (provided by mise;
see AGENTS.md) — without them this test is skipped via enabled_if.

Only the TLC verdict is asserted; the full TLC output is dumped when the
verdict is neither a clean pass nor a deadlock, so failures stay debuggable.

  $ tlc() {
  >   "$SANPOU_JAVA" -cp "$SANPOU_TLA2TOOLS_JAR" -XX:+UseParallelGC tlc2.TLC \
  >     -config "$1/$2.cfg" -metadir "$1/states" -workers 1 "$1/$2.tla" \
  >     > "$1/$2.raw" 2>&1
  >   verdict=$(grep -Eo 'No error has been found|Deadlock reached' "$1/$2.raw" | head -n 1)
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
  >   fn fact(n) {
  >     if (n == 0) {
  >       return 1;
  >     } else {
  >       var ans = n * fact(n - 1);
  >       return ans;
  >     }
  >   }
  >   fn f() {
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
  >   fn f() {
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
  >   fn fa() {
  >     while (true) {
  >       await b == true,
  >       a = true;
  >     }
  >   }
  >   fn fb() {
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
