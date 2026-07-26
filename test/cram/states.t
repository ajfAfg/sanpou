State-count regression guard: pins the exact number of distinct states TLC
explores for every shipped example. Unlike wall-clock time, the
distinct-state count is fully deterministic and machine-independent, and it
is the direct driver of model-checking cost — so a compiler change that
makes checking slower (or faster) fails here and must update these numbers
via `dune promote`, making the change visible in review. Requires
SANPOU_JAVA and SANPOU_TLA2TOOLS_JAR (provided by mise; see AGENTS.md).

The private java.io.tmpdir keeps concurrent TLC processes from racing on
the standard modules tla2tools extracts under fixed names (#169).

  $ states() {
  >   mkdir -p "$1/jtmp"
  >   "$SANPOU_JAVA" -cp "$SANPOU_TLA2TOOLS_JAR" -XX:+UseParallelGC \
  >     -Djava.io.tmpdir="$1/jtmp" tlc2.TLC \
  >     -config "$1/$2.cfg" -metadir "$1/states" -workers 1 "$1/$2.tla" \
  >     > "$1/$2.raw" 2>&1
  >   summary=$(grep -Eo '[0-9]+ distinct states found, 0 states left on queue' "$1/$2.raw" | tail -n 1)
  >   if [ -n "$summary" ]; then echo "$summary"; else cat "$1/$2.raw"; fi
  > }

  $ sanpou compile ../../example/barrier.snp -o barrier
  $ states barrier bakery_algorithm
  6129 distinct states found, 0 states left on queue

  $ sanpou compile ../../example/conditional_variable.snp -o cv
  $ states cv conditional_variable
  511 distinct states found, 0 states left on queue

  $ sanpou compile ../../example/factorial.snp -o factorial
  $ states factorial factorial
  28 distinct states found, 0 states left on queue

  $ sanpou compile ../../example/rwlock.snp -o rwlock
  $ states rwlock rwlock
  29820 distinct states found, 0 states left on queue

  $ sanpou compile ../../example/semaphore.snp -o semaphore
  $ states semaphore semaphore
  64 distinct states found, 0 states left on queue
