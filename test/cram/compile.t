`sanpou compile` turns each example into a .tla module, a source map, and —
when a sidecar .json config exists — a .cfg file.

  $ for ex in barrier conditional_variable factorial rwlock semaphore; do
  >   sanpou compile ../../example/$ex.snp -o out_$ex || echo "FAILED: $ex"
  > done
  $ ls out_*
  out_barrier:
  bakery_algorithm.cfg
  bakery_algorithm.sourcemap.json
  bakery_algorithm.tla
  
  out_conditional_variable:
  conditional_variable.cfg
  conditional_variable.sourcemap.json
  conditional_variable.tla
  
  out_factorial:
  factorial.cfg
  factorial.sourcemap.json
  factorial.tla
  
  out_rwlock:
  rwlock.cfg
  rwlock.sourcemap.json
  rwlock.tla
  
  out_semaphore:
  semaphore.cfg
  semaphore.sourcemap.json
  semaphore.tla

Golden output for factorial: a small example that still exercises recursion,
frame-resident locals, parameters, the process wrapper, temporal properties,
and termination checking.

  $ cat out_factorial/factorial.tla
  ---- MODULE factorial ----
  EXTENDS TLC, Sequences, Integers
  
  CONSTANT defaultInitValue
  
  VARIABLES pc, x, stack
  
  vars == << pc, x, stack >>
  
  testing == <>((x = 120))
  
  ProcSet == ({1})
  
  Init ==
      /\ x = 0
      /\ stack = [self \in ProcSet |-> << >>]
      /\ pc = [self \in ProcSet |-> "__w_workers_entry__"]
  
  L1(self) ==
      /\ pc[self] = "L1"
      /\ pc' = [pc EXCEPT ![self] = IF (Head(stack[self]).x__1 = 0) THEN "L2" ELSE "L5"]
      /\ UNCHANGED << stack, x >>
  
  L2(self) ==
      /\ pc[self] = "L2"
      /\ stack' = [stack EXCEPT ![self] = << [value |-> 1] >> \o Tail(stack[self])]
      /\ pc' = [pc EXCEPT ![self] = Head(stack[self]).return_pc]
      /\ UNCHANGED << x >>
  
  L5(self) ==
      /\ pc[self] = "L5"
      /\ stack' = [stack EXCEPT ![self] = << [procedure |-> "fact", return_pc |-> "L6", x__1 |-> (Head(stack[self]).x__1 - 1), callRet__1 |-> defaultInitValue, ans__2 |-> defaultInitValue] >> \o stack[self]]
      /\ pc' = [pc EXCEPT ![self] = "L1"]
      /\ UNCHANGED << x >>
  
  L6(self) ==
      /\ pc[self] = "L6"
      /\ stack' = [stack EXCEPT ![self] = [Tail(stack[self]) EXCEPT ![1].callRet__1 = Head(stack[self]).value]]
      /\ pc' = [pc EXCEPT ![self] = "L4"]
      /\ UNCHANGED << x >>
  
  L4(self) ==
      /\ pc[self] = "L4"
      /\ stack' = [stack EXCEPT ![self] = << [value |-> (Head(stack[self]).x__1 * Head(stack[self]).callRet__1)] >> \o Tail([stack[self] EXCEPT ![1].ans__2 = (Head(stack[self]).x__1 * Head(stack[self]).callRet__1)])]
      /\ pc' = [pc EXCEPT ![self] = Head(stack[self]).return_pc]
      /\ UNCHANGED << x >>
  
  fact(self) == L1(self) \/ L2(self) \/ L5(self) \/ L6(self) \/ L4(self)
  
  L9(self) ==
      /\ pc[self] = "L9"
      /\ stack' = [stack EXCEPT ![self] = << [procedure |-> "fact", return_pc |-> "L10", x__1 |-> 5, callRet__1 |-> defaultInitValue, ans__2 |-> defaultInitValue] >> \o stack[self]]
      /\ pc' = [pc EXCEPT ![self] = "L1"]
      /\ UNCHANGED << x >>
  
  L10(self) ==
      /\ pc[self] = "L10"
      /\ stack' = [stack EXCEPT ![self] = [Tail(stack[self]) EXCEPT ![1].callRet__2 = Head(stack[self]).value]]
      /\ pc' = [pc EXCEPT ![self] = "L8"]
      /\ UNCHANGED << x >>
  
  L8(self) ==
      /\ pc[self] = "L8"
      /\ x' = Head(stack[self]).callRet__2
      /\ stack' = [stack EXCEPT ![self] = << [value |-> << >>] >> \o Tail(stack[self])]
      /\ pc' = [pc EXCEPT ![self] = Head(stack[self]).return_pc]
  
  worker(self) == L9(self) \/ L10(self) \/ L8(self)
  
  __w_workers_entry__(self) ==
      /\ pc[self] = "__w_workers_entry__"
      /\ stack' = [stack EXCEPT ![self] = << [procedure |-> "worker", return_pc |-> "__w_workers_discard__", callRet__2 |-> defaultInitValue] >> \o stack[self]]
      /\ pc' = [pc EXCEPT ![self] = "L9"]
      /\ UNCHANGED << x >>
  
  __w_workers_discard__(self) ==
      /\ pc[self] = "__w_workers_discard__"
      /\ stack' = [stack EXCEPT ![self] = Tail(stack[self])]
      /\ pc' = [pc EXCEPT ![self] = "Done"]
      /\ UNCHANGED << x >>
  
  __process_workers_wrapper__(self) == __w_workers_entry__(self) \/ __w_workers_discard__(self)
  
  Terminating ==
      /\ \A self \in ProcSet: pc[self] = "Done"
      /\ UNCHANGED vars
  
  Termination == <>(\A self \in ProcSet: pc[self] = "Done")
  
  Next ==
      \/ (\E self \in ProcSet: fact(self) \/ worker(self))
      \/ (\E self \in {1}: __process_workers_wrapper__(self))
      \/ Terminating
  
  Spec == Init /\ [][Next]_vars /\ (\A self \in {1}: WF_vars(__process_workers_wrapper__(self))) /\ (\A self \in {1}: WF_vars(fact(self))) /\ (\A self \in {1}: WF_vars(worker(self)))
  
  ====
  $ cat out_factorial/factorial.cfg
  SPECIFICATION Spec
  
  CONSTANT defaultInitValue = defaultInitValue
  
  CHECK_DEADLOCK TRUE
  
  PROPERTIES
      testing
      Termination
  $ cat out_factorial/factorial.sourcemap.json
  { "version": 2, "module": "factorial", "labels": [
      { "label": "L1", "proc": "fact", "desc": "if (x == 0) [check]", "line": 7, "col": 5 },
      { "label": "L2", "proc": "fact", "desc": "return 1", "line": 8, "col": 7 },
      { "label": "L5", "proc": "fact", "desc": "[call fact]", "line": 10, "col": 21 },
      { "label": "L6", "proc": "fact", "desc": "[return from fact]", "line": 10, "col": 21 },
      { "label": "L4", "proc": "fact", "desc": "var ans = x * callRet__1; return ans", "line": 10, "col": 7 },
      { "label": "L9", "proc": "worker", "desc": "[call fact]", "line": 16, "col": 9 },
      { "label": "L10", "proc": "worker", "desc": "[return from fact]", "line": 16, "col": 9 },
      { "label": "L8", "proc": "worker", "desc": "x = callRet__2; return ()", "line": 16, "col": 5 },
      { "label": "__w_workers_entry__", "proc": "workers", "desc": "[process workers starts worker]", "line": 20, "col": 3 },
      { "label": "__w_workers_discard__", "proc": "workers", "desc": "[process workers finished]", "line": 20, "col": 3 }
  ]
  , "vars": [
      { "tla": "x", "name": "x", "kind": "global" },
      { "tla": "x__1", "name": "x", "proc": "fact", "kind": "param" },
      { "tla": "callRet__1", "name": "callRet__1", "proc": "fact", "kind": "callret", "callee": "fact" },
      { "tla": "ans__2", "name": "ans", "proc": "fact", "kind": "local" },
      { "tla": "callRet__2", "name": "callRet__2", "proc": "worker", "kind": "callret", "callee": "fact" }
  ]
   }
