(* Dead-local canonicalization: after a backward liveness analysis, frame
   locals that are dead following an action are pinned to the
   [defaultInitValue] sentinel (dead writes rewritten, dead call results
   discarded, resets riding along in existing actions), so states no longer
   differ by values no behavior can read. *)

val canonicalize_module : Ir.module_ir -> Ir.module_ir
