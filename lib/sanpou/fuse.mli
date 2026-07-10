(* Action fusion: composes process-local actions with their successors
   (static partial-order reduction) so the emitted TLA+ has fewer
   interleaving points, at most one shared access per action. Preserves
   everything TLC checks through the sidecar: invariants, temporal
   properties over module variables, and deadlock existence. *)

val fuse_module : Ir.module_ir -> Ir.module_ir
