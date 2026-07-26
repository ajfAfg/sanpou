# Temporal properties

## `property`

`property name = ...;` declares a temporal property:

```sanpou
property counted = finally(count > 0);
property bounded = globally(forall (i in 1..n) { grid[i][1] >= 0 });
```

A `property` definition is the only place `globally(p)` / `finally(p)` may
appear, and only properties may reference other properties.

## Checking properties with TLC

Properties are not checked unless listed: put property names in the sidecar
config's `properties`, and plain boolean `def`s in `invariants` — an
invariant is cheaper for TLC than the equivalent `globally(...)` property.
See the [sidecar config section of the README](../../README.md#sidecar-config)
for the config format.
