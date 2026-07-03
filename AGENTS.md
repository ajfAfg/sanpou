# Agent Guide

Read `README.md` before working in this repository. It is the source of truth for setup, build, test, and CLI usage.

## TLC and E2E tests

The E2E tests (`test/sanpou/test_e2e.ml`) run the TLC model checker and are
skipped unless `SANPOU_JAVA` and `SANPOU_TLA2TOOLS_JAR` are set. This
repository provides both through mise (`[env]` in `mise.toml`):

- `SANPOU_JAVA` points to the mise-installed Java (`[tools]` in `mise.toml`).
- `SANPOU_TLA2TOOLS_JAR` points to `tla2tools.jar` in the project root.
  Download it with `mise run download-tla2tools`.

The `tla2tools.jar` version is pinned in `[vars]` in `mise.toml` — it is
fixed to a specific release, not whatever is latest. Bump the pin
deliberately when upgrading TLA+ tools.

Run tests through mise so the environment applies, e.g. `mise x -- dune
runtest`. In shells where mise is activated, plain `dune runtest` works too.
