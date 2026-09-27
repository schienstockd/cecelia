# AI-assist infrastructure — effectiveness

_Rendered 2026-09-26T07:35:59Z — no events logged yet._

The AI-assist infrastructure (pre-commit fanout audit, CLAUDE.md ratchets, convention-check reviewer) writes structured rows to `~/.cecelia-effectiveness/events.jsonl` as it runs. This page renders those rows into an aggregate view of catch rate, false-positive rate, and what the infrastructure structurally cannot measure.

See [`EFFECTIVENESS_METHODOLOGY.md`](EFFECTIVENESS_METHODOLOGY.md) for the schema, event taxonomy, sample method, and the honest ceiling on what this log can and cannot tell you.

## What this log cannot measure

- **Silent misses.** Bugs shipped and never noticed. Unbounded, unmeasurable.
- **Cross-module private-helper cloning.** The convention-check reviewer excludes this class — opaque names don't respond to synonym greps; detection needs semantic-similarity indexing.
- **Counterfactual attribution.** "The audit flagged X, and the author fixed it" is measurable; "this bug would have shipped without the audit" is not — the author might have noticed anyway.
- **Selection bias in retrospective rows.** Merged-PR samples miss the ones that got closed unreviewed.
- **Novelty decay.** Ratchets productive at N=0 may become noise at N=100 as the codebase adapts around them. Rising FP rate over time is a signal to read, not to aggregate.
