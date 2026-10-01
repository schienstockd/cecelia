# Audit — does a fresh output value_name sandbox an unattended agent run?

**Date:** 2026-10-01 · **Traced against:** `origin/main` @ `f99c27cf` · **Method:** read-only code
trace, not a run. **Plan this feeds:** [`docs/todo/AGENT_OVERNIGHT_PLAN.md`](../todo/AGENT_OVERNIGHT_PLAN.md).

## Question

An unattended agent ("here are my images — track everything, give me the behaviours") must not
damage the user's prior work. VN versioning ([`VN_VERSIONING_PLAN.md`](../todo/VN_VERSIONING_PLAN.md))
was built for that, but `version_write!` is wired into the 9 **pixel** writers only (ingest, the six
cleanup tasks, Flip, Dtype). Segmentation, tracking, behaviour and clustering never mint a `vN`.

So: does the **outer** axis — writing every output under a fresh value_name (e.g. `agent-2026-10-02`) —
already isolate a segment → measure → track → track measures → HMM / motif → cluster run?

## Answer

**Almost.** The per-step outputs are all keyed by value_name. Two things break the sandbox, and one
latent bug sat on the path.

## Per-step writes

| Step | Writes | Isolated? |
|---|---|---|
| Segment (cellpose / coastal / ridges) | `labels/{outVN}.zarr` + `ccid.labels[outVN]` via `register_label_files!` (`app/src/segmentation.jl:84`) | yes |
| Measure | `labelProps/{outVN}.h5ad` + `ccid.label_props[outVN]` (`tasks/segment/measure_labels.jl`) | yes — **but flips `_active`**, below |
| Track (btrack) | lineage cols into the **same** `labelProps/{vn}.h5ad` obs; reads `gating/{vn}.json` (`tasks/tracking/bayesian_tracking.jl:63-112`) | yes (in place on `vn`) |
| Track measures | `labelProps/{vn}__tracks.h5ad` (`tasks/tracking/track_measures.jl:413-494`) | yes |
| HMM states / transitions | obs cols into each pop's own `{vn}.h5ad` (`tasks/behaviour/hmm_states.jl:118-134`, `hmm_transitions.jl:76-91`) | yes **iff pops are vn-prefixed** |
| Motif discovery | obs into `{vn}.h5ad` + `{vn}__tracks.h5ad` + `.motiffeatures.json` sidecar (`tasks/behaviour/motif_discovery.jl:215-250`) | yes iff pops are vn-prefixed |
| clustPops / clustTracks | `clusters.{suffix}` + `obsm` into the pops' h5ad; QC banked as `{vn}.{suffix}` (`tasks/clustPops/cluster.jl:125-166`, `qc.jl:767`) | yes |
| QC | `qc/{fun}/{vn}` (`qc.jl:124-133`) | yes |
| `ccid.json` itself | `commit_state!` holds the image transaction (`model/project.jl:380`) | safe against a concurrent human |

## What breaks it

### L1 — the run moves `_active` (leak)

`measure_labels.jl` sets `label_props._active = outVN`; `versioned_set_field!` (default
`set_active=true`) and `tasks/task/composite.jl:315` do the same for `filepath`. After the run every
image's *default* segmentation and pixel version is the agent's. That default feeds:

- gating module + every value_name picker;
- `resolve_value_name(img)` fallbacks;
- un-prefixed pops in HMM / motif / clustTracks (`hmm_states.jl:91`, `motif_discovery.jl:135`,
  `clustTracks/cluster.jl:196-202`).

Nothing is lost, but the user's next session silently opens on the agent's output, and the only
"rollback" is re-pointing `_active` by hand.

### L2 — nothing refuses an existing value_name (missing guard)

`outputValueName = "default"` overwrites the user's segmentation in place; tracking / HMM / motif /
clustering write into whatever `vn` they are given. Outside the pixel writers under
`keep_previous_version`, the semantics are overwrite. Isolation holds only if the agent chooses a
fresh name — nothing enforces it.

### B1 — segmentation writers flattened versioned entries (latent bug, **fixed in this branch**)

`register_label_files!` rebuilt `labels` with `[string(v)]`, measureLabels rebuilt `label_props` with
`string(v)`, and `segment/branching.jl` rebuilt `branch_labels` the same way (found by recital's fanout
audit). Any versioned (dict-shaped) entry they passed over became a string — so the first step
toward versioning segmentation would have corrupted `ccid.json`. No writer produces such entries
yet, so no data was affected. Fix: both route through the new
`versioned_entry_overwrite!` (`app/src/helpers.jl`), which keeps every entry's shape and replaces only a
versioned target's `_latest` leaf. The two readers with the same flatten (measureLabels and
SegmentCorrect resolving `labels[vn]`) now unwrap through `unversion_value`. Test: `app/test/suite/vn_pilot_writer.jl` →
*versioned_entry_overwrite! — keeps every entry's shape*.

### B2 — toggle-off re-run flattens a versioned `filepath` (noted, **not fixed**)

With `keep_previous_version` off, `plan_versioned_target` (`app/src/helpers.jl`) returns the flat v1
path even when `filepath[vn]` is already versioned, and `versioned_filepath_write!` then calls
`versioned_set_field!`, which replaces the whole versioned entry with a scalar. Net effect of
"toggle on, re-run, toggle off, re-run": the v1 store is overwritten in place and `v2+` stay on disk
but drop out of `ccid.json` (so P5 prune can't see them). Needs a decision — probably "once an entry is
versioned, a re-run keeps versioning regardless of the toggle" — and touches the outer-axis writer's
20 callers, so it is left for its own change.

The same family, **live today** for anyone who has turned `keep_previous_version` on (found by
recital's fanout audit on this branch; not fixed here):

- `remove_image_version!` (`app/src/storage.jl:389`) reads `versioned_get_field(raw, "filepath", vn)`
  and joins `string(filename)` — on a versioned entry that is a Dict, so it logs "not found on disk",
  drops the entry, and orphans every `vN` store.
- `api_image_stores` (`api/src/image_geometry.jl:445`) sizes `filepath` the same way → 0 bytes, no
  codec, no levels for a versioned image. (Its `labels` loop is fixed here via `version_leaves`.)
- `composite.jl:315` and `versioned_filepath_write!` reach `versioned_set_field!`, which replaces a
  versioned entry whole.

Fix shape for the first two: walk `version_leaves(entry)` (added on this branch).

## What closes the sandbox (small)

1. **Autonomous runs don't move `_active`.** One condition at each of the three sites in L1. Accepting
   a run = flipping `_active` — which doubles as the missing promote / rollback lever.
2. **Autonomous runs refuse an existing value_name** for any output, and the agent's pops are always
   vn-prefixed so nothing resolves through `_active`.

Extending `version_write!` to segmentation / tracking / behaviour is **not** needed for a safe
overnight run; the outer axis already isolates. Keep it for the in-place re-run case.

## Not verified

The isolation above is what the code says. No segment → track → HMM pass has been run under a fresh
value_name to confirm it end to end — that is phase 1 of the plan.
