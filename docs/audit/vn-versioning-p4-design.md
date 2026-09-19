# VN versioning P4 — design analysis (2026-09-19)

Sibling to `docs/audit/vn-versioning-touchpoints.md`. Written after P1a/P1b/P1c/P1d and P2 infra
shipped; the pre-P1 "P4" scope was written before those primitives existed and needs revisiting.

**Design decisions locked with Dominik on 2026-09-19 (see § *Locked decisions* below). This doc
records the analysis; the pilot-writer PR ships the code.**

## The *why* — guardrails for autonomous execution

The chain-execution-prerequisites prompt (`docs/archive/chain-execution-prerequisites-prompt.md`)
asked for two things this primitive answers together:

- **Non-destructive default** — a Claude that autonomously runs a task on the user's data must not
  be able to destroy their prior work.
- **Mechanically-can't-overwrite invariant** — the safety is enforced by the writer, not by
  convention.

The `version_write!` guarded writer (P2 infra) is that mechanical enforcement — it *refuses* to
overwrite an existing version. Autonomous execution routes through it unconditionally; the same
knob is exposed as a user-facing Settings toggle for humans who want the guardrail for niche use
cases (A/B comparison, publication freeze, chain branching, regression investigation).

This shifts the framing away from "versioning is a feature users pick": **versioning is safety
scaffolding for autonomous execution**, exposed to humans as an opt-in Settings toggle because if
the machinery exists, hiding it entirely would be wrong.

## Post-P1+P2 state — the "18 helpers" split into two buckets

The pre-P1 audit called out 18 hand-joining helpers as P4b targets. After P1b routed the four
struct-backed helpers via `unversion_value`, those split into:

### Bucket A — already routed (4 helpers, no P4b work needed)

- `app/src/model/image.jl:101` (`img_filepath`)
- `app/src/model/image.jl:124` (`img_label_props_path`)
- `app/src/model/image.jl:149` (`img_labels_path`)
- `app/src/model/image.jl:373` (`img_branch_labels_path`)

They read whichever filename is recorded for the requested version. As long as writers store
DISTINCT filenames per version, these helpers work unchanged.

### Bucket B — hand-composed, but won't get per-version files (14 helpers)

Under the locked answer to Q2 (below), Bucket B stays single-file per (image, value_name) and
follows `_latest` implicitly. **No P4b sweep needed** — safety for these families comes from
chain/task-level pinning (P3), not per-artifact version files.

| Site | Current path shape | Family |
|---|---|---|
| `app/src/model/image.jl:298` (`img_track_props_path`) | `labelProps/{vn}__tracks.h5ad` | trackProps |
| `app/src/model/image.jl:336` (`img_branch_props_path`) | `labelProps/{vn}__branch.h5ad` | branchProps |
| `app/src/gating/popmanager/persistence.jl:132` (`gating_path`) | `gating/{vn}[__suffix].json` | gating |
| `app/src/gating/popmanager/clustering_colour.jl:13` (`_clustfeatures_path`) | inherits label_props | clustfeatures |
| `app/src/tracking/track_correction.jl:345` (`corrections_path`) | `corrections/{vn}.json` | corrections |
| `app/src/label_correction.jl:192` (`_label_corrections_path`) | `corrections/labels_{vn}.json` | corrections |
| `app/src/correction_staleness.jl:158` | `corrections/{vn}.staleness.json` | corrections |
| `app/src/qc.jl:25` (`qc_path`) | `qc/{fun}/{vn}.json` | qc |
| `app/src/qc_cohort.jl:202` (`cohort_qc_path`) | `qc/cohort/{fun}/{vn}.json` (set scope) | qc |
| `python/cecelia/utils/label_props_utils.py:392-394` (`label_props_filepath`) | `labelProps/{vn}.h5ad` | labelProps |
| `python/cecelia/utils/tracking_utils.py:106` | `labelProps/{vn}.h5ad` | labelProps |
| `python/cecelia/utils/measure_utils.py:470` | `{output_value_name}.h5ad` | labelProps |
| `python/cecelia/utils/segmentation_utils.py:295-299` (`_store_path`) | `labels/{outputValueName}[_ma].zarr` | labels |
| `python/cecelia/utils/store_sweep.py:110-113` | scans ccid.json (schema-aware) | (schema) |

## Locked decisions

### Q1 — On-disk layout — **per-version subdir**

`{root}/{vn}/v{N}/{filename}`. Zarrs and files both nest under a `default/v1/`, `default/v2/`, …
subdirectory. Migration promotes file → dir but is deferred (see § *Migrator scope*).

**Only Bucket A families use this layout** (filepath initially, plus labels / label_props /
branch_labels when their writers convert). Bucket B stays flat.

### Q2 — Bucket B stays single-file, follows `_latest`

QC, corrections, gating, clustfeatures, trackProps, branchProps do **not** get per-version files on
disk. They keep their current `{kind}/{vn}.json` / `{kind}/{vn}.h5ad` layout and always resolve
against the value_name's `_latest` version.

**Caveat — the safety story hangs on chain/task-level pinning (P3).** If a task creates label v2
while gating still points at v1's cell IDs, gating silently applies to v2's (different) cells.
This is prevented by:

- **Chain runs**: P3 pins `vn@v` at plan time; every downstream artifact resolves inputs against
  the pinned version, not `_latest`.
- **Standalone Claude task runs**: the same pin-at-invocation rule must hold — an autonomous task
  reads specific versions and writes downstream artifacts against the versions it read.

### Q3 — Re-import behavior — **default overwrite; Settings toggle opts into versioning**

- **Toggle off (default)**: current behavior — every re-run overwrites the store at the current
  path.
- **Toggle on**: re-runs route through `version_write!` — new file at
  `default/v{N+1}/ccidImage.ome.zarr`, ccid.json gets a new version entry, prior version stays.

**Autonomous execution forces the toggle on** — a Claude-triggered run always creates a new
version regardless of the user's setting. That's the "mechanically can't overwrite" invariant.

Toggle placement: **Settings → Storage → "Reprocessing keeps previous version" (default off)**.
Not a headline feature; most users never touch it. Same knob autonomous execution flips
programmatically.

**Scenarios where a human turns it on:**
- A/B parameter comparison (cellpose model A vs B; drift correction with different ref channels)
- Publication freeze (results under review; iterate further without losing the exact output)
- Chain branching (rerun drift-correct with new params; keep the old drift's tracking intact)
- Regression investigation (QC finding shifted between runs; keep both to diff)
- Sharing intermediates (v1 was the "raw import" a collaborator wants)

Non-scenarios (overwrite is right):
- Fixing a broken run
- Routine reprocessing after a bug fix
- Everyday exploratory iteration
- Space-constrained workstations

## Direction — leaner path (pilot writer now, no full P4b sweep)

Under Q2's answer (Bucket B stays flat), P4b's helper sweep collapses to zero. The pilot writer
for `filepath` (ingest) ships against Bucket A alone — which is already routed.

**Ingest writer conversion (`app/src/tasks/importImages/omezarr/ccid_sync.jl:91`):**

- Fresh import (no prior entry): current behavior — write `ccidImage.ome.zarr` (flat), record
  `filepath[default] = "ccidImage.ome.zarr"` via `versioned_set_field!`. Ccid.json shape stays
  legacy-scalar. **No forced schema change on greenfield projects.**
- Re-import with toggle off: overwrite in place (current behavior — no change).
- Re-import with toggle on **or** autonomous run:
  - `versioned_upgrade_entry!(raw["filepath"], value_name)` wraps the legacy scalar as `v1` in
    place (P2 infra composer).
  - Write the new zarr under `default/v{N+1}/ccidImage.ome.zarr` — a per-version subdirectory
    sibling to the flat v1.
  - `version_write!(inner, "default/v{N+1}/ccidImage.ome.zarr")` — appends the version, updates
    `_latest`, refuses collision (D6).

**On-disk convention specific to filepath:**
- v1 path stays flat (`ccidImage.ome.zarr`) — legacy-friendly.
- v2, v3, … live in `default/v{N}/ccidImage.ome.zarr` subdirs.
- The `versioned_get_field_at` composer + `unversion_value` unwrap this without reader changes.

## Migrator scope — deferred

Since the P2 infra's union type supports both shapes forever, an eager migrator has no clear
runtime benefit. Legacy projects work unchanged; the ccid.json schema flips to versioned shape JIT
when the first `versioned_upgrade_entry!` runs (which happens on the first re-import with the
toggle on).

**Not shipping a migrator.** Filesystem files stay in place until an actual re-run.

## Related memory pointers

- `project-vn-versioning-p3-pinning` — chain-planner pinning (agreed 2026-09-19)
- `project-vn-versioning-p5-prune` — prune UX (agreed 2026-09-19)
- `project-vn-versioning-decisions` — Q1–Q3 locks + autonomous-execution framing
