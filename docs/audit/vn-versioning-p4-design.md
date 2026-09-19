# VN versioning P4 — design analysis (2026-09-19)

Sibling to `docs/audit/vn-versioning-touchpoints.md`, focused on P4 (helpers sweep + migrator).
Written after P1a/P1b/P1c/P1d and P2 infra shipped — the original P4 sizing was pre-P1 and now
splits into two buckets with different work profiles. Surfaces design questions the user needs to
sign off on before P4b coding starts.

**This is a design doc — no code changes in the PR that carries it. Once questions Q1–Q3 below are
answered, P4b sweeps ship one family per PR.**

## What the post-P1+P2 state actually looks like

The pre-P1 audit ("18 legacy path-joining helpers") splits into TWO buckets now:

### Bucket A — already routed (4 helpers, no P4b work needed)

The four struct-backed helpers in `app/src/model/image.jl` already unwrap versioned entries via
`unversion_value(entry, version)`, shipped in P1b:

- `img_filepath` (line 101)
- `img_label_props_path` (line 124)
- `img_labels_path` (line 149)
- `img_branch_labels_path` (line 373)

They read whichever filename is recorded for the requested version. As long as writers store
DISTINCT filenames per version, these helpers work unchanged — no rewrite needed.

### Bucket B — not yet routed (14 helpers)

These COMPOSE paths inline from `{vn}` templates with no version segment. Version is not part of
the on-disk path today. Each needs a writer that produces a distinct file per version AND a reader
routing update.

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

## Design questions before P4b

Each family needs a per-version convention. The plan text ("`default/<vn>/<contents>` →
`default/<vn>/v1/<contents>`") suggests promoting the value_name to a directory and adding a
version subdir. But per-family semantics differ enough that a single convention may not fit all.

### Q1 — On-disk convention per family

Two shapes to pick between:

- **A. Filename suffix**: `default_v1.h5ad`, `default_v1.zarr/`, `default_v1.json`. Minimal
  directory changes. Migration is a rename in place.
- **B. Per-version subdirectory**: `default/v1.h5ad`, `default/v1.zarr/`, `default/v1.json`.
  Cleaner tree, matches plan doc phrasing. Migration promotes file → dir.

Recommendation: **A (suffix)**. Zarrs are directories so `default_v1.zarr/` is fine. `.h5ad`/`.json`
stay files. Migration is a rename, not a promote. Readers that already receive
filenames-from-ccid work unchanged (`img.label_props[vn][v1] = "default_v1.h5ad"`, `unversion_value`
returns the string, `img_label_props_path` joins it — no code change to the reader). B is prettier
but doubles the migration risk (create dir + move file + handle collisions).

### Q2 — Does every family truly need per-version files?

- **filepath, labels, label_props, branch_labels** — YES. Reproducibility hinges on distinct stores
  per run. Bucket A already ready.
- **QC** — PROBABLY YES. Each run's QC is independent history the user might want to compare
  (v1 vs v2 segmentation counts). Fits `_v1`/`_v2` cleanly.
- **corrections** — UNCLEAR. Corrections are a JOURNAL that appends across runs. If v1's labels
  get corrections and v2 is a fresh re-segmentation, do v2's corrections start fresh or carry over?
  Two coherent answers: (a) per-version file (`corrections/default_v1.json`, fresh journal per
  version) — cleanest; (b) single journal per (image, vn) — matches today's semantics.
- **gating** — UNCLEAR. Gating is task-scoped (`{task_dir}/gating/…`), not image-scoped. Task dir
  already versions per run. May not need vn version.
- **clustfeatures** — INHERITS from label_props (path is derived by extension swap). Whatever
  label_props gets, clustfeatures follows.

### Q3 — Re-import behavior (the pilot writer's UX question)

When a user re-runs ingest on an already-imported image, does the writer:

- **A. Default overwrite v1**; explicit opt-in flag `asNewVersion=true` mints v2. Matches every
  other task's re-run semantic today.
- **B. Default mint v2**; explicit opt-in flag `overwriteVersion=true` destroys v1, writes v1 fresh.
  Safer (never loses data).

Recommendation: **A**. Users who want v1 preserved tick a box; users just fixing a broken import
don't get surprise v2 clutter. Chain re-plans control which version chains READ via the
freeze-vs-auto toggle (already decided) — orthogonal to write semantics.

## Reconsidered path — pilot writer may not need full P4 sweep

**The earlier blocker analysis (in memory: `project-vn-versioning-p2-pilot-blocked`) was based on
"writers overwrite same relative path". That's fixable in the writer itself by producing DISTINCT
filenames per version.** Bucket A is already routed, so a pilot writer for `filepath` (ingest) can
ship WITHOUT the Bucket B sweep — as long as ingest writes a distinct filename per version.

Concretely, pilot writer for `ccid_sync.jl:91`:

- Fresh import: write zarr to `ccidImage.ome.zarr`, record `filepath[default][v1] =
  "ccidImage.ome.zarr"` via `versioned_upgrade_entry!` + `version_write!`.
- Re-import `asNewVersion=false` (default): write to `ccidImage.ome.zarr`, still record v1
  (overwrite semantics preserved).
- Re-import `asNewVersion=true`: write to `ccidImage_v2.ome.zarr`, record
  `filepath[default][v2]`.

The 14 Bucket B helpers are UNRELATED to the filepath pilot — each family's sweep is a
separate, self-contained PR that lands when that family gets its own writer conversion.

## Requested decision — full-P4 vs leaner path

**Path 1 — full-P4 first (what "C properly" asked for).** Sweep Bucket B (14 helpers, one family
per PR, per your Q1/Q2 answers) BEFORE the pilot writer. Bigger surface, safer landing, no
inconsistency where filepath is versioned but sidecars aren't.

**Path 2 — leaner (emerged from this re-analysis).** Pilot writer for filepath ships NOW with the
`asNewVersion` opt-in (Q3=A). Bucket B sweeps happen when each family needs versioning. Smaller
surface, more incremental. Filepath becomes versioned while QC/corrections/gating stay
un-versioned — but that's already the ccid.json schema state today (fields have their union type
for exactly this).

Both are coherent. Path 1 is what you asked for; Path 2 might be what you actually want given how
self-contained Bucket A already is post-P1.

If Path 1: I need answers to Q1 (suffix vs subdir) + Q2 (per-family) before writing any P4b PR.
If Path 2: I need Q3 (default overwrite vs default new-version) before writing the pilot writer PR.

## Migrator scope (either path)

The ccid.json legacy → versioned wrapper is trivial and could ship independent of the path
decision: wrap every legacy bare scalar/vector in the 4 versioned struct fields as `{v1: value,
_latest: "v1"}` via existing `versioned_upgrade_entry!`, at project open. Idempotent, no file moves.

**But — since the P2 infra already supports both shapes forever (via the widened union type), an
eager migrator has no clear runtime benefit over the JIT wrap at first-write.** Skipping it keeps
the "user projects work exactly as before until a writer opts in" property. I'd only add it if you
want the on-disk shape canonicalised for a downstream reason (schema clarity? reduced conditional
branches?) — otherwise JIT is enough.

## Related memory pointers

- `project-vn-versioning-p3-pinning` — chain planner design already agreed
- `project-vn-versioning-p5-prune` — prune UX already agreed
- `project-vn-versioning-p2-pilot-blocked` — the blocker analysis this doc replaces
