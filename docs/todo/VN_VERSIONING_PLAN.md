# Value-name versioning — every writer targets a new `vn@v`, never overwrites

**Status:** **P1a + P1b + P1c + P1d + P2 infra shipped** (2026-09-18) — helpers + composer + Julia
app-layer + Julia API-layer + Python resolver + Python reader routing + guarded writer composer +
widened CciaImage field types, additive, backward-compat verified via full test suite. P2 pilot
writer + P3–P6 planning. Comes out
of the chain-execution-prerequisites prompt
(`docs/archive/chain-execution-prerequisites-prompt.md`) — closing items **#1** (non-destructive
default) and **#3** (mechanically-can't-overwrite invariant) collapses into this one primitive.
Worth building on its own merit for human-triggered runs; not conditional on ever granting execution
rights.

**Grounded by `docs/audit/vn-versioning-touchpoints.md` (2026-09-18).** The pre-audit touchpoint
estimate below was 3–5× too low on callsite counts; the geometry (few primitives, many callsites)
still holds. See §*Touchpoints* for the corrected numbers and §*Reservations from the audit* for the
three revisions to the phasing that fell out.

## Goal

Every chain-produced output — zarr image store, zarr label store, `.h5ad`, `gating/{vn}.json`, and
any other artifact keyed by `value_name` — is written to a new *version* of that `vn`. Existing
versions stay on disk until an **explicit** prune. In-place overwrite becomes impossible at the
writer, not by convention.

**Explicit non-goal.** This is not project-directory snapshotting (the old-R "duplicate the data
dir" shape). It is per-`value_name` versioning at the writer level.

## Why now / why this shape

- The R version's whole-project-dir shape doesn't fit. Store sizes are multi-GB per image; on a
  per-image chain (which is the concurrency model the scheduler already runs on), a 4-image set
  with denoise + seg + tracking + gating would 8×–16× disk cost. ~99% of the bytes are raw input
  that doesn't move — a whole-dir copy pays the invariant part to protect the delta.
- The primitive already exists in shape. `ccid.json` versioned-fields is content-hashed history for
  small values, and `value_name` is the addressable axis every writer already uses. Extending that
  pattern is cheaper than inventing a new one, and the chain-execution prompt asks for exactly this
  ("the same content-hashed-history pattern chain templates already use").
- Filesystem snapshots (btrfs / zfs / APFS clone) are ruled out on Windows — see *Non-goals*.

## Locked decisions

### D1 — Monotonic per-vn integer, not a content hash

Version is `1, 2, 3, …` per `value_name`. Not a hash.

- Content hash needs the store to exist before the path is known — chicken-and-egg for a directory
  of chunks written incrementally.
- Monotonic ints are readable in the file browser and stable at write-plan time.
- Hash-of-tree for integrity is a separate future concern, orthogonal to addressing (see *Open
  questions*).

### D2 — `latest` pointer in `ccid.json`, per (vn, kind)

Every reader that asks for `vn` unqualified resolves to `latest`. The pointer lives in `ccid.json`
under a versioned-field, one entry per `(value_name, kind)`. Promoting a run's output to `latest`
is one atomic ccid patch.

### D3 — Chain runs pin their inputs at plan time

When a chain node's params say `input_value_name: "cells"`, the run **freezes** that to `cells@3`
(whatever `latest` was at plan time). Re-running upstream later cannot silently change what a
running downstream node sees. Lockfile-vs-manifest.

### D4 — Directory layout: `default/<vn>/v3/…`, not `default/<vn>@3/…`

Grouped by vn.

- Prune is `rm -rf default/<vn>/v2/`, no glob.
- One directory per vn in the file browser, not N sibling dirs cluttering the root.
- Backward compat is a one-shot migration: `default/<vn>/<contents>` becomes
  `default/<vn>/v1/<contents>` (P4).

### D5 — Prune is explicit only, at least at first

No age-based or count-based auto-eviction until real disk numbers say otherwise. First
implementation ships a Settings → Storage → "Prune versions" surface + a `POST /api/versions/prune`
route. Auto policies are a follow-up plan, not this one.

### D6 — The write path refuses to target an existing `(vn, version)`

`zarr_utils.staged_store`, `write_h5ad_atomic`, and the Julia writers (`write_atomic` for gating
JSON) refuse to promote to `default/<vn>/vN/` if that directory exists. Enforcement lives in the
writer, not in a caller check. This is item **#3** of the chain-execution list; it falls out for
free once D1+D2 are in place — no separate mechanism.

### D7 — Kinds covered

All of these are versioned per `(vn, kind)`:

- Image stores (`default/<vn>/vN/` — image data)
- Label stores (`default/<vn>/vN/` — label data)
- Branch label stores (`branchLabels/…` — separate ccid key `img.branch_labels`)
- `.h5ad` files: cell (`{vn}.h5ad`), tracks (`{vn}__tracks.h5ad`), branch (`{vn}__branch.h5ad`)
- `gating/{vn}[__suffix].json`
- Per-(vn, run) sidecars written via `write_json_atomic`: `qc/{fun}/{vn}.json`,
  `corrections/{vn}.json`, `corrections/labels_{vn}.json`, `corrections/{vn}.staleness.json`,
  `{props}.clustfeatures.json`, motif sidecar
- Per-vn image attrs written by `omezarr/calibration.jl` (`.zattrs` + OME-XML under `default/<vn>/…`)

**Not versioned here** (audit-confirmed): raw input; `ccid.json` itself (schema grows fields per D2
but the file is project-scope); `spatialGraph/{suffix}.h5ad` and `spatialStats/{suffix}.json`
(keyed by run suffix, not vn); `plan.json` (per image, not per vn); `runlog.json`, `project.json`;
denoise `{name}.json` model manifest (project-scope under `<config_dir>/models/denoiseModels/`).

## Phases

Each phase is an independently-shippable PR.

### P1a — Schema helpers + composer, additive — **SHIPPED** (2026-09-18)

Split out from P1 before writing code, because the schema decision is load-bearing.

- `_latest` sentinel + `version_latest` / `version_get` / `version_set!` / `version_keys` +
  `is_versioned_entry` in `app/src/helpers.jl`, symmetric to the existing outer `versioned_*` set.
- Composer `versioned_get_field_at(raw, field, value_name; version=nothing)` walks BOTH axes:
  returns the leaf value on new-shape entries, the entry unchanged on legacy (bare scalar / vector),
  `nothing` on missing.
- **Additive only.** Struct field types unchanged; no writer produces new-shape entries yet; every
  existing reader keeps working because a bare scalar is treated as implicit `v1`.
- 37 new tests in `app/test/suite/labelprops.jl` — legacy shape, new shape, mixed shape, Symbol-key
  (JSON3) shape.
- Docs updated: `docs/OBJECTMODEL.md` (schema section), `docs/inventory/DATA_ACCESS.md` (new helpers row).

### P1b — Julia app-layer reader routing — **SHIPPED** (2026-09-18)

- `unversion_value(value, version=nothing)` in `helpers.jl` — the one-argument composer, for
  callers that already have the resolved value_name entry in hand (e.g. struct-field accessors).
- Four `img_*_path` helpers in `app/src/model/image.jl` grow an optional `version` kwarg and route
  through `unversion_value`: `img_filepath`, `img_label_props_path`, `img_labels_path`,
  `img_branch_labels_path`.
- `label_props(img; value_name, version)` in `app/src/label_props.jl` — same treatment.
- `resolve_version(img, field, value_name=nothing)::String` — companion to `resolve_value_name`
  for callers that need the version string for pinning / logging (P3 chain-planner surface).
- Legacy is a no-op end-to-end: every existing test suite passes unchanged, and a new testset
  proves `img_*_path(...; version="v99")` returns the legacy path today (bare scalar unwraps to
  itself).
- Struct field types **not** widened yet — that lands with the first writer (P2). Today every
  `resolve_version` call returns `LATEST_DEFAULT_VAL`, since struct fields still preclude versioned
  entries.

### P1c — Julia API-layer resolver — **SHIPPED** (2026-09-18)

- `resolve_image_version(project_uid, image_uid, value_name; version=nothing)`
  (`api/src/image_geometry.jl`) — the ONE api-side VN resolver grew an optional `version` kwarg and
  routes through `unversion_value`. Legacy bare-scalar entries unwrap to themselves (`version` is a
  no-op); versioned entries default to `_latest` and honour an explicit version. A missing version
  reports a specific error instead of a generic miss.
- 11 callers (audit said 12; one line was a `_gating_image` diff, off by one — recorded here for
  future audits): `crop_api.api_crop_info`/`api_crop_frame`; `movie_rail._resolve_frame_for_record`;
  `optical_flow_api.api_optical_flow_inspect`; `viewer_api.api_viewer_meta`/`try_serve_slab`/
  `api_viewer_props_get`/`api_viewer_props_post`/`api_viewer_record_test`/(record body handler);
  `image_geometry.api_image_geometry`. Each takes an optional `version` query/body param and threads
  it through unchanged. Legacy callers (no `version` supplied) keep their behaviour verbatim.
- New testset in `api/test/runtests.jl` — `API: resolve_image_version — inner version axis` —
  builds a versioned filepath entry directly on disk and verifies: legacy → `version` kwarg is a
  no-op; versioned → default resolves `_latest`; explicit `v1` addresses the older store; missing
  version returns a specific error.

### P1d — Python resolver + reader routing — **SHIPPED** (2026-09-18)

- Created `python/cecelia/utils/vn_versioning.py` — full Python mirror of the Julia composer.
  Exports `resolve_value_name`, `resolve_version`, `versioned_active`/`_get`/`_get_field`/`_keys`,
  inner-axis `is_versioned_entry`/`version_latest`/`version_get`/`version_keys`, composers
  `versioned_get_field_at` and one-argument `unversion_value`, plus constants
  (`VERSIONED_ACTIVE_KEY`, `VERSIONED_DEFAULT_VAL`, `LATEST_ACTIVE_KEY`, `LATEST_DEFAULT_VAL`).
- Reader pass-through — additive `version=None` kwarg / attr on: `LabelPropsView.__init__`,
  `LabelPropsUtils.__init__` / `.label_props_filepath` / `.label_props_view`. Class inits that
  take a `params: dict` now read `params.get('version')` and store it as `self.version`:
  `BayesianTrackingUtils` (`tracking_utils.py`), `MeasureUtils` (`measure_utils.py`),
  `SegmentationUtils` (`segmentation_utils.py`).
- `zarr_utils.open_as_zarr`/`open_zarr`/`series_base`/`read_axes`/`read_scale` unchanged — they
  take fully-resolved paths from the Julia side (Julia's `img_filepath` does the routing), so
  they need no signature change. Their internals continue to work with grouped-layout paths.
- Legacy is a no-op end-to-end: version kwarg on legacy scalar / list / None entries returns
  unchanged, verified by a `LabelPropsPassthroughTest.test_label_props_utils_filepath_version_kwarg_is_a_noop`
  test and the composer suite in `test_vn_versioning.py`.
- **Legacy path helpers (`labelProps/{vn}.h5ad`, `labels/{outputValueName}.zarr`, etc) NOT
  rewritten** — that lands with P4b so it stays independently reviewable. Today the version kwarg
  is stored but does not affect path assembly.

### P2 — Writer path: versioned target + can't-overwrite guard

Two independent pieces of P2 ship as separate PRs so the review surface stays small:

#### P2 infra — **SHIPPED** (2026-09-18)

Everything needed for a writer to land its data as a versioned entry, minus the actual writer:

- `version_next(d)::String` — pick the next unused `vN` (max numeric suffix + 1). Non-numeric keys
  are ignored so a hand-labelled `"draft"` doesn't skew the mint.
- `version_write!(d, item_value; version=nothing)::String` — the guarded writer (D6). Refuses
  `error` on collision; updates `_latest` to the version just written. `version_set!` remains the
  unguarded escape hatch for the P4a legacy migrator (stamp existing content as `v1` in place).
- `versioned_upgrade_entry!(d, value_name)` — writer's on-ramp: wraps a legacy bare scalar/vector
  entry as `v1` in place, so a task's first-ever v2 write doesn't require a schema migration.
  Idempotent on already-versioned entries.
- `CciaImage.filepath` / `.label_props` / `.labels` / `.branch_labels` widened to
  `Dict{String, Union{legacy_shape, Dict{String,Any}}}`, so a partially-migrated project (some
  value_names versioned, some not) loads and roundtrips through `save!` unchanged.
- `to_spaths` / `to_labels` load helpers accept the two-shape entry.
- Two lingering direct-access sites routed through `unversion_value`:
  `_image_payload` (`api/src/routes/helpers.jl`), `_label_zarr_path`
  (`app/src/tasks/spatialAnalysis/contactsMeshes.jl`),
  `segment/branching.jl` labels resolution. `test_zarr_access_convention.py` + the `zarr-access
  ratchet` testset extension moves to the pilot-writer PR.
- 3 new testsets in `app/test/suite/labelprops.jl` (`version_next`, `version_write!`,
  `versioned_upgrade_entry!`) and 2 in `app/test/suite/image_model.jl` (mixed-shape roundtrip
  through `read_ccid_raw`; widened field types accept a versioned Dict entry through init_object
  → save! → re-init).

#### P2 pilot — writer conversion (deferred)

A canonical writer opts into `version_write!` — probably ingest/conversion (`filepath`), since
that's the field with full P1c API-layer routing in place. Extends
`test_zarr_access_convention.py` and the `zarr-access ratchet` testset to catch bare
`default/<vn>/…` joins that bypass a resolver. Item #3 lands here.

### P3 — Chain planner pins inputs

`chain.jl` freezes `input_value_name` refs to `vn@version` at plan time (D3). Chain params grow an
optional explicit version qualifier for advanced use; default is "pin to `latest` now." Also settle
how gating references a specific version — probably "the gate is authored against `vn@v` and only
follows `latest` if the user opts in" (see *Open questions*).

### P4 — One-shot migration for existing projects

Sweep every existing project's `default/<vn>/<contents>` into `default/<vn>/v1/<contents>` and
update `ccid.json`. Idempotent, atomic per-vn. Runs on first project open after upgrade, with a
progress log entry.

**The audit surfaced 18 path-joining helpers** that hard-code `default/<vn>/` shape without a
version segment — image.jl (6 helpers), gating persistence, track/label correction paths, qc paths,
LabelPropsView.label_props_filepath, tracking_utils.props_path, measure_utils out_path,
segmentation_utils._store_path, store_sweep. Each is a rewrite site. P4 breaks into a
sub-plan (`P4a` migrator + `P4b` rewrite these helpers to compose through the resolver) so it is
independently reviewable.

### P5 — Prune surface (Settings + route + UI)

Settings → Storage: per vn, list versions with size + timestamp + "in use by chain run X" flag;
explicit prune button per version. `POST /api/versions/prune {vn, versions[]}`. No autoprune.

### P6 — Frontend vn pickers grow a version chip

Every place a vn is selected — LabelView, PopManager, plot vn selectors, ChainDesigner, Correction
worklists — gets an optional version chip. Default `latest`; explicit past-version selection is
read-only downstream (a task run against `v2` writes `v4`, not overwriting `v3`).

**The audit surfaced 43 `.vue` files touching `valueName`** — one declarative `valueNameSelection`
widget in `ParamRenderer.vue` (P6 lands here cleanly), but ~30 components hand-roll their own
`<select v-model="valueName">`. That sprawl overlaps with `VALUE_NAME_INPUT_PLAN` P3
("namespaces with nothing to suggest from"). **Coordinate P6 with `VALUE_NAME_INPUT_PLAN`** — either
fold P6 into that plan as a new phase, or collapse the ad-hoc selects onto the declarative widget
in a preceding PR and then let P6 add the version chip in one place. Decision deferred until P1
lands; the corrected numbers make consolidation the obviously cheaper path.

**Movie compare** already models the "versions + segmentations as chip sets" pattern
(`frontend/src/utils/movieCompare.ts` + `MovieCompareControls.vue`); the version chip should extend
this shape rather than introduce a second control.

## Touchpoints — grep-verified 2026-09-18

From `docs/audit/vn-versioning-touchpoints.md` (against origin/main at `20a4fa73`). The pre-audit
column shows the plan's original grep-free estimate.

| Category | Helpers | Callsites (verified) | Pre-audit estimate |
|---|---|---|---|
| Julia writers (`staged_store` / `create_multiscales`) | 3 | 15 | ~10 |
| Julia writers (`versioned_set_field!` on filepath) | — | 15+20 label registry | not counted |
| Julia writers (per-vn `write_json_atomic` sidecars) | — | ~20 | not counted |
| Python writers (`staged_store`, `write_h5ad_atomic`) | 2 | 15 + 5 h5ad | same shape |
| Julia readers keyed by vn | 8 | ~65 | ~15–20 |
| Python readers keyed by vn | 5 | ~40 | included above |
| API-side resolvers (`resolve_image_version` + payload builders) | 2 | ~15 | **missing** |
| ccid.json versioned-field helpers | 8 | schema + ~40 callers | one migration |
| Frontend vn pickers | 1 widget + ~30 ad-hoc | 43 `.vue` files | ~5 |
| Chain planner refs (composite snapshot) | 3 | ~10 | one pin |
| MCP tools referencing `value_name` | 2 | 3 callers | audit in P1 |
| Legacy `default/<vn>/` path assumptions | — | 18 helpers | not counted |

**The geometry holds:** ~4 Julia helpers + ~5 Python helpers + one api resolver + one ccid schema
are the primitives to change. Callsites are many but they cluster on those primitives — that's what
keeps the refactor tractable.

## Reservations from the audit

Three phasing revisions fell out of the grep sweep — folded into P1/P4/P6 above; recorded here so
the deltas are visible:

1. **P1 needs three resolvers, not one.** Julia app-layer (`resolve_value_name`), Julia API-layer
   (`resolve_image_version` — 12 callers, different return shape), and a **new** Python
   `resolve_value_name` (does not exist today). Missed in the original plan.
2. **P4 is a two-part phase.** 18 legacy path-joining helpers hard-code `default/<vn>/` shape
   without a version segment. Sub-plan into P4a (data migrator) + P4b (rewrite the helpers to
   compose through the resolver). Independently reviewable.
3. **P6 must coordinate with `VALUE_NAME_INPUT_PLAN`.** 30 of the 43 `.vue` files hand-roll their
   own `<select v-model="valueName">` instead of using the declarative widget. Consolidating them
   is cheaper than adding a version chip to every one — the two plans overlap enough that P6 might
   fold into `VALUE_NAME_INPUT_PLAN` as a new phase. Decision deferred until P1 lands.

Two smaller surfaces the audit called out that don't change the phasing but are worth citing here:

- **Two resolvers behave differently today.** `resolve_image_version` returns
  `(zarr_path, meta_dir, error)`; `resolve_value_name` returns `(vn, active_key)`. Both grow
  versions in P1, but the shapes stay distinct.
- **A `write_json_atomic` per-vn sidecar class** (qc, corrections, staleness, clustfeatures, motif)
  was missing from D7's original list. Now folded in.

## Open questions — non-blocking, decide in-phase

- **Content-hash integrity** as a separate future item (hash-of-tree after the write completes,
  recorded next to the version in ccid.json). Not in this plan; different concern.
- **Cross-project vn versioning** — `PROJECT_IO_PLAN.md` needs to know how to serialise version
  chains in `.ccbundle`. Coordinate at P4/P5.
- **Gating vs `latest`** — does a gate authored on `v2` re-evaluate against `latest`, or stay
  pinned? Probably pinned unless the user opts in; settle in P3.
- **Failed-run cleanup.** If P2 crashes mid-write, does the partial `vN/` dir need `latest` never
  to have moved (yes — atomic promote is only on success), *and* a way to remove the partial? P2
  should refuse to reuse a version number even if the previous attempt was aborted; prune surface
  (P5) exposes the orphan.

## Non-goals

- **Filesystem snapshots** (btrfs, zfs, APFS clone). Zero-cost when they work; Windows kills it and
  item #1 explicitly asks for a structural default, not FS-dependent.
- **Whole-project directory snapshots** (the R shape). Rejected on cost + granularity — see *Why
  now*.
- **Automatic eviction policies.** Explicit prune only in v1.
- **Granting chain execution rights.** This plan closes prerequisites #1 and #3 from the
  chain-execution list. Granting execution is a separate decision this plan does not authorise.
