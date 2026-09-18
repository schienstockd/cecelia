# Value-name versioning — every writer targets a new `vn@v`, never overwrites

**Status:** **P1a + P1b shipped** (2026-09-18) — helpers + composer + Julia app-layer reader
routing, additive, backward-compat verified via full test suite. P1c/d + P2–P6 planning. Comes out
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

### P1c/d — Julia API-layer resolver + Python resolver

Two remaining resolver surfaces (P1b handled Julia app-layer):

- **Julia API-layer**: extend `resolve_image_version` at `api/src/image_geometry.jl:157` (the ONE
  api-side VN resolver — 12 callers across viewer/crop/movie/optical-flow APIs). The audit surfaced
  this as a separate resolver the plan initially missed. Different return shape (`(zarr_path,
  meta_dir, error)` vs `(vn, active_key)`); both grow versions.
- **Python**: **create** `resolve_value_name` in `python/cecelia/utils/` — no equivalent exists
  today (`LabelPropsView`'s constructor defaults `value_name="default"` and never falls back to an
  active pointer).

Route every existing reader through them: `label_props`, `img_*_path` family (`img_filepath`,
`img_label_props_path`, `img_track_props_path`, `img_branch_props_path`, `img_labels_path`,
`img_branch_labels_path`), `gating_path`, `load_pop_map`; Python side `LabelPropsView`,
`zarr_utils.open_as_zarr`/`open_zarr`, `series_base`, `read_axes`/`read_scale`,
`tracking_utils.props_path`, `measure_utils.out_path`, `segmentation_utils._store_path`; API side
every caller of `resolve_image_version`. Legacy single-version projects return `v1` implicitly.

**No writer changes yet.** Test-enforced: extend `test_zarr_access_convention.py` and the
`zarr-access ratchet` testset to catch bare `default/<vn>/…` joins that bypass a resolver.

### P2 — Writer path: versioned target + can't-overwrite guard

`zarr_utils.staged_store`, `write_h5ad_atomic`, and Julia `write_atomic` take a `(vn, version)`
target. Version defaults to `latest+1` at plan time. Refuse if `default/<vn>/vN/` exists (D6).
`test_zarr_access_convention.py` + the `zarr-access ratchet` testset extend to cover this. Item #3
lands here.

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
