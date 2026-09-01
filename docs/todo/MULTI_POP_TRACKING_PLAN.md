# Multi-population tracking + per-pop track layers

Status: **planning** (2026-09-01)

## Goal

Let a user run `tracking.bayesian_tracking` on two (or more) gated populations of the same
segmentation and see BOTH populations' tracks in the viewer, each as its own layer that reads and
toggles like the point populations do (`overlays.pops` → swatch / name / count / eye).

Concrete scenario (Dominik, `fXgbTl`, 2026-09-01):

> "i tried to track /qc/CD169-/cells and /qc/CD169-/fragments in fXgbTl. but then it seems when i
> toggle the track ribbons it only shows me the last tracked population. also. the different track
> populations are not being shown in the image viewer. just the controls for tails. i would have
> expected a similar layer approach as in the point populations"

## Root causes (verified in code, 2026-09-01)

1. **Overwrite in `_write_back` (Python).** `BayesianTrackingUtils._write_back`
   (`python/cecelia/utils/tracking_utils.py`) builds `lineage` for the tracked cells only, calls
   `view.add_obs(lineage)`, and `add_obs` (in `label_props_utils.py`) creates a **full-length array
   filled with NaN** for labels absent from the input frame. That array replaces the whole
   `track_id` column, so tracking `/qc/CD169-/fragments` after `/qc/CD169-/cells` NaNs out the
   cells rows. This is the "only the last tracked pop shows" behaviour.

2. **`is_track` is population-type-scoped, not track-content-scoped (Julia).** In
   `app/src/gating/population_manager.jl` `is_track = String(pop_type) == "track"` — set true only
   for the track-family pop type. `api/src/overlay_author.jl` (`is_track_pop = get(p, :is_track,
   false) && hasK`) skips every flow pop when deciding which pops render as ribbons. So even if
   `track_id` were populated correctly per pop, a flow pop with tracked cells still wouldn't turn
   into a ribbon layer.

3. **No per-track-pop UI in the viewer (frontend).** `ViewerWindow.vue` Populations section
   enumerates `overlays!.pops` as one row each (swatch/name/count/CcToggle); the Tracks section
   shows only shared tail sliders and a per-source legend that is gated on
   `trackColorMode === 'solid'`. There is no equivalent per-pop layer row for tracks.

4. **User's ask does not need existing tracked-data preservation** (Dominik, 2026-09-01: "dont
   worry about existing tracked data"). Re-running tracking after this ships is acceptable.

## Locked decisions

0. **Reintroduce stable population UIDs.** Populations get a new `uid::String` field on the
   `Population` struct, generated at creation via `gen_uid` (the project-wide id helper), persisted
   in the gating JSON, unchanged by rename/move. The R version had this as `popID` (`popMap` was
   keyed by it; `popIDsByAttr`/`popIDsFromPaths` mediated between UI paths and the stable id); the
   Julia port collapsed it to a path-keyed map. Bringing it back is the general fix, not a
   tracking-only band-aid: it's the primitive `track_source` needs and the primitive
   `boolean_pops` references *should* be using instead of the path-cascade in `_repath!` — but
   this plan only rewires `track_source`. Boolean references stay on paths until a follow-up
   plan justifies the migration cost. Migration: on load, any pop without a UID gets one assigned
   in-memory (persisted the next time the sidecar is written). The map stays `path → Population`
   (the UI reads by path); a `Dict{String,String}` `uid → path` lookup lives beside it.

1. **Provenance-aware merge, not raw additive.** Keep the single `track_id` (+ lineage columns) in
   the segmentation h5ad, PLUS a new `track_source` obs column (categorical/string) recording
   which pop **UID** (or `"whole_seg"`) produced each cell's `track_id`. On a tracking run with
   source `X` (`X` = the pop's UID, not its path):
     a. Load existing `track_id`, `track_parent`, `track_root`, `track_source`.
     b. **Delete** (NaN) `track_*` rows where `track_source == X` — the previous X run is gone
        as a unit, wherever those cells now sit.
     c. **Compact** the remaining track_ids to `1..N` (rewrite `track_id`, then re-map
        `track_parent` / `track_root` through the same permutation — they reference `track_id`).
     d. Assign the new run's `track_id`s from `N+1`.
     e. Set `track_source = X` on the newly tracked cells.
   Why not raw additive (offset by `max(existing) + 1`, never compact): re-running tracking on
   pop X ten times would push its `track_id`s to 10× their working range, and cells that were
   once in X but no longer are (user re-gated between runs) would keep their old track_ids
   forever. Provenance-plus-compaction is the version that lets a user *tune* a pop's tracking
   without polluting the id space.
   Alternative rejected: per-pop `track_id_/<popPath>` columns. That splits every downstream
   consumer (`track_measures`, `track_props`, gating derived `_tracked`, cohort QC) and none of
   them are shaped for a multi-column universe today. Provenance-aware merge stays inside the
   existing single-`track_id`-column contract.

2. **A pop's ribbon eligibility is data, not type.** Extend the pop payload (`resolve_pops` →
   `viewer_api.jl`) to carry `hasTracks` (`labels ∩ {label : track_id > 0}` non-empty on the
   current segmentation). Do NOT change the meaning of `is_track` — that flag still names
   track-family pop types (gates on track measures), and `TrackSchemeView` / analysis board rely
   on it. `overlay_author.jl` checks `is_track || hasTracks` before drawing ribbons for a pop.

3. **Per-pop track layers, not per-segmentation only.** In the viewer, the "Tracks" section shows
   one row per gated pop with `hasTracks` (swatch/name/count/CcToggle), mirroring the Populations
   section. The existing per-segmentation directions eye stays as the "everyone else" fallback:
   it draws tracks for cells NOT covered by any listed track-pop. This keeps the raw-segmentation
   view accessible when no gating exists.

4. **Master gates stay unchanged.** `gatedTracksShown` (per-set) still gates the whole track-pop
   family off/on above the per-pop rows. `trackVisibility` (per-image × segmentation) still gates
   the per-segmentation directions layer.

5. **Filter set + hidden-tracks set are separate.** Points have `hiddenPops`; tracks get their own
   `hiddenTrackPops`. A user showing points for `/qc/CD169-/cells` and hiding its tracks is a
   valid state — collapsing the two would silently link them.

## Phases

### P0 — pop UID (Julia)

- `app/src/gating/population_manager.jl`: add `uid::String` to `Population` (generated via
  `gen_uid` at construction), a `uid → path` lookup on `PopulationMap`, and a `pop_uid(m, path)`
  / `pop_by_uid(m, uid)` pair. Persist `uid` in the gating JSON serializer
  (`to_json`/`from_json`); on read, assign a fresh UID to any legacy pop lacking one and mark the
  map dirty so it saves on the next `save!`. `_repath!` no-ops on UID (it's already stable).
  `remove_pop!` drops the UID from the lookup.
- Test (`app/test/`): a pop's `uid` survives `rename_pop!` and `move_pop!`; two loads round-trip
  the same UID; a legacy sidecar without a `uid` field parses (in-memory backfill) and rewrites
  with one on save.
- **Not in P0**: boolean pop references stay on paths. Migrating those to UIDs is a separate
  cleanup — the path cascade in `_repath!` covers it today, and mixing the two migration
  concerns invited two rounds of "I broke boolean pops".

### P1 — provenance-aware merge (Python + Julia)

- **Julia handler** (`app/src/tasks/tracking/bayesian_tracking.jl`): pass the run's
  `track_source` string down to Python — the pop's **UID** (via `pop_uid(m, pops_to_track)`, from
  P0) when `pops_to_track != "NONE"`, else `"whole_seg"`. One line into the `run_py` param bag.
  Using the UID here is what makes rename/move transparent: the sidecar changes, the h5ad
  doesn't.
- **Python** (`python/cecelia/utils/tracking_utils.py::BayesianTrackingUtils._write_back`):
  - Read existing `track_id`, `track_parent`, `track_root`, `track_source` (via
    `LabelPropsView`). Missing `track_source` on first ever run is fine — treat as all-NaN.
  - **Delete step**: mask rows where `track_source == self.track_source` back to NaN in all
    four columns.
  - **Compact step**: after the delete, remaining `track_id`s are non-NaN but no longer
    contiguous. Build a `{old_id → new_id}` permutation for the surviving unique ids, then
    apply it to `track_id`, `track_parent`, `track_root` (they all live in the same id space).
    `NaN` stays `NaN`; root/parent that reference `NaN` stay `NaN`.
  - **Write step**: emit the new run's lineage with `track_id` starting at `max(compacted) + 1`
    (or `1` when empty). Set `track_source = self.track_source` on the newly tracked rows.
    Combine with the compacted survivors and write ONE column each via `add_obs`, plus the
    new `track_source` via `add_categorical_obs`.
  - Keep the existing `drop_obs` of stale `live.cell.*` / `live.track.*` (measures compute
    against a superset that has changed).
- **`add_categorical_obs`** already exists — use it for `track_source`. No new writer.
- Test: `python/cecelia/tests/test_tracking_units.py`
  - **Two disjoint pops**: track A then B, assert both sets' `track_id`s survive and don't
    collide; `track_source` matches the run.
  - **Re-track pop A** after A + B: assert A's old ids are gone (delete worked), B's ids are
    still present but renumbered (compact worked), `max(track_id) == n_A_new + n_B` (no
    growth).
  - **Whole-seg then per-pop**: track whole_seg, then track A. The A rows previously carried
    `track_source == "whole_seg"`; the new A run doesn't match `track_source == "A"`, so the
    delete step *doesn't* remove them. Then the write step overwrites A's rows with new ids,
    but does NOT touch the whole_seg rows outside A. Document this as an intentional
    non-symmetric: whole-seg tracking is a "prime everything" mode; per-pop tracking after it
    only refines the pop.

### P2 — pop payload flag (Julia)

- `api/src/overlay_author.jl` (`_pops_payload`, or wherever pop rows are shaped): compute
  `has_tracks = any(track_id[l] > 0 for l in labels)` per pop when `hasK`, add to the emitted
  NamedTuple.
- `api/src/viewer_api.jl` `overlaysUrl` handler: forward `hasTracks` to the client
  (`p.has_tracks`).
- `overlay_author.jl` render decision: `is_track_pop = (Bool(get(p, :is_track, false)) || Bool(get(p, :has_tracks, false))) && hasK`.
- No test — this is a pass-through; the visible behaviour lands in P3.

### P3 — per-pop track rows in ViewerWindow (frontend)

- Types (`frontend/src/utils/viewerOverlays.ts` or wherever `OverlayPayload` lives): add
  `hasTracks?: boolean` to the pop entry.
- `frontend/src/modules/ViewerWindow.vue`:
  - `rebuildOverlays`: extend the gated-track loop from `pop.isTrack` to `pop.isTrack || pop.hasTracks`.
    Respect a new `hiddenTrackPops: Set<string>` alongside `hiddenPops`.
  - Tracks section template: enumerate `overlays!.pops.filter(p => p.hasTracks)` as rows with a
    swatch, name, count (labels with `track_id > 0`), and CcToggle bound to `hiddenTrackPops`.
  - Empty state: when `gatedTracksShown` is on and no pops have tracks, say so ("no gated
    populations have been tracked yet") rather than falling through to the tail sliders alone.
- Per-segmentation "all other tracks" behaviour: the current directions eye keeps drawing every
  `track_id > 0` cell in the segmentation. Its rendered set is the WHOLE segmentation, so it
  intentionally overlaps the per-pop rows above — that is the "which cells aren't in any of my
  track pops" fallback view. Do NOT filter it to unclaimed cells; the two are separate
  observational lenses.

### P4 — parity across renderers

- The offline compositor (`api/src/movie_helpers.jl` → the movie renderer) reads the same overlay
  payload. Confirm the `hasTracks` flag flows through it too so a recorded movie matches the
  browser view. If it doesn't, add the filter there.
- Add a decision-level parity test per `VIEWER_PARITY_PLAN.md` P3 (payload comparison, not
  pixels): given a fixture with two tracked pops, assert the emitted per-pop track source list is
  the same on both paths.

## Non-goals

- **No new tracking task.** One `bayesian_tracking` invocation still tracks one pop or the whole
  segmentation, as today. The user runs it multiple times.
- **No run-level provenance.** `track_source` is *pop-level* (which pop authored a row's
  `track_id`), not *run-level* (which invocation). Two runs of pop X still identify as `X`, and
  the second replaces the first — that is the point. If a downstream feature ever needs
  per-invocation provenance (compare tracking parameters across runs), add a `track_run_id`
  then, not now.
- **No backward-compat for existing tracked data.** Per the user (2026-09-01), re-running is
  acceptable. Existing h5ads have no `track_source` column — the delete step no-ops on first
  run against new-code data, and any pre-existing `track_id`s are treated as `track_source ==
  NaN` (kept, never matched by the pop-path delete filter, and freely renumbered by the compact
  step). This is fine: whole-seg tracking will overwrite them the next time it runs.

## Files touched (estimate)

- P0: 1 julia file (`population_manager.jl`) + 1 test module.
- P1: 1 julia file (`bayesian_tracking.jl`) + 1 python module + 1 test module.
- P2: 2 julia files (`overlay_author.jl`, `viewer_api.jl`).
- P3: 1 vue file (ViewerWindow) + 1 type file.
- P4: 1 julia file + 1 test.

Total: ~10 files. No new packages. No new API routes.
