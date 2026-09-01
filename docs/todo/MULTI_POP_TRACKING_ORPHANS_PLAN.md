# Multi-pop tracking — orphans, collisions, attribution guard

Status: **planning (2026-09-02)** · no branch. Follow-up to
[`MULTI_POP_TRACKING_PLAN.md`](MULTI_POP_TRACKING_PLAN.md) (shipped 2026-09-01, #741–#746).

## Why this plan exists

The shipped `track_source` obs column + provenance-aware `_write_back` fixed the "last-pop wins"
bug. Two structural gaps survived the ship, both trace-verified in the code as of
2026-09-02:

1. **Orphaned rows on pop deletion.** `_write_back`'s DELETE step is a string equality
   (`del_mask = [s == self.track_source for s in src_obj]`, `tracking_utils.py:255`). A pop UID that
   never fires again — because the pop was deleted — never gets NaN'd. `del_pop!`
   (`population_manager.jl:482`) → `_del_paths!` drops the UID from `uid_index` and returns; it
   never reaches into the h5ad. `api_gating_pop_delete` (`gating_api.jl:1016`) just calls
   `del_pop!`. There is no cleanup path.

2. **Silent attribution corruption downstream of an orphan.** `has_tracks` on the pop payload
   (`population_manager.jl:1795`) is computed from `track_id > 0 ∩ pop.labels`. It does **not**
   consult `track_source`. So an orphaned row whose label happens to fall inside a **different**
   live pop's gate makes that pop's `has_tracks` fire — ribbons authored by deleted pop A silently
   render as pop C's, with C's colour, in both the viewer and the batch movie
   (`movie_helpers.jl:308`). This is not just "stale rows": it's wrong attribution.

3. **Cross-pop label collision on WRITE — undocumented for the general case.** Lines 316–326 of
   `_write_back` unconditionally overwrite `cur_id/parent/root/state/gen/cellid/src_obj[r]` for
   every label in the run's `track_df`. There is no guard for "this label is already claimed by a
   different `track_source`." The plan documented only the `whole_seg → pop` non-symmetry as
   intentional (line 158 of the shipped plan). Two overlapping flow pops (parent/child, or two
   orthogonal gates that intersect) → tracking B after A silently transfers ownership of shared
   labels from A to B, breaking the invariant that "A's tracks = the rows A authored." A's
   subsequent DELETE misses those labels because they now read `track_source == B`.

4. **UID reuse across deletion — silent-failure surface, low probability.** `_fresh_pop_uid`
   (`population_manager.jl:156`) checks the LIVE `uid_index` only. `del_pop!` releases the UID.
   `gen_uid` can reissue it. Space is 6-char `[A-Za-z0-9]` ≈ 56.8 billion; collision probability
   across a project of O(100) pops is negligible in practice, but the failure mode is silent — a
   new pop would inherit the deleted pop's orphan rows on its first tracking run.

## Locked decisions

1. **`has_tracks` becomes provenance-aware.** A pop's `has_tracks` fires only when it holds a cell
   with `track_id > 0` **and** at least one of those tracked cells was authored by this pop OR by
   whole-seg tracking. Concretely: `track_id > 0 ∧ (track_source == pop.uid ∨ track_source ==
   "whole_seg")`. This is the immediate visible-bug fix (mis-attributed ribbons), and it also
   silences an orphan retroactively as soon as a pop that used to overlap it is re-gated to no
   longer include those labels.

   **Legacy data (no `track_source` column).** Rows written before the P1 ship are treated as
   `track_source is missing`, and the guard treats them as **whole-seg-equivalent** — i.e. every
   pop that touches those labels claims them. Rationale: the shipped plan already declared "No
   backward-compat for existing tracked data" (line 209); a project that hasn't been re-tracked
   post-ship keeps the pre-ship visible behaviour rather than silently losing all its ribbons.
   Re-tracking whole-seg or per-pop rewrites the marker and the guard tightens.

2. **Conflict detection lives in `_write_back`, fails by default, force to override.** Before the
   WRITE step, compute `conflict_mask = [src_obj[r] not in {None, "whole_seg", self.track_source}
   for r in run's labels]`. If any label is in conflict:
   - Log the count + the first N (label, existing_source) pairs.
   - Raise with an actionable message: "Cannot track pop X: n labels already tracked by pop(s)
     Y, Z. Re-track those pops first, adjust pop definitions to remove overlap, or pass
     force=true."
   - Callers pass `trackSourceForce = true` to bypass. Not exposed as a param widget in P1 —
     added later if the whole-seg refine idiom generalises. `whole_seg` is already treated as
     non-conflicting because it's the documented "prime everything, refine one pop" mode.

   Rejected: **skip-not-fail** ("leave labels owned by other sources alone"). That is the silent
   behaviour we're trying to escape.

   Rejected: **conflict detection at pop-deletion time**. That couples gating to a Python h5ad
   side-effect and requires the image handle in `gating_api.jl`. A hot-path check that is a no-op
   in ~99% of gating edits is what the prompt author explicitly cautioned against.

3. **Orphan sweep runs at tracking-time, not at deletion-time.** `bayesian_tracking.jl` passes
   `liveTrackSources = [pop_uid(m, p) for p in pop_paths(m)]` (plus `"whole_seg"`) down as a
   param. Before COMPACT, `_write_back` NaNs any row whose `src_obj` value is neither `None` nor
   in the live set. Cost: an orphan lingers between deletion and the next tracking run on that
   segmentation. Benefit: the sweep runs where the h5ad is already open, driven by a task the
   user consciously started, not on every gating edit. The attribution guard in decision 1
   suppresses the ribbon meanwhile — the orphan is invisible, not just cleaned up eventually.

4. **UID reuse is closed via a persisted `retired_uids` set.** `PopulationMap` gains
   `retired_uids::Set{String}`; `_del_paths!` inserts each dropped pop's UID; `_fresh_pop_uid`
   also refuses candidates in it. Serialised alongside `uid_index` in the gating JSON. Grows
   unbounded (6 chars per entry) — hundreds of deletions is bytes. Cheaper than lengthening the
   UID space or timestamp-scoping identifiers, and it makes UID identity monotonic within a
   project's history.

5. **No new API routes.** All four decisions live inside existing entry points: `_write_back`
   (Python), `resolve_pops` (Julia), `_del_paths!` (Julia), the `bayesian_tracking` handler
   (Julia). No new WS messages, no new frontend types.

## Phases

Ordered by user-visible impact, each independently PR-able.

### P0 — attribution guard on `has_tracks` (Julia)

- `app/src/gating/population_manager.jl` `resolve_pops` (~line 1770): extend the label-props read
  to also select `track_source`. Compute `has_tracks` as `∃ label ∈ pop.labels :
  track_id[label] > 0 ∧ track_source[label] ∈ {pop.uid, "whole_seg", <missing>}`. The `<missing>`
  branch preserves legacy behaviour for pre-P1 data (decision 1).
- No change to the pop payload shape — `has_tracks::Bool` is still the field.
- Test (`app/test/`): fixture with two overlapping flow pops A + B, `track_source` set to A's UID
  on the shared labels. Assert `resolve_pops` returns `A.has_tracks == true`, `B.has_tracks ==
  false`. Second fixture with `track_source == "whole_seg"` on the shared labels: both true.
- **Wins the visible bug fix on its own** — even before P2/P3 land, the mis-attributed ribbon
  disappears.

### P1 — conflict detector in `_write_back` (Python)

- `python/cecelia/utils/tracking_utils.py::BayesianTrackingUtils._write_back`: after the DELETE
  step, compute `conflict_mask` per decision 2. Log + raise unless `self.force_track_source` (new
  attr, defaulting to `False`).
- `python/cecelia/utils/tracking_utils.py::BayesianTrackingUtils.__init__`: read
  `force_track_source = bool(params.get("trackSourceForce", False))`.
- `app/src/tasks/tracking/bayesian_tracking.jl`: pass `trackSourceForce` through the param bag
  (not exposed to the user in P1; wired for future use).
- Tests (`python/cecelia/tests/test_tracking_units.py`): (a) two disjoint pops → both track
  cleanly, no error; (b) two overlapping pops → second run raises with a message naming the
  colliding labels and the existing source; (c) same as (b) with `trackSourceForce = True` →
  proceeds, second pop's ownership replaces the first for the overlap.

### P2 — retired UID set (Julia)

- `app/src/gating/population_manager.jl`:
  - `PopulationMap` field `retired_uids::Set{String}`, initialised empty.
  - `_del_paths!` (`_del_paths!`, ~line 500): for each deleted pop, `push!(m.retired_uids,
    p.uid)`.
  - `_fresh_pop_uid`: `haskey(m.uid_index, candidate) || candidate ∈ m.retired_uids || return
    candidate`.
  - Serializer round-trip: emit `"retired_uids"` at the map level in `_map_dict` /
    `to_json`-equivalent, parse in `from_json`-equivalent (backfill from empty when absent).
- Tests: (a) delete a pop with UID X, `add_pop!` returns some UID ≠ X; (b) load a sidecar with
  `retired_uids: ["abc123"]` and observe `_fresh_pop_uid` skips it; (c) legacy sidecar without
  the field loads with an empty set, marks dirty on first mutation.

### P3 — orphan sweep at tracking-time (Julia + Python)

- `app/src/tasks/tracking/bayesian_tracking.jl`: emit `liveTrackSources =
  [pop_uid(m, p) for p in pop_paths(m)]` in the run params, plus the sentinel `"whole_seg"`.
- `python/cecelia/utils/tracking_utils.py::_write_back`: after the DELETE step and BEFORE
  COMPACT, mask rows whose `src_obj` is a string not in the live set, NaN'ing the same six
  columns + resetting `src_obj[i] = None`. Log the count.
- Non-goal: on-demand cleanup UI, deletion-time hook.
- Test (Python): (a) fixture with `track_source == "deleted_uid"` on three rows; run
  `_write_back` with `liveTrackSources == {"pop_A_uid", "whole_seg"}` → the three rows are NaN
  after the run, along with the run's own writes; (b) `whole_seg` rows are preserved (sentinel
  always live).

### P4 — parity + integration tests

- Confirm `movie_helpers.jl` legend + ribbon filtering (`has_tracks` is already the gate — no
  code change).
- Add a Julia integration test that runs `resolve_pops` end-to-end on a fixture with an orphan
  + a live pop, asserting the orphan is invisible via P0's guard and swept by P3's step.
- Add a decision-level parity test per the shipped `VIEWER_PARITY_PLAN.md`: emitted per-pop
  track-source list matches on browser and offline paths given the fixture.

## Non-goals

- **No deletion-time h5ad reach.** Gating stays gating; tracking stays tracking. The sweep
  happens where the h5ad is already open.
- **No new user-facing `force` widget in P1.** Wired for future use; today's whole-seg refine
  case is already covered by the `whole_seg` sentinel bypass. If the pop→pop refine idiom
  generalises, add the widget then.
- **No backfill for legacy data.** Consistent with the shipped plan: pre-P1 rows have
  `track_source == NaN` and are treated as whole-seg-equivalent by P0. Re-tracking rewrites the
  marker.
- **No cross-image sweep.** `_write_back` operates on one segmentation's h5ad. Multi-image
  cleanup happens the same way: when each image is re-tracked, its orphans go.

## Files touched (estimate)

- P0: 1 Julia file (`population_manager.jl`) + 1 test module.
- P1: 1 Python module + 1 Julia file + 1 test module.
- P2: 1 Julia file + 1 test module.
- P3: 1 Julia file + 1 Python module + 1 test module.
- P4: 1 test module.

Total: ~6 code files, 4 test modules. No new packages. No new API routes. No new frontend types.
