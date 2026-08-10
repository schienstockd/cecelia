# Spatial gates in µm — one pixel scale, applied in one place

**Status:** built (this branch), all four suites green. Depends on `scale_centroids!` /
`img_is_calibrated` from PR #509. Phase 5 was written, verified and then deliberately dropped — see
decision 8.

## Problem

You can gate on position — `centroid_x`/`_y`/`_z` are offered as gate axes (Phase 4 of
`CENTROID_AXES_PLAN.md`). Everything about that is in **pixels**, and nothing says so:

| Layer | Today | Where |
|---|---|---|
| values plotted | raw obsm → **pixels** | `gating_api._plot_xy_raw` |
| transform | **linear** for a centroid axis, i.e. identity — nothing rescales | `axisDefaultTransform` → `isLinearAxis` |
| tick labels | `invert_transform` back to raw → **pixel numbers** | `gating_api.jl:142` |
| axis name | the bare column: literally `centroid_x`, no unit | `colLabel` (`stores/gating.ts:139`) |
| stored gate coords | gate's own transform space; linear ⇒ **pixels** | `gates.jl:49-58` |

This is **internally consistent today** — px thresholds compared against px data, so membership is
correct. It is not a live bug. Two things make it wrong anyway:

1. **Gate broadcast across images** (`applyBroadcast`, POPULATION.md → *Gating pop types & copy across
   images*). A pixel rectangle copied to an image with a different pixel size selects a **different
   physical region**, silently. Same failure class as PR #491, in the place #509 called isolated.
2. **`pop_df(…; centroids = :physical)`** (#509) returns µm. So a population whose boundary was drawn
   at "500" (px) hands back coordinates in µm — the same cells, but two numbers for one boundary, and
   nothing on screen distinguishing them.

## Locked decisions

1. **Gates on spatial axes are stored in µm.** Physical units are what the science is in, and they are
   the only thing that survives being copied to another image.

2. **The scale is a property of (image, axis), resolved at eval/display time — NEVER stored in the
   gate.** The tempting route is to fold µm/px into the stored transform spec (the transform machinery
   already converts spaces, and `inside()` already applies it). Rejected: that bakes ONE image's pixel
   size into the gate and re-breaks broadcast in exactly the way this plan exists to fix. The gate
   carries µm; each image supplies its own µm/px.

3. **`recompute!` applies the scale, once.** It already receives the fetched DataFrame and knows which
   columns are centroids. `recompute!` is reached from **six** independent call sites, five of which
   build their own fetch closure:

   | Call site | What it resolves |
   |---|---|
   | `api/src/gating_api.jl:119` | the gate editor / plotdata |
   | `app/src/gating/population_manager.jl:655` | `_pop_df` membership |
   | `app/src/gating/population_manager.jl:716` | `_pop_df_tracks` |
   | `app/src/gating/population_manager.jl:1338` | `resolve_pops` |
   | `app/src/tasks/tracking/bayesian_tracking.jl:35` | gated tracking input |
   | `app/src/tasks/segment/branching.jl:153` | gated branching input |

   Scaling per call site would drift — a seventh forgets, and the symptom is a population with
   different members depending on which path resolved it. Scaling inside `recompute!` makes that
   structurally impossible.

4. **A `spatial_unit` stamp on the gating file, so legacy files keep working.** `PopulationMap` gains
   `spatial_unit` ∈ `{"px", "um"}`, round-tripped through `to_tree`/`from_tree`. **Absent ⇒ `"px"`**
   (every file written before this change). `recompute!` scales only when the map says `"um"`, so a file
   that was never converted keeps evaluating correctly — deliberately unlike `CENTROID_AXES_PLAN.md`'s
   no-fallback stance, because here the old data is *correct in its own terms* rather than malformed.
   That fallback is what lets decision 8 ship no migration at all.

5. **A map adopts µm whenever there is nothing to reinterpret.** `load_pop_map(img; …)` stamps `"um"`
   when the image is calibrated **and** the map has no spatial gate (`has_spatial_gate`) — on a new map
   and equally on a long-standing intensity-only one, since neither holds coordinates whose meaning
   could change. Left alone: a map that *does* carry a position gate (re-stamping would silently move
   its numbers), and an uncalibrated image (no µm to adopt; `img_physical_sizes`' 1.0 default would
   masquerade as one).

6. **`centroid_t` stays a frame index.** It is gateable (`temporalColumns`) and it is an *index into the
   data*; PR #491 established that scaling time silently redefines every frame-counted parameter. It is
   labelled, not converted.

7. **Uncalibrated images: px, and broadcast is refused.** With no pixel size the scale is 1.0 and the
   axis is pixels — `img_physical_sizes` defaults a missing axis to 1.0, which is indistinguishable
   from a real 1 µm/px (hence `img_is_calibrated`, #509). Copying a µm gate onto an uncalibrated image
   is **refused with a message naming the image**, not warned-and-proceeded: the result would be wrong
   by the pixel size, which is exactly the silent-drift bug being removed.

8. **No migration ships.** A per-image converter was written, run against real data, and **dropped**:
   position gating is new enough that no project holds a pixel-space position gate, so it measured 0
   coordinate changes over 13 real gating files and could only ever have been a permanent Settings entry
   that did nothing. Same call as the retired centroid-axes patch (`CENTROID_AXES_PLAN.md` Phase 2), and
   decision 5's lazy adoption covers every real case instead. A pre-existing px position gate would stay
   px and keep evaluating correctly as px; if one ever turns up, the converter is in this branch's git
   history (`python/cecelia/utils/spatial_gate_units.py` + its tests). It needed the stamp for
   idempotency — 500 px and 500 µm are the same number on disk, so a second run would square the scale.

## Blast radius

### Julia — package
- `gating/population_manager.jl` — `PopulationMap.spatial_unit` + pixel sizes; `to_tree`/`from_tree`
  round-trip; `load_pop_map(img; …)` stamps sizes; `save_pop_map!` stamps the unit.
- `gating/gating_engine.jl` — `recompute!` scales the fetched frame (the ONE place).
- `label_props.jl` — `is_spatial_axis` (the physical-axis predicate; excludes `centroid_t`).

### Julia — API
- `gating_api.jl` — `_plot_xy_raw` + the extents/density path (`:532-559`) scale; tick labels read µm;
  `api_gating_channels` serves the unit; the broadcast route guards on calibration.

### Frontend
- `stores/gating.ts` — carry the unit; `colLabel` appends it.
- `utils/gatingAxes.ts` — the pure label helper (+ Vitest).

### Python
- None. (The migration script was dropped — decision 8.)

## Phases

**Phase 1 — the stamp.** `PopulationMap.spatial_unit` + sizes, `to_tree`/`from_tree` round-trip,
`load_pop_map`/`save_pop_map!`. No behaviour change yet (everything reads `"px"`).

**Phase 2 — the scale, in `recompute!`.** Membership honours µm maps. Legacy px maps unchanged.

**Phase 3 — display.** plotdata, extents/density, ticks, the served unit, the axis label.

**Phase 4 — the guard.** Refuse spatial-gate broadcast onto an uncalibrated image.

**Phase 5 — the patch. WRITTEN, VERIFIED, THEN DROPPED (decision 8).** It converted correctly
(500 px → 298.2 µm at 0.596 µm/px) and was idempotent via the stamp, but measured 0 coordinate changes
on real data. Removed rather than shipped as a no-op; decision 5's lazy adoption replaced it.

**Phase 6 — docs.** POPULATION.md (spatial gates are µm, the stamp, the eval-time scale), and promote
the durable parts out of this plan.

## Reservations

- **A pre-existing pixel-space position gate stays in px** (decision 8 ships no converter). It keeps
  evaluating correctly and its axis reads `(px)`, but it will not match a µm gate on another image. No
  such gate exists in any current project — that is why the converter was dropped, not an assumption.
- Membership for a µm gate can only be checked headless against a fixture; the drawing experience
  (draw → save → reload → outline still on the dots) needs a browser.
- An image whose pixel size is *corrected* after gates were drawn in µm will shift those gates. That is
  inherent to storing physical units and is the right trade — but it is a real behaviour change from
  px, where a calibration edit was a no-op for gating.
