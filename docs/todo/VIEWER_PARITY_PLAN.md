# Viewer parity — one decision, two renderers

**Status:** Phases 1 + 2 BUILT on `feat/viewer-parity-palette-json` (2026-08-27); Phases 3–5 next

## Goal

The browser's WebGPU viewer (interactive) and the Julia offline renderer (batch movies) draw the
same picture two ways. That is deliberate — the offline path can't run WebGPU without a browser
open, and the interactive path can't wait on a CPU compositor. **The primitives can't share code.
The DECISIONS must.** This plan is the list of decisions that are currently duplicated by hand, and
the smallest set of steps that turn each of them into one source of truth plus a test that fails
when they drift.

Read this before adding a new visual knob to either renderer.

## What's shared today (leave alone)

- **Pop → labels → colour**: `resolve_pops` (`app/src/gating/pops.jl`). Server-side, both the
  browser overlay payload (`GET /api/viewer/overlays`) and the Julia author (`build_overlays_for`,
  `build_mask_for`) read it. A colour change in the gating manager flows to both.
- **Contrast + LUT**: saved viewer props → `resolved_display_specs`. One source, one API answer.
- **Camera / view state**: `capture_view_state` / `apply_view_state` (WEB_VIEWER_PLAN.md, decision
  6). Same contract the animation page already speaks.

Nothing on this list needs changing. If a fourth item joins ("both renderers agree on X"), it
belongs here or on the trouble list below — not in a fresh copy.

## What's duplicated today (the trouble list)

Every item here is a hand-edit-both-files trap. The comments in each file cite the other one, but
that's convention, not enforcement — a palette edit in `plot.ts` that never touches Julia ships as
a movie whose track colours look nothing like the live viewer.

1. **`CECELIA_TRACK_PALETTE`** (12 RGB values) — hand-copied from `PALETTES.cecelia` in
   `frontend/src/plots/plot.ts` into `api/src/overlay_author.jl`. The Julia comment says so.
2. **Track colour modes** — `TrackColorMode = 'track' | 'speed' | 'solid'` in
   `frontend/src/utils/viewerOverlays.ts`; re-implemented as `track_color_mode` in
   `overlay_author.jl`. Same three names, same three behaviours, two implementations.
3. **Tail window** — `tailRange` in `viewerOverlays.ts` (`hi = t + 1`, `lo = hi - L + 1`); mirrored
   in `build_overlays_for`'s segment slicing (`[t + 2 - L, t + 1]`). Off-by-one is documented but
   not asserted anywhere.
4. **Heat ramp** for `speed` mode — five viridis-ish stops, hand-transcribed on both sides
   (`heatUnit` in ts, `_heat_ramp` in jl).
5. **Point / segment / mask defaults** — `viewerPointSizePx`, `viewerSegmentWidthPx`,
   `viewerTailLength` in `settings.ts`; matched by hand as the smoke route's defaults.
6. **Mask outline algorithm** — CPU boundary pass (`draw_mask_outline!`) vs shader.
   Necessarily different code, potentially different pixels — a 1-px shader outline and a 1-px
   two-pass CPU outline are not the same shape.

## Locked decisions

**1. Duplication is the price, enforcement is the answer.** We are not going to run WebGPU on the
server. We are not going to keep a browser open for every batch clip. The two renderers stay. What
we add is enforcement that they agree on what to draw.

**2. Shared decisions live in one asset that both sides read.** For each item on the trouble list,
the ONE source of truth is a JSON file the frontend imports at build time and Julia reads at boot.
No codegen step. No round-trip. Editing the JSON is the edit; both sides pick it up.

**3. The parity test compares DECISIONS, not pixels.** A pixel-diff between a shader and a CPU
rasteriser is a losing bet. What the test compares is the columnar overlay payload the browser
would layout for a given `(image, value_name, look, t)` against what `build_overlays_for` emits for
the same tuple. Same cells, same colours, same t-buckets, same segment endpoints. If those match,
the drawing primitives are the only place drift can hide — and a visual smoke test on one fixture
catches that.

**4. Pixels-across-time uses the browser.** When exact WebGPU pixels matter (a single clip from
the live view, a thumbnail card), the browser's `canvas.toBlob()` per frame is the escape hatch.
Julia is for the batch case where "no browser open" is the point.

## Phases

### Phase 1 — palette to shared JSON — BUILT (feat/viewer-parity-palette-json)

- New file: `frontend/src/plots/palettes.json` (or a `.ts` re-export around a shared JSON). Every
  entry keyed by palette name; `cecelia` is the first key, the twelve current colours.
- `plot.ts` reads it at build time; delete the inline literal.
- Julia reads it at boot into `CECELIA_TRACK_PALETTE` (JSON3 → `Vector{RGB{N0f8}}`); the twelve-
  colour literal in `overlay_author.jl` becomes a fallback used only if the file is missing (a
  broken checkout, not a normal state). Comment names the JSON path.
- Test: assert the Julia table equals the parsed JSON at run time. A single-line testset.

**Ship gate:** a colour edit in the JSON changes a browser look AND a Julia movie without either
`.ts` or `.jl` being touched.

### Phase 2 — track colour modes + heat ramp to shared spec — BUILT (feat/viewer-parity-palette-json)

- Extend the same JSON asset with a `trackColorModes` block: the three names, the heat-ramp stop
  list (five RGB triples).
- Both sides read it. The `TrackColorMode` union in ts becomes a runtime array; the Julia
  `_heat_ramp` reads the stops at boot.
- Test: assert every mode name known to the browser is accepted by the Julia author (no silent
  fallback to `"track"` on a new mode).

**Ship gate:** adding a fourth mode is a JSON edit + one shader branch + one CPU branch, not four.

### Phase 3 — the parity test

- New testset (`api/test/`), one fixture (the labelProps + tracked segmentation the overlay tests
  already use).
- Load `resolve_pops` + `pop_df` the way the browser payload does (`api_viewer_overlays`); build
  the client-side layout in Julia (there's already an `overlays.ts` helper to inline).
- Call `build_overlays_for` on the same fixture, same `t`s, same `look`.
- Assert per-t: same cell count, same colour multiset, same segment endpoint set. Numeric tolerance
  on coordinates (rounding differs one drawn pixel between float shaders and Julia `Int(round)`).
- Same shape for `build_mask_for` vs the browser's mask id → colour dict.

**Ship gate:** a palette drift, a tail-window off-by-one, a pop that changed colour on one side and
not the other — all fail the test with a legible diff.

### Phase 4 — settings defaults to the same asset

- `viewerPointSizePx` / `viewerSegmentWidthPx` / `viewerTailLength` in `settings.ts` become
  imports from the shared JSON; the smoke route's defaults read the same file.
- Small change, but it's the last hand-mirrored constant on the record path.

### Phase 5 (deferred) — mask outline algorithm

The CPU two-pass outline and the WebGPU shader outline don't need to be pixel-identical, but they
should be recognisably the same width and connectivity. If the parity test's mask branch flags
"different id set per frame", the two algorithms are drawing different boundaries and we look at
this then. Parked until the test says so.

## Non-goals

- **A shared drawing library across GPU + CPU.** Doesn't exist in Julia; writing one is a plan of
  its own and this one exists to avoid that.
- **Pixel-diffing the browser vs Julia frames.** Different rasterisers, different antialiasing;
  the diff is noise. Phase 3's decision-level test is what buys the parity claim.
- **Serving movies through a headless browser.** Sold and rejected in the audit
  (`NAPARI_WEBGPU_AUDIT.md`, `CLOUD_MIGRATION_ASSESSMENT.md` §3b). Julia batch renderer is the
  answer.

## Cross-references

- [`WEB_VIEWER_PLAN.md`](WEB_VIEWER_PLAN.md) — the parent plan (browser + offline renderer). This
  plan is the maintenance discipline for the shape it built.
- [`NAPARI_WEBGPU_AUDIT.md`](NAPARI_WEBGPU_AUDIT.md) — why we have two renderers at all.
- `frontend/src/utils/viewerOverlays.ts` — the browser layout code the parity test mirrors.
- `api/src/overlay_author.jl` — the Julia author the parity test compares against.
