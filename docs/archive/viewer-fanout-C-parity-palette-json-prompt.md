# Session C — VIEWER_PARITY Phases 1 + 2: palette + track-mode JSON

Branch: `feat/viewer-parity-palette-json` (fork from origin/main).

## Goal

The browser WebGPU viewer and the Julia offline renderer draw the same picture two ways.
Today `PALETTES.cecelia`, `TrackColorMode` names, the tail window off-by-one and the
heat-ramp stops are duplicated by hand across the two paths — comments cite the other
side but nothing enforces agreement. Move all of these to ONE shared JSON asset that
both sides read.

Full plan: `docs/todo/VIEWER_PARITY_PLAN.md` (Phases 1 + 2 only for this session).

**Phase 3 (parity test) and Phase 4 (settings defaults) come later — do NOT do them
here.** Once this ships, a second session picks up 3 + 4.

## What to build

**Phase 1 — palette JSON.**
1. New file: `frontend/src/plots/palettes.json` (or a `.ts` thin re-export around a
   shared JSON). Every palette keyed by name; `cecelia` first, the twelve current
   colours from `frontend/src/plots/plot.ts`.
2. `plot.ts` imports the JSON; delete the inline literal.
3. Julia reads the same file at boot into `CECELIA_TRACK_PALETTE` (JSON3 →
   `Vector{RGB{N0f8}}`); delete the twelve-colour literal in
   `api/src/overlay_author.jl`. Keep a fallback path used only if the file is missing
   (broken checkout, not a normal state) with a `@warn`.
4. Test: assert the Julia table equals the parsed JSON (a single testset).

**Phase 2 — track colour modes + heat ramp.**
1. Extend the SAME JSON with a `trackColorModes` block: the three names
   (`track` / `speed` / `solid`) and the heat-ramp stops (five RGB triples from
   `_heat_ramp` in `overlay_author.jl` / `heatUnit` in `viewerOverlays.ts`).
2. Both sides read it.
3. Test: assert every mode name known to the browser is accepted by the Julia author
   (no silent fallback to `"track"` on a new mode).

## Files to touch

- **NEW:** `frontend/src/plots/palettes.json` (or split into two if cleaner)
- `frontend/src/plots/plot.ts` — replace inline `PALETTES.cecelia` with the JSON import
- `frontend/src/utils/viewerOverlays.ts` — replace `TrackColorMode` literal + heat stops
- `api/src/overlay_author.jl` — replace `CECELIA_TRACK_PALETTE` + `_heat_stops` with a
  JSON load at module init
- `api/test/runtests.jl` — parity assertion testset
- `frontend/src/**/*.test.ts` — mirror assertion on the frontend side
- `docs/todo/VIEWER_PARITY_PLAN.md` — flip Phase 1 + 2 status to BUILT

## Files NOT to touch

- `api/src/viewer_api.jl` `api_viewer_record_test` handler — **Session A** owns it.
- `api/src/preview_api.jl` + napari preview routes — **Session B** owns them.
- `frontend/src/lib/webgpu/volumeRenderer.ts` — **Session D** owns it.
- Settings defaults (`viewerPointSizePx` etc.) — that's Phase 4, deferred.
- The MASK outline algorithm — Phase 5, parked.

## Existing code to lean on

- `frontend/src/plots/plot.ts` line ~51 for `PALETTES.cecelia`.
- `api/src/overlay_author.jl` line ~118 for `CECELIA_TRACK_PALETTE`.
- `api/src/overlay_author.jl` `_heat_stops` (line ~136) + `_heat_ramp`.
- `frontend/src/utils/viewerOverlays.ts` `heatUnit` + `TrackColorMode`.
- Julia JSON3 pattern for boot-time reads: any of the existing `*.json` config loaders
  in `app/src/`.

## Test obligation

Julia test in `api/test/runtests.jl`:
- Read the JSON, parse to `Vector{RGB{N0f8}}`, assert against `CECELIA_TRACK_PALETTE`
- Read the mode list, assert Julia's `track_color_mode` accepts every entry (no fall
  through to the default warning)

Frontend test in `frontend/src/**/*.test.ts`:
- Import `plot.ts`'s `PALETTES.cecelia` and the JSON, assert equal
- Import `viewerOverlays.ts` `TrackColorMode`, assert `heatUnit` stops match

Both run: `pixi run test-api` + `pixi run test-frontend`.

## Success criteria

1. Both test commands pass.
2. Editing a colour in the JSON, restarting the API + reloading the frontend, shows
   the changed colour in BOTH a browser look AND a Julia-rendered movie — without
   touching `.ts` or `.jl`.
3. `PALETTES.cecelia` inline literal is gone from `plot.ts`.
4. `CECELIA_TRACK_PALETTE` and `_heat_stops` literals are gone from `overlay_author.jl`
   (or reduced to a fallback with a `@warn`).

## Reservations to state before commit

- Fallback behaviour when the JSON is missing (broken checkout vs shipped install) —
  state the choice.
- JSON is loaded ONCE at module init in Julia — a live edit needs an API restart.
  State so.
- If the frontend imports the JSON at build time (Vite), a live JSON edit needs a HMR
  cycle. State whether that's the case.

## Explicit constraints

- **Never start/kill the dev server.** Dominik owns 8080/5173/7655.
- **Never write shared dev config.**
- **Ask before commit; state reservations first.**
- **Branches + PRs only, never push to main.**
- **Copy `.env`** into your worktree.
- **Do NOT touch the parity test or settings JSON in this session** — those are
  separate follow-up work.
