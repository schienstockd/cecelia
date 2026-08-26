# The 2D viewer: pan/zoom, per-viewport tiles, cache + prefetch

Status: OPEN — Phases B, C, D and E (halo) shipped in PR #660; Phase F (timecourse × tiles) is next; velocity-prefetch stretch not scheduled.

Follow-up to the spatial-buffering audit (`docs/archive/spatial-buffering-pyramid-prompt.md`).
The audit's Phase 1 (measurements) and Phase 2 server surface shipped in PR #659
(`open_level`, `read_slab(x, y, level)`, `/api/viewer/slab?x&xTo&y&yTo&level`, meta `levels`).
The 3D volume view now defaults to the coarsest level to survive `maxBufferSize`.

**This plan covers what #659 did NOT ship**: the 2D pan/zoom viewer that turns the browser
into a slippy-map over a large XY tilescan (e.g. `f8gzA2`, 20329×16898). Same server surface,
new client behaviour.

## Why this is a different shape from the timecourse

The timecourse viewer fetches ONE (t, c) whole plane and pans within the loaded texture
(`panX`/`panY` in the shader). That worked because the intravital images cap at ~1024×1024 —
one plane fits comfortably in VRAM. On a whole-slide tilescan, one plane at L0 is 687 MB per
channel (`f8gzA2`), which:

- Exceeds WebGPU `maxBufferSize` (256 MB on the Dawn adapter observed).
- Wastes 99% of the fetched bytes when the viewport shows 1/100th of the frame.
- Cannot be zoomed into progressively — the coarsest level shows detail no higher than L0.

The pan/zoom viewer's access pattern is closer to a slippy map (Google Maps, deepzoom): a
small window over an enormous plane, LOD picked by zoom, tiles fetched on demand as the
window moves. Same underlying zarr/slab primitives (`#659`), different eviction and prefetch
logic.

## Locked decisions

1. **The 2D view fetches by TILE, not by whole plane.** The audit's Phase 2 measurement
   showed a viewport at L0 covers ~4-6 chunks against ~340 whole-plane chunks; per-tile
   requests are the only way to make this interactive.
2. **A tile is one 1024×1024 zarr chunk × 1 c × 1 z × 1 t.** No bespoke tile size — the
   native chunk shape is 2 MB u16, which is a reasonable request size on real data. Blocks
   of chunks assembled server-side (a viewport spans up to ~9 chunks at L0) beat naive
   per-chunk browser fetches (audit's Phase 2, mirrors the temporal work's own 4.2× win).
3. **LOD selection is on the client, pure function of viewport zoom.** Formula
   `level = clamp(floor(log2(zoom)), 0, nLevels-1)` where `zoom = imagePxPerDevicePx`.
   Already tested (`pickTileLevel`, #659).
4. **Cache key = `(level, tileX, tileY, c)`.** No `z` component — the 2D view is
   z=const-per-fetch, and Z>1 tilescans belong to the volume renderer's LOD path (#659's
   `pickVolumeLevel`) rather than to the tile viewer. Confirmed by measurement: `f8gzA2` is
   Z=1, and z=const holds for every whole-slide case examined.
5. **LRU eviction is 2D-aware, not recency-only.** Distance-from-current-viewport ranks
   candidates; a tile just outside the viewport is more valuable than a stale tile two
   viewports away, however recently touched.
6. **Progressive refinement, not blank-then-fresh, on level change.** When zoom crosses a
   level boundary, coarse tiles stay resident and are drawn UNDER newer finer tiles as they
   arrive. Never blank the canvas — same spirit as the temporal work holding contrast
   constant across frames.
7. **The tile viewer is 2D-only.** The 3D volume view stays on `pickVolumeLevel` (whole
   volume at the coarsest fitting level); it is a different renderer already (raycaster vs
   flat sampler) and does not share tile infrastructure. Two view modes, one tile system for
   the flat one.

## Phases

### Phase A — 2D "auto-fit" level (`pickPlaneLevel`) — SUPERSEDED

Originally shipped in PR #660: a static byte-budget picker that chose the finest level
whose whole-plane fetch fit under `PLANE_LEVEL_BUDGET_BYTES = 200 MB`. Dominik pushed
back: "why does it load l1 for 2d? that should depend on the zoom level of the camera —
that's the whole point of the pyramids." He was right — a static per-session level
ignores what the pyramid exists for. `pickPlaneLevel` + `PLANE_LEVEL_BUDGET_BYTES` were
deleted with their tests in the same PR; Phase B replaces it.

### Phase B — Zoom-triggered LOD swap (whole plane) — SHIPPED (PR #660)

`slabLevel` in plane mode is now `pickTileLevel(camZoom, meta)`. `camZoom` = L0 pixels
per device pixel, derived from `cam.dist`, `VIEW_HALF_ANGLE`, `voxelUm[1]`, and the
canvas' device-pixel height. A watch on `slabLevel` schedules a debounced (150 ms)
`reallocate(false)` when the level drifts from `loadedLevel` (the level the current
textures were allocated for). A wheel gesture that crosses two thresholds refetches
once at the settled level.

The whole-plane fetch pattern is unchanged: at level L, one texture per channel holds
the entire XY plane at that level's dims. What changed is which L is picked and when.

**Ceiling this hits**: at zoom=1 (magnified to 1:1) on a whole-slide image, L0 still
doesn't fit (687 MB > 256 MB adapter buffer cap). Phase C (per-viewport tiles) is the
answer to zoom-all-the-way-in on whole-slide data. Phase B ships if the user zooms
INTO an image far enough to want detail, gets the level below fit, and doesn't ask
for L0 at 1:1 on a `f8gzA2`-sized store. That's the current envelope.

**Test coverage**: `pickTileLevel` is unit-tested; the wire-up in `ViewerWindow.vue`
is not (extracting a component-mounting testable shape is a follow-up).

### Phase C — Per-viewport TILE fetching — SHIPPED (PR #660)

The actual slippy-map: fetch only the tiles the viewport intersects, at the LOD picked
from zoom. The single-texture design is replaced with a tile atlas (or one texture per
tile — decision C1).

- **C1 (decision)**: tile atlas (one big texture, tile slots) vs many small textures
  (WebGPU can indirect-draw). Measure: the atlas is one bind group and one draw but
  fragments on eviction; per-tile textures are `O(tiles)` bind groups but stable. Pick
  based on measured allocation overhead.
- Viewport → intersecting tile-keys → fetch missing → composite in a fragment shader that
  samples the atlas / iterates tile slots.
- Cancellation: a pan mid-fetch invalidates tiles no longer on screen — abort them before
  they land, not just on arrival (audit's Phase 4).

**Fails if**: WebGPU's max bind-group count / max texture-view count runs out on a large
viewport. Estimate: 1080p at L0 with 1024² tiles is 2×2 = 4 tiles per channel × 8 channels
= 32 bind entries — well under any adapter's cap. Should be safe.

### Phase D — 2D-aware LRU + progressive refinement — SHIPPED (PR #660)

The cache from Phase C shipped with a naive recency LRU. This tightens it:

- Eviction rank = f(distance from current viewport, level distance from current level,
  recency). A tile just outside the viewport survives an eviction round; a stale
  three-viewports-away tile does not.
- On level swap, don't blank the newly-empty finer level's tiles — keep the coarser
  level's tiles resident and drawn UNDER the finer tiles as they arrive. The frame is
  never blank between zoom levels.

**Fails if**: the "keep two levels resident" cost approaches the byte budget on very
zoomed-out views. Measure worst-case (deepest level's whole tile set + current level's
viewport tiles) — expected: `deepest level ≤ 1 tile × nC` (that's what "deepest" means),
so negligible.

### Phase E — Prefetch — HALO SHIPPED (PR #660); velocity-based is stretch and NOT scheduled

- **Halo**: fetch the tiles one ring beyond the current viewport, so a small pan into
  freshly-visible tiles is instant.
- **Velocity-based** (stretch): during a drag, predict the pan direction and prefetch
  further along it. Only build this if the halo turns out not to be enough — the halo is
  simpler and works for zoom + arbitrary pan directions.

**Fails if**: halo cost dominates the fetch stream on slow networks. Fine on loopback;
remote/HPC setups may want a "halo=0" setting.

### Phase F — Timecourse × tiles

Big intravital timelapses (multi-timepoint, planes over the tile threshold — e.g. a wide-XY movie
where each timepoint's plane alone exceeds 200 MB) currently hit the volume path and OOM: the
tile pipeline is gated `nT ≤ 1` because the tile key has no time dimension, and caching tiles
across timepoints would fight for slots with no ordering signal. This phase adds `t` to the tile
key + a cross-time distance penalty in the eviction ranker, and drops the gate.

**Locked decisions**

1. **Add `t` to `TileKey`.** String form becomes `T${t}/L${level}/x${tx}/y${ty}` — one namespace,
   no ambiguity with the level's own `tx`. Every downstream ranker/pump entry gains the same
   field.
2. **`TileEntry` gains `t`.** The atlas doesn't care about time — slots hold whatever the pump
   uploads — but the ranker needs `t` on every resident tile to compute the time penalty.
3. **`tileEvictions` gets a cross-time distance penalty.** Same shape as the level penalty
   already there, but LARGER: a wrong timepoint is worse than a wrong resolution. Suggested
   value: `10_000_000 * |e.t - centre.t|` on top of the level and Chebyshev terms. Confirmed by
   measurement — if too aggressive, cross-t caching disappears; if too weak, scrubbing feels
   like a full refetch. Tuning is a one-shot after the wire-up.
4. **Prefetch is current-t only in MVP.** No cross-t halo. Reason: the atlas is capacity-bound
   (≈ 30 slots on the whole-slide shape) and multiplying by a t-halo of 2 would leave no room
   for the spatial halo. If scrubbing feels bad, add cross-t after measuring.
5. **`useTiles` gate drops `nT ≤ 1`.** New gate: `mode === 'plane' && needsTiling`.
6. **The timepoint slider schedules the tile pump in tile mode.** `gotoT` currently calls
   `schedulePump` (the timepoint pump) unconditionally. In tile mode, it schedules the tile
   pump instead; `showT` becomes a no-op (there's no per-timepoint texture to bind — the atlas
   holds mixed-t tiles and the ranker filters for draw). `shownT` still flips on the first
   tile that lands for the current `t`, driving the still-overlay gate.
7. **No cross-t auto-window recompute.** `autoWin` stays first-tile-only. A new timepoint
   doesn't rebase it; consecutive Auto presses land at the same place, same discipline as the
   still image.
8. **Atlas capacity is unchanged.** Same per-slot budget. Timepoint churn happens at the
   ranker level, not the atlas level.

**What ships**

- `utils/tileViewer.ts`: `TileKey.t` added, `tileKeyStr` includes it, `tileEvictions` takes
  `centre.t` and applies the time penalty. Its unit tests gain a cross-t case (a same-position
  wrong-t tile ranks farther than a same-t neighbour).
- `lib/webgpu/tileRenderer.ts`: `TileEntry.t` added; `uploadTile` records `key.t`.
- `modules/ViewerWindow.vue`:
  - `useTiles` drops `nT ≤ 1`.
  - `missingTiles`, `evictionKeepSet`, `fetchTile` thread current `t.value` into keys and
    slab URLs (currently hardcoded `t: 0`).
  - `gotoT` schedules the tile pump when `useTiles` is true; `schedulePump` (timepoint) is
    tile-mode-skipped.
  - Timepoint scrub cancels stale-t tile fetches (already the shape `scheduleTilePump`
    aborts non-keep — extend the keep set to current-t only).

**What does NOT ship**

- Cross-t prefetch window. Ship only if scrubbing feels bad after Phase F lands.
- Autoplay through tile mode. Scrub first; play later — a play tick simply advances `t` and
  the same tile pump handles the rest, but the fetch-vs-frame-rate discipline needs
  measurement first.
- Reference-frame caching (freeze one t as background under the current). Would need atlas
  slots reserved outside the ranker's control — not for this phase.

**Fails if**: atlas capacity forces cross-t thrashing on scrub. At f8gzA2 shape
(`nch = 24 → cap ≈ 30 slots`) with viewport + halo ≈ 20 tiles, a t-swap fills the atlas with
new-t tiles and evicts all old-t within one round. The fix is measured: a wider atlas
(higher `SAFE_CACHE_BYTES` or lower `nch`) or multi-tile batching on the server (already
reserved in the plan) buys headroom. Do not preemptively expand.

**Test coverage**

- Unit tests for the extended `TileKey`, `tileKeyStr`, and `tileEvictions` cross-t ranking.
- A wire-up smoke test would want a mountable `ViewerWindow` — that's the extract-for-tests
  follow-up already flagged. Not gated on this phase.

## Server surface — already shipped in #659

- `/api/viewer/slab?level=&x=&xTo=&y=&yTo=` returns an XY rect at a chosen level.
- `/api/viewer/meta` returns `levels: [{level, nX, nY, chunkX, chunkY}]`.
- `X-Slab-Level` header echoes the served level (post-clamp).
- No further server changes are required for Phase A-E. If Phase C measures per-tile
  request overhead as the bottleneck, we can add multi-tile batching (`x=` accepts a list)
  — but only if measured, not preemptively.

## Test plan (per phase)

- Pure logic goes into `frontend/src/utils/volumeViewer.ts` (or a new `tileViewer.ts` if
  the code volume warrants it) and gets unit tests. Component-level tests are not written
  here — extract the logic first, per `frontend/CLAUDE.md`.
- Server-side tests exist for the slab route already; if per-tile behaviour surfaces
  server bugs, add regressions to `api/test/runtests.jl`.
