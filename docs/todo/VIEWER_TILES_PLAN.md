# The 2D viewer: pan/zoom, per-viewport tiles, cache + prefetch

## Status

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

### Phase A — 2D "auto-fit" level (`pickPlaneLevel`)

**One PR** that unblocks 2D on big-XY images without any tile work. The 2D view still
fetches a WHOLE plane, but at a level whose whole-plane bytes fit under a byte budget.
Small, complete, incremental — proves the level plumbing works in the 2D path.

- Add `pickPlaneLevel(meta, budgetBytes, override?)`: finest level where
  `nX[l] * nY[l] * bpv * nC ≤ budgetBytes`, else deepest.
- In `ViewerWindow`, `slabLevel` in plane mode uses `pickPlaneLevel`; the 2D sidebar gets a
  "Level" dropdown mirroring the 3D one (auto = pickPlaneLevel, or force a level).
- No zoom-triggered LOD switching yet. Zoom in past the auto level's native resolution and
  the renderer upscales.

**Fails if**: a small-XY intravital image (~1024×1024) picks a coarser-than-L0 level and
loses detail people are used to. Budget must be sized so L0 always fits for intravital
data (~4 MB/channel u16 is fine; the budget only kicks in for whole-slide sizes).

### Phase B — Zoom-triggered LOD swap (whole plane)

Keep the whole-plane fetch but re-fetch at a new level when the user zooms across a
threshold. The renderer's texture is always "current level's whole plane"; the shader's
`panX`/`panY`/`dist` sample within it as before.

- On zoom, compute `level = pickTileLevel(zoom, meta)`; if it differs from the current
  level, cancel in-flight, refetch, reallocate the texture.
- Debounce zoom so a single wheel gesture doesn't fire N refetches — `debouncedLatest` on
  the fetch, not the zoom itself.

**Fails if**: at zoom=1 on a whole-slide image, L0 still doesn't fit (687 MB > 256 MB
buffer). This is the ceiling that pushes us to Phase C. Phase B ships if the largest
INTRAVITAL case works at zoom=1; whole-slide 1:1 needs tiles.

### Phase C — Per-viewport TILE fetching

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

### Phase D — 2D-aware LRU + progressive refinement

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

### Phase E — Prefetch

- **Halo**: fetch the tiles one ring beyond the current viewport, so a small pan into
  freshly-visible tiles is instant.
- **Velocity-based** (stretch): during a drag, predict the pan direction and prefetch
  further along it. Only build this if the halo turns out not to be enough — the halo is
  simpler and works for zoom + arbitrary pan directions.

**Fails if**: halo cost dominates the fetch stream on slow networks. Fine on loopback;
remote/HPC setups may want a "halo=0" setting.

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
