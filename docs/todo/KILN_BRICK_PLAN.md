## Kiln brick atlas — LOD-in-3D for the WebGPU viewer

**Status:** planning · branch `feat/kiln-brick-port` · reference: [Kiln (mpanknin/kiln-render)](https://github.com/mpanknin/kiln-render) (GPL-3-compatible)

**Goal.** Bring pyramid LOD to the **3D** path of the WebGPU viewer. The 2D tile path already loads
one level per viewport, hysteresis-clamped ([`WEB_VIEWER_PLAN.md`](WEB_VIEWER_PLAN.md) → Decision 4,
[`VIEWER_TILES_PLAN.md`](VIEWER_TILES_PLAN.md)); the 3D path still loads the **whole volume at one
pyramid level** via `pickVolumeLevel` (defaults to the coarsest — `volumeViewer.ts:199-204`). That
was fine when every store was single-level (`WEB_VIEWER_PLAN.md` → Decision 7, since revised: SispLk
has 6 levels), but on real 3D tilescans it costs a full reload on every zoom and never renders at L0
in the visible region.

**What Kiln buys us.** Kiln packs a virtual-texture-style indirection over a physical **brick
atlas**: the world is divided into fixed-size 3D cells, each cell can be resident at *any* pyramid
level, and only cells the frustum touches are loaded. Three concrete wins over our current 3D path:

1. **Per-region LOD in 3D.** Near-camera bricks render at a finer level than far bricks. Today
   every voxel is at one level — usually the coarsest we can afford, because the atlas is sized to
   the whole volume.
2. **View-driven residency.** Only bricks the frustum touches get loaded. On `SispLk` (7848×7293×4,
   38 ch, uint8, 8.7 GB at L0) the visible region can render at L0 while off-screen bricks stay
   coarse or absent.
3. **Cheap LOD swap.** A brick refined from L2 → L1 is one atlas slot rewritten, not a whole-volume
   reload. That's the plane-change / zoom-in behaviour that feels broken today.

This is **not** a bet on deep-Z volumes — SispLk/35uedD have nZ=4 — so bricks are a **3D halo**
around what's visible, not a Z-streaming device. Same atlas, different geometry.

## Locked decisions

1. **Vendor Kiln into `frontend/src/lib/webgpu/kiln/`** (GPL-3 compatible with our GPL-3-or-later
   licence — [`project_license`](../../MEMORY.md#license)) with an attribution comment naming the
   upstream commit. What we adopt: the physical brick atlas, page-table indirection, and the SSE
   LOD scheduler. What we rewrite: texture format, channel model, T axis, halo/prefetch policy.
2. **Brick shape = 128×128×`min(brick_z, nZ)`.** For SispLk/35uedD (nZ=4) `brick_z` collapses to 4:
   a single Z-slab, tiling only in XY against the pyramid. For future nZ ≫ brick_z we tile in Z as
   well. Same atlas machinery, geometry only.
3. **Atlas format keys on `meta.bytesPerVoxel`.** `r8uint` for Manual IBEX (`|u1`), `r16uint` for
   Automated IBEX (`>u2`) — the same branch shipped in #684 for the 2D atlas. Neither is filterable;
   MIP reads `textureLoad` and doesn't need it. Kiln's own `r16float`/filterable path is dropped.
4. **N-channel WGSL, not Kiln's 4-channel fixed layout.** Cecelia's stores are 4–38 channels; the
   channel axis stays stacked along Z in the atlas texture (same convention as the flat-atlas path
   — `WEB_VIEWER_PLAN.md` → Decision 2), so the WGSL loops `nChannels` times per brick sample.
5. **T axis is real; brick residency keys include `t`.** Kiln has no time axis. Cache key becomes
   `(t, brick_x, brick_y, brick_z, level)`; the existing timepoint LRU (`tileEvictions`
   `timePenalty`) generalises with a `tPenalty` on the brick cache.
6. **3D halo = one ring of bricks around the visible frustum**, at the same level as the visible
   brick. The name Dominik coined for it (2026-08-28) — same idea as the 2D `haloPrefetch` but a 3D
   neighbourhood shell around what the camera sees. Not a Kiln concept; grafted on.
7. **Data source is the existing Julia slab endpoint — verbatim, no new route.** A brick request
   is a bounded slab: `try_serve_slab` at `api/src/viewer_api.jl:320` already accepts
   `(t, c, z, zTo, x, xTo, y, yTo, level, enc)` with clamping, level selection and zstd. Adding a
   `slab_brick(...)` helper would be a second answer to a question that has one; the client
   composes brick URLs from the existing route. Only new work server-side is a per-brick
   measurement gate (see P0). No client-side chunk assembly.
8. **Fallback is the current `pickVolumeLevel` path**, kept behind a `viewerBrickEnabled` flag until
   Phase 4 validates parity. The old path exits when the flag lands in `main`, not before.

## Atlas sizing — grounded in SispLk / 35uedD

Both stores are `[T=1, C, Z=4, Y, X]`, uint8, 6 pyramid levels, chunk `[1,1,1,1024,1024]`.

| | nC | nY | nX | L0 vol | Bricks at L0 (128²×4) | VRAM per brick (bytes) |
|---|---|---|---|---|---|---|
| SispLk | 38 | 7293 | 7848 | 8.7 GB | 61 × 57 = 3477 bricks per z-slab (× 38 ch stacked) | 128·128·4·38·1 = 2.4 MB |
| 35uedD | 25 | 6543 | 12977 | 8.5 GB | 102 × 51 = 5202 bricks per z-slab (× 25 ch stacked) | 128·128·4·25·1 = 1.6 MB |

A 1024×1024×1024 physical atlas at r8uint holds ~419 SispLk bricks (~1 GB VRAM), which is well
under `maxBufferSize=1 GiB` and covers a viewport-sized halo at L1 without evicting L2 coarsest.
Concrete atlas dimensions live in the atlas manager and are computed from
`limits.maxTextureDimension3D` — not baked here — but the 1024³ number is the design centre.

## Phased build sequence

Each phase is independently shippable; each ends with a green `pixi run test-frontend` and a
manual browser check on SispLk + 35uedD.

**P0 — Brick-size + fetch measurement gate (server-side).** No new endpoint: `try_serve_slab` at
`api/src/viewer_api.jl:320` already answers `(t, c, z, zTo, x, xTo, y, yTo, level, enc)` — a brick
IS a bounded slab. What P0 delivers is a per-brick fetch benchmark against SispLk (uint8, 8.7 GB
L0) and 35uedD (uint8, 8.5 GB L0) at brick sizes 64/128/256 and pyramid levels 0-2, so brick size
(Decision 2) is a measured number rather than a Kiln inheritance. Bench script under
`docs/todo/spike/webgpu/`, JSON result files under the same, and the plan's brick-size decision
either confirmed or amended based on the numbers.

**P1 — Vendor Kiln into `frontend/src/lib/webgpu/kiln/`.** Import the physical atlas + page table +
SSE scheduler unchanged. Add an attribution comment naming the upstream commit hash. Wire nothing
into `ViewerWindow.vue` yet.

**P2 — Rewrite texture format + channel model.** Fork Kiln's `r16float`/filterable path to
`r16uint`/`r8uint` keyed on `bytesPerVoxel`, N-channel WGSL loop, `textureLoad` nearest. Reuse the
`atlasBPV` reuse-check pattern from `tileRenderer.ts` (#684).

**P3 — Wire the Julia provider.** Replace Kiln's stub loader with `fetch('/viewer/brick?...')`;
brick cache key = `(t, brick_x, brick_y, brick_z, level)`; LRU with `tPenalty` mirroring
`tileViewer.ts`. Behind `viewerBrickEnabled` (default off).

**P4 — 3D halo + SSE scheduler.** One ring of neighbour bricks at the visible level; SSE-driven
LOD selection with hysteresis (port the `TILE_LOD_HYST_LOG2 = log2(1/0.7)` constant from PR #682
once merged). Instrument with a `?bricks` debug overlay showing residency, level per brick, and
eviction reason (mirrors the tile mini-map).

**P5 — Integrate.** Toggle `viewerBrickEnabled` in dev; verify SispLk and 35uedD render at L0 in
the visible region and coarsen away from the camera. Compare visible frame time against the
current whole-volume path on Dominik's RTX 2000 Ada; the audit's 5.3 ms MIP budget is the target
that must NOT regress.

**P6 — Overlays.** Points, tracks, tails, masks continue to work — they're screen-space and don't
touch atlas geometry. Confirm no regression on `zolIMa` / `fXgbTl` reference movies.

**P7 — Retire the flat-atlas 3D path.** Delete `pickVolumeLevel` from the 3D render loop (keep it
for the 2D mini-map indicator until that surface has its own answer). Flip `viewerBrickEnabled` to
default on. Update `WEB_VIEWER_PLAN.md` → Decision 7 to point at this plan.

## What this plan is NOT

- **Not a Kiln clone.** We adopt three components (physical atlas, page-table indirection, SSE
  scheduler). Everything else (texture format, channel model, T axis, halo, data source, WGSL
  main) is rewritten.
- **Not a Z-streaming device.** SispLk/35uedD are XY-heavy, not Z-deep. The value is LOD-in-3D and
  view-driven residency, not streaming through 1000+ z planes.
- **Not a 2D-path change.** The 2D tile pipeline (`tileRenderer.ts` +
  [`VIEWER_TILES_PLAN.md`](VIEWER_TILES_PLAN.md)) stays as it is; #684's `TileKey` z-axis is a
  separate correctness fix.

## Open questions

1. **Physical atlas dimensions vs. `maxTextureDimension3D`.** 1024³ is the design centre; some
   integrated GPUs cap at 2048 which is fine, but a headless CI environment might not. Query
   `limits` first (same pattern as the audit — `WEB_VIEWER_PLAN.md` → Decision 3) and downgrade
   the atlas rather than refusing to render.
2. **Brick size 128 vs. 64.** Kiln uses 128; the SSE scheduler's hysteresis band widens with brick
   size (larger bricks = fewer LOD swaps = smoother, but coarser per-region granularity). Measure
   in P4 on SispLk. Not a locked decision yet.
3. **T-axis prefetch policy.** Same "cache adjacent timepoints under a byte budget" question as
   the 2D tile cache, resolved in `VIEWER_TILES_PLAN.md` Phase F. Port the answer, don't
   re-derive.

## References

- Upstream: [`mpanknin/kiln-render`](https://github.com/mpanknin/kiln-render) (GPL-3)
- Audit that grounds the perf targets: [`NAPARI_WEBGPU_AUDIT.md`](NAPARI_WEBGPU_AUDIT.md)
- 2D LOD conventions to mirror: [`VIEWER_TILES_PLAN.md`](VIEWER_TILES_PLAN.md), PR #682 (2D LOD hysteresis, `cecelia-lodhyst`)
- Locked decision this plan revises: [`WEB_VIEWER_PLAN.md`](WEB_VIEWER_PLAN.md) → Decision 7
- Uint8 support (prereq for using SispLk/35uedD as reference): #684 (`fix/viewer-uint8-support`)
