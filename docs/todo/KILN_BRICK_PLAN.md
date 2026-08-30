## Kiln brick atlas — LOD-in-3D for the WebGPU viewer

**Status:** **superseded** (2026-08-29) by [`BRICK_INTEGRATION_PLAN.md`](BRICK_INTEGRATION_PLAN.md).
P0–P5 shipped through PRs #691–#704 (see git log). The remaining work — auto-select from meta, close
the f8gzA2 shader-scaling regression, retire the flat 3D path — moved to the successor plan, which
is grounded in the 2026-08-29 bench blobs the old plan didn't have. Keep this file as a decision
record for the ~30 PRs that built the renderer; do NOT continue design work here.

**Original status:** planning · branch `feat/kiln-brick-port` · reference: [Kiln (mpanknin/kiln-render)](https://github.com/mpanknin/kiln-render) (GPL-3-compatible)

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

1. **Take Kiln as a reference, not a vendored fork** (Dominik, 2026-08-28: "not gospel — what we
   can use here, not port the entire mechanism"). What we adopt as *concepts*: the physical brick
   atlas, page-table indirection, and the SSE-with-hysteresis LOD scheduler. What we write from
   scratch, cecelia-shaped: everything below — r8uint/r16uint texture, N-channel WGSL, T axis,
   3D halo, hooked to `/api/viewer/slab`. Attribution to `github.com/mpanknin/kiln-render` in a
   header comment on `brickAtlas.ts` for the ideas; no imported code.
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
7. **Data source is the existing Julia slab endpoint, extended by a `cTo` param.** A brick request
   is a bounded slab: `try_serve_slab` at `api/src/viewer_api.jl:320` already accepts
   `(t, c, z, zTo, x, xTo, y, yTo, level, enc)`. But it takes a single `c` — measured cost of
   fetching one brick × all channels serially is **273 ms on SispLk (nC=38)** and **190 ms on
   35uedD (nC=25)** at brick 128 L0, dominated by per-request overhead (single-channel is ~7 ms
   whether the brick is 64² or 256²). A 3×3 brick viewport = ~2.5 s. So the route grows a `cTo`
   parameter promoting `c` to a range (same shape as `z`/`zTo`), and the client fetches one
   brick × all-channels per request. All-channels-per-brick is the only mode the client uses; the
   single-`c` path stays for backward compatibility with the flat atlas. No client-side chunk
   assembly. Numbers: [`docs/todo/spike/webgpu/p0_brick_bench.json`](spike/webgpu/p0_brick_bench.json).
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

**P0 — Brick-size + fetch measurement gate (server-side). ✓ done 2026-08-28.**
[`brick_bench.jl`](spike/webgpu/brick_bench.jl) hits `/api/viewer/slab` at brick 64/128/256 ×
levels 0/1/2 on SispLk (uint8, 8.7 GB L0) and 35uedD (uint8, 8.5 GB L0), warm reads only. Results
in [`p0_brick_bench.json`](spike/webgpu/p0_brick_bench.json).

Findings:

- **Per-request overhead dominates.** A single-channel brick is 6–8 ms whether it's 64² (16 KB) or
  256² (256 KB). Payload only starts to matter above ~1 MB (L2 B256 → 22 ms).
- **All-channels serially is not viable at 38 channels.** SispLk L0 B128 = **273 ms/brick × 9
  visible bricks = ~2.5 s/frame**. Motivated the `cTo` extension in Decision 7.
- **Brick size 128 is confirmed** — same cost as 64 at fewer requests per viewport; 256 is a wash
  at L0 but starts to cost at deeper levels where read time takes over.
- **The measurement gate rejected the naïve plan.** Without the `cTo` extension, P3 would ship a
  visibly slower 3D view than the current whole-volume path. Amended Decision 7 accordingly
  before writing frontend code.
- **`cTo` implemented in P0.5 (2026-08-28); bench re-run confirms the projection.** All-channels
  batched matches single-channel cost: SispLk L0 B128 dropped from 273 ms serial to **7.9 ms
  batched (34.3× speedup)**. A 3×3 visible viewport is 71 ms, not 2.5 s. The per-request overhead
  was the whole story — payload size barely moves the needle at L0/L1 (2.4 MB for all 38 channels
  of a 128² brick fetched in 8 ms).

**P1 — Write the three brick primitives from scratch, cecelia-shaped.**
Pure-logic modules under `frontend/src/utils/` (per `frontend/CLAUDE.md`: tests run only on
`src/utils/*.ts`; same split as `tileViewer.ts` alongside the WebGPU-side `tileRenderer.ts`):
`brickAtlas.ts` (slot allocation + LRU for the physical atlas), `pageTable.ts` (virtual → physical
brick indirection), and `sseLod.ts` (SSE per brick + hysteresis, reuses `TILE_LOD_HYST_LOG2` from
`volumeViewer.ts:200`). Header comment on each: "concepts from github.com/mpanknin/kiln-render,
cecelia implementation". Unit-tested in matching `*.test.ts`. No wiring, no runtime effect. The
WebGPU-side wrapper (physical 3D texture allocation, `writeTexture`) lands in P2 as
`frontend/src/lib/webgpu/brickAtlasTexture.ts`.

**P2 — WebGPU brick atlas texture wrapper (allocation + brick write). ✓ scaffolded 2026-08-28.**
`frontend/src/lib/webgpu/brickAtlasTexture.ts` — `createBrickAtlasTexture(device, layout, limits,
onError?)` allocates the physical 3D texture at `r8uint | r16uint` keyed on `bytesPerVoxel` (same
branch as #684), wraps `writeTexture` for a one-brick × all-channels payload from
`/api/viewer/slab?cTo=nC-1` (Decision 7), routes one write per channel to `slot × brickZ × nC + c
× brickZ` along Z (Decision 4), same OOM discipline as `volumeRenderer.ts`. Shader, bind group,
and draw loop deferred to P4 alongside the 3D-halo scheduler — the physical texture has to exist
and be writeable first. `canReuseAtlas` (in `brickAtlas.ts`, tested) is the dtype-safety gate
that catches the #684 "byte length" trap on any layout change.

**P3 — Wire the existing slab endpoint as the brick data source. ✓ scaffolded 2026-08-28.**
`frontend/src/utils/brickLoader.ts` — `brickSlabUrl(base, brick, nC, brickSizeVox)` builds a
`/api/viewer/slab?cTo=nC-1` URL for one VirtualBrick, `brickShapeError` validates the P0.5
4-tuple `X-Slab-Shape` header and byte count, `fetchBrick` is the thin fetch wrapper. Refuses
the legacy 3-tuple response — a scalar-c fallback would upload `nz*ny*nx` bytes into a slot
sized for `nc*nz*ny*nx` and draw a shifted image with no error (the #684 trap). `SlabQuery`
grew `cTo?: number` in `volumeViewer.ts`; flat-atlas callers are unchanged (scalar `c` alone
never emits the param). Not yet plumbed into a scheduler or `ViewerWindow.vue` — the LRU and
`viewerBrickEnabled` flag live in P4 alongside the halo prefetch.

**P4 — 3D halo + SSE scheduler. ✓ scaffolded 2026-08-28; P4.1 anisotropic-z amended 2026-08-28.**
`frontend/src/utils/brickScheduler.ts` — `bricksIntersectingViewport(view, world, level)` walks
the brick grid at a level and returns core + 1-ring halo bricks with a Chebyshev distance rank
(same shape as `tileEvictions` in `tileViewer.ts`). Halo is a **full 3D ring** — one brick wider
in x, y AND z. `pickBrickLevel` composes `sseDesiredLevel` + `sseLevelWithHysteresis` per axis
and takes the **MIN of xy and z** desired levels so an anisotropic-z store (thick vibratome,
vz ≈ 3-10× vxy) doesn't undersample z. `scheduleBricks(view, world, resident, previousLevel)`
is the frame decision: `{ level, toLoad, toEvict }`. XY-only reduces to the special case where
the viewport's `halfDUm ≥ nZ * voxelUmZ / 2` — SispLk-shape (nZ=4) reproduces the pre-amendment
"walk every z-slab" behaviour unchanged. Debug overlay + shader-side integration land in P5.

**Why P4.1 landed before P5, not after data arrives (2026-08-28):** Dominik confirmed thick
vibratome images are coming but no reference image exists yet. Extending the scheduler's API
now — `centreUm: [x, y, z]`, `halfDUm`, anisotropic voxel scaling — is purely additive and
costs ~1 hour; retrofitting after the wire-up would touch every P5 call site. Same discipline
as designing `SlabQuery.cTo` before P3 rather than after.

**P5 — Integrate.** Toggle `viewerBrickEnabled` in dev; verify SispLk and 35uedD render at L0 in
the visible region and coarsen away from the camera. Compare visible frame time against the
current whole-volume path on Dominik's RTX 2000 Ada; the audit's 5.3 ms MIP budget is the target
that must NOT regress.

Broken into four visible checkpoints so a milestone can be eyeballed before the next lands:

- **P5a — Plumbing.** `?bricks=1` routes to a `brickVolumeRenderer` that clears the canvas
  magenta. Proves the URL flag swaps constructors and the device + canvas survive a mode change.
  SHIPPED — confirmed 2026-08-27 ("yes magenta").
- **P5b — First render.** WGSL raycast with page-table indirection, atlas 3D texture allocated
  in `setImage`, real render pipeline. The fetch loop is P5c, so nothing populates the atlas by
  default — flip the existing Debug **Test pattern** toggle to fill brick (0,0,0) with a diagonal
  ramp and confirm the shader math (camera basis, brick indirection, atlas slot lookup, in-brick
  voxel offset, N-channel sample loop) all work. Same VIEW_HALF_ANGLE and same camera basis as
  `mipShader.ts` so the toggle between renderers doesn't jump the framing.
- **P5c — Fetch loop.** Wire `scheduleBricks` into the frame tick, drive `fetchBrick` per
  scheduled miss, `writeBrick` on arrival, update the page-table CPU buffer + mark dirty.
  Bricks stream in view-first, coarsen away from the camera under the SSE picker.
- **P5d — LUT + overlays + perf.** Add the LUT texture binding, per-channel contrast windows,
  label texture, overlay pipelines from `mipShader.ts`. Compare visible frame time against the
  flat renderer's 5.3 ms MIP budget on SispLk / 35uedD.

**P6 — Overlays.** Points, tracks, tails, masks continue to work — they're screen-space and don't
touch atlas geometry. Confirm no regression on `zolIMa` / `fXgbTl` reference movies.

**P7 — Retire the flat-atlas 3D path.** Delete `pickVolumeLevel` from the 3D render loop (keep it
for the 2D mini-map indicator until that surface has its own answer). Flip `viewerBrickEnabled` to
default on. Update `WEB_VIEWER_PLAN.md` → Decision 7 to point at this plan.

## What this plan is NOT

- **Not a Kiln clone or fork.** We take three *ideas* (physical atlas, page-table indirection,
  SSE scheduler) and implement them cecelia-shaped. No imported code, no upstream sync burden,
  no design lock-in to Kiln's format assumptions.
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
