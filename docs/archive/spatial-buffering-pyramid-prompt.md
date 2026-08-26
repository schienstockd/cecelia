> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.
>
> **Outcome (2026-08-25).** Phase 1 measured on `f8gzA2` (Human Lymph Node IBEX, L0 20329×16898, 25 c,
> 6 levels, 1024² chunks, 2× per step) and `FtGoJO` — well-behaved bf2raw pyramids, no untested paths.
> Phase 2 server surface **SHIPPED**: `open_level(zarr_path, level_idx)` alongside `open_level0`,
> `read_slab` grew optional `x`/`y` ranges + `level`, `/api/viewer/slab` grew matching
> `x`/`xTo`/`y`/`yTo`/`level` query params, `/api/viewer/meta` returns per-level shape + chunk shape so
> the client picks LOD. Phase 2.5 (Z) resolved: 3D volume view defaults to the DEEPEST pyramid level
> (napari-parity, avoids WebGPU `maxBufferSize`); user-overridable via `settings.viewerVolumeLevel`.
> **Phase 2 client-side tile UI (per-viewport requests for 2D pan/zoom) and Phase 4 spatial cache/
> prefetch not yet built** — the server serves the pieces they need.

# Spatial buffering + pyramid scales for large XY tilescans

## Status

Exploratory / audit-first, same posture as the temporal buffering work in
`WEB_VIEWER_PLAN.md` P2. Do not implement before the audit's numbers are in
— this doc mirrors that structure: measure the real data first, then design
the cache/prefetch strategy against real numbers, not assumptions.

## Why this is a different problem than the timecourse

Intravital movies max out around 1024×1024 XY — small enough to ship as one
slab per timepoint, no spatial tiling needed (see `WEB_VIEWER_PLAN.md`).
Large tilescans (Phenocycler and similar whole-slide/large-tile data) are
the opposite shape: enormous in XY, typically no meaningful time axis. This
is a pan/zoom-over-a-huge-flat-plane problem, not a raycast-a-volume or
scrub-through-time problem — closer to a slippy map than to the timecourse
viewer. Same underlying zarr/pyramid/slab-serving infrastructure, different
access pattern, different cache eviction logic (spatial locality vs
temporal locality).

## Phase 1: Audit real XY chunking, on real large tilescan data

We don't currently know how zarr's XY chunking behaves on the actual large
datasets — find out empirically rather than assuming a chunk shape.

1. Pick 2-3 real large tilescan stores (get paths from Dominik — he has
   larger tilescans to work off already). For each, read `.zarray`/
   `.zattrs` and record: full XY dimensions, chunk shape (all axes, not
   just XY — note if chunks span full Z/C or are also split there),
   `dimension_separator` (flat vs nested — already known to vary across
   stores per the temporal work's finding), compressor/codec, and total
   chunk count in XY at native resolution.
2. Confirm whether these stores already have a multiscale pyramid written
   (check `.zattrs` `multiscales` metadata for >1 dataset/level) or are
   single-level like the intravital stores the earlier audit measured. If
   a pyramid exists, record how many levels and the downsample factor
   between them.
3. If no pyramid exists on these specific stores yet, check whether the
   existing pyramid-writing code (used elsewhere in the pipeline — confirm
   where) can simply be run against these stores, or whether large-XY
   inputs hit some untested path (memory, chunking choice, timing) in that
   writer. Measure pyramid-build time and output size for at least one
   real tilescan.
4. Output: a short table per store — dimensions, chunk shape, existing
   pyramid (y/n, levels), and anything that looked like it would break
   naive tile-serving assumptions (e.g. chunks not aligned to a clean tile
   grid, chunks spanning multiple channels making per-channel serving
   awkward).

## Phase 2: Define the tile-serving unit

Mirror the temporal work's core decision (slab-assembly beats per-chunk
HTTP) but for the spatial axis:

- Per-chunk HTTP was measured dead for the timecourse (1116 requests,
  5270ms). The XY case likely has the same failure mode at large tilescan
  scale — check the actual chunk count across a viewport-sized region at a
  given zoom level and estimate whether naive per-chunk fetching would hit
  the same wall.
- Define what "one tile request" means here: probably (region in x, region
  in y, channel, pyramid level) → assembled server-side into one
  contiguous 2D (or thin 2D+few-channel) block, same
  assemble-once-then-transfer principle as the temporal slab. Confirm
  whether the natural zarr chunk size is a reasonable tile unit directly,
  or whether tiles need to span multiple chunks (more assembly work,
  fewer requests) or split single chunks (less assembly, more requests) to
  land on a good request-size/request-count tradeoff — use the actual
  chunk shapes from Phase 1 to decide, don't guess a tile size in the
  abstract.
- Reuse the existing slab-assembly server code path/pattern from the
  temporal work rather than writing a second one — note where it needs to
  generalize (2D region instead of full-volume-at-timepoint) versus where
  it's already general enough as-is.

## Phase 2.5: Z-axis — open question, explore rather than assume

Large tilescans may or may not carry a meaningful Z axis, and it's not yet
decided how Z should be handled in this view mode. Explore rather than
picking one option upfront:

- **First, find out what these stores actually look like in Z.** Some
  large-XY acquisitions are single-plane (Z=1), some carry a handful of
  Z-planes for focus stacking, some may be closer to the intravital
  shape (real Z depth) but just also large in XY. Pull this from the
  Phase 1 metadata pass — don't assume large-XY implies flat.
- **Option A — one plane at a time**, same posture as the temporal
  timecourse work: user is looking at a single Z (or a fixed
  projection, e.g. a pre-computed MIP stored as its own pyramid level)
  and switching planes is a discrete, on-demand load — same shape as
  "3D per-timepoint, not scrubbing" scope cut already made for
  intravital 3D. Cheapest to build, and tile cache keys stay
  (x, y, channel, level) with z fixed or absent from the key entirely.
- **Option B — treat Z like the live/intravital images do**, i.e. Z
  becomes another axis in the volumetric raycaster rather than the tile
  viewer — meaning a large-XY, shallow-Z tilescan would actually want to
  route into the *volume* renderer's data path for regions of interest,
  not the flat tile-pan viewer at all. Worth checking whether this is
  actually two different view modes for two different data shapes
  (flat huge-XY tile browsing vs. bounded-region volumetric look) rather
  than one viewer trying to do both.
- **Option C — precompute a Z-projection (MIP or similar) as an extra
  pyramid level/channel**, so the default flat-browsing experience never
  touches raw Z at all, and true per-plane or volumetric Z access is a
  separate, explicit user action (e.g. "inspect this region in 3D") that
  drops into the existing volumetric raycaster on a cropped subregion
  rather than needing its own Z-tiling logic.
- Whichever direction this points, record the reasoning and the
  triggering data characteristic (e.g. "these stores are Z=1, so this is
  moot" or "these stores have real Z depth, so Option C avoids building a
  third rendering path"). This decision affects whether Phase 4's cache
  key needs a Z dimension at all — resolve it before finalizing that
  design, not after.

## Phase 3: Pyramid level (LOD) selection

- Standard slippy-map logic: at a given zoom/scale factor, pick the
  pyramid level whose native resolution is closest to (without going
  far below) the current screen pixel density, so zoomed-out views fetch
  cheap low-res levels and only full zoom pulls full-res tiles.
- Define the mapping from "current viewport zoom" to "pyramid level index"
  concretely against the actual downsample factors found in Phase 1 —
  don't assume a standard 2x-per-level pyramid without checking.
- Note what happens at the seams: switching pyramid level mid-pan/zoom
  should not cause a visible flash-to-blank — likely needs the previous
  level's tiles to stay rendered until the new level's tiles arrive
  (progressive refinement), same spirit as how the temporal work avoided
  flicker via holding contrast constant across frames.

## Phase 4: Spatial cache + prefetch, mirrored against the temporal design

Reuse the *shape* of the temporal LRU/prefetch/cancellation design (P2 in
`WEB_VIEWER_PLAN.md`), adapted to 2D locality instead of 1D:

- **Cache key**: (tile x, tile y, channel, pyramid level) instead of
  (timepoint, channel) — extend with a Z component only if Phase 2.5
  concludes the tile viewer needs to carry Z at all; otherwise Z is
  either fixed/absent here or handled entirely by a separate volumetric
  path per that phase's conclusion.
- **Eviction**: LRU under a byte budget, same as temporal — but eviction
  should account for 2D adjacency (evict tiles far from the current
  viewport first) rather than a simple recency-only order, since panning
  can move in any direction, not just forward/back along one axis like
  time scrubbing.
- **Prefetch**: directional prefetch in the temporal case predicted
  forward/backward along time. Here, prefetch should predict along the
  actual pan vector (if the user is panning right, prefetch tiles to the
  right) plus a fixed-radius halo around the current viewport for
  zoom/direction changes. Keep this simple first — a halo-only prefetch
  (no velocity prediction) may be good enough; note as a stretch goal if
  velocity-based prediction turns out to matter after testing the simple
  version.
- **Cancellation**: same requirement as temporal — a fast pan/zoom that
  invalidates in-flight tile requests needs those requests cancelled
  before they land, not just ignored on arrival, to avoid wasting
  bandwidth on tiles no longer in view.
- **VRAM budget interaction**: note how the spatial tile cache and the
  (potentially still-resident) temporal/volume caches from other view
  modes share the same GPU memory budget if a user can be in a
  Phenocycler-style tile view and a volumetric view in the same session —
  or confirm they're mutually exclusive view modes with independent
  budgets, which would simplify this significantly.

## Deliverable

1. Phase 1 table: real chunk shapes/pyramid status for actual large
   tilescan stores, including Z depth per store (feeds Phase 2.5).
2. Phase 2.5 conclusion: how Z should be handled for this data — one
   plane/projection at a time, routed to the existing volumetric
   renderer, or precomputed projection — with the reasoning and data
   evidence behind whichever direction it points.
3. Tile unit definition (Phase 2) with a stated request-size/request-count
   tradeoff, grounded in the real chunk shapes, not assumed.
4. LOD-selection mapping (Phase 3) grounded in the real pyramid downsample
   factors found, plus the seam-handling approach.
5. Cache/prefetch design (Phase 4), explicitly noting what's reused
   as-is from the temporal work vs. what needed real adaptation for 2D
   locality.
6. Fails-if for each phase, same discipline as `WEB_VIEWER_PLAN.md` — name
   what would make this approach not work before building it out.

Check in after Phase 1 — the real chunk/pyramid numbers may reshape Phase 2
and 3 more than expected, same as how the temporal work's own numbers
overturned an earlier assumption (pre-chunking-first advice, refuted once
server-side assembly was measured).
