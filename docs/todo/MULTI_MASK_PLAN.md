# Multi-mask viewer — several segmentations (and their halo/nuc files) at once

**Status:** PARKED (2026-11-11) — revisit when two *segmentations* must show together (e.g. specks over
cells). The halo/nuc case shipped separately as [`MASK_FILE_LAYERS_PLAN.md`](MASK_FILE_LAYERS_PLAN.md). Nothing built. Supersedes `VIEWER_CONTROLS_SPLIT_PLAN.md` § PX (2026-08-25), whose
direction it keeps (per-cell ids + per-cell contours per mask, no bitmask, a user-owned cap) and whose
mechanism it replaces (N texture bindings — see D2).

**Trigger (2026-11-11).** ylwqS4 (Speck counting) has `cells → [cells.zarr, cells_halo.zarr]` and
`specks`. The viewer shows neither the halo nor cells + specks together. The old R/napari viewer added
one layer per file of every segmentation (`inst/py/napari_utils.py:~609`: `({vn}) {suffix} Labels`,
halo/nuc opacity 1.0, base 0.7, cyto hidden).

## Why it is one mask today

Not a data limit — a renderer one. Every renderer holds exactly one mask texture and one mask
identity, and everything above inherits that:

| layer | single-mask point |
|---|---|
| flat (`volumeRenderer`) | slot `{labelTexture, labelSrc}`, binding 3; `setLabelSource/needsLabel/uploadLabel` |
| tiles (`tileRenderer`) | one `labAtlas` (layer = slot), `labelSrcBySlot`, instance flag `labOn` |
| bricks (`brickVolumeRenderer`) | `labelTextures[variantN]`, one `labelSrc`/`labelGen`, `labelOf[slot]` |
| shaders | one `lab` uniform vec4 (opacity, contourPx, paletteRows, pointBorderPx — all lanes used), one `pal`, one pick buffer; march keeps ONE front-most `labId` |
| ViewerWindow | `maskName = previewVn ‖ liveVn ‖ labelName` (L944), `maskSourceId`, `maskTable`, `brickSource().labelName` |
| panel/settings | `getLabelVisibility` enforces radio on read; `toggleLabel` unticks the others |
| server | `label_store_path` → `img_labels_path` → `first(filenames)`: the halo file is unreachable |

## Decisions

**D1 — The unit is a mask *layer* = (segmentation, file).** `cells`, `cells · halo`, `specks` are three
layers. A halo/nuc file shares ids with its base, so pop colours, pick highlights and cell tables apply
to it unchanged. The panel shows file layers as sub-rows of their segmentation.

**D2 — Stack layers along an axis of the ONE existing mask texture; no new bindings.** This is how
channels are already packed (bricks: Z per slot, `bz*nC`; flat: `c * zpc`).
- bricks: label atlas Z = `bz * M * sz`, sample at `z + m*bz`.
- flat: slot mask texture depth `dp * M`.
- tiles: atlas layer `slot*M + m`.

PX's "one binding per mask" no longer fits since the multi-atlas brick renderer (2026-09-17): sampled
textures = `N(1+M) + 1 + M` → at N=4 atlases, M=3 masks needs 19 > the default 16. Stacking keeps it at
today's `2N+2` for any M. Bound instead by `maxTextureDimension3D` on the stacked axis — validated with
the same `validateAtlasLayout`.

**D3 — Masks are budgeted, everywhere.** Today brick and tile mask bytes sit OUTSIDE the cache budget
(`pickAtlasLayout`, `computeCapacity` size intensity only; a mask that does not fit falls into the OOM
step-down). With M masks that is up to `4M/(nC·bpv)`× the intensity cache unbudgeted (1-ch u8, 3 masks =
12×). Bytes per slot become `vox * (nC*bpv + 4*M)` in all three renderers.

**D4 — Reserve room for the layers the image HAS (up to the cap), not the ones ticked.** Keeps the
ARCHITECTURE invariant "a mask toggle never drops the channels / never changes how many timepoints
fit" (`maskRoom` generalised to a count). Adding a segmentation (new task) reallocates; ticking does
not. Cost: an image with 3 mask layers holds less intensity even with one ticked. **[Q1]**

**D5 — Cap = `viewerMaxMasks` setting, default 4, min 1.** Effective cap shown beside it =
min(setting, what `maxTextureDimension3D` allows on the stacked axis). Over-cap ticks get the PX
warning row ("6 ticked, showing 4"), not silent truncation.

**D6 — Compositing: each layer independent, panel order = draw order (last on top).**
- 2D: per layer, its id at the pixel → its own `labEdge` (fill/contour) → its own colour → mix with its
  own opacity, in order.
- 3D (MIP/brick march): carry M front-most ids (`array<u32, MAX_M>`), each found as today; composite in
  order after the march. Cost is linear in M per step — bench M=1,2,4 on the brick path.
- Uniforms: `lab` stays for the global lanes (pointBorderPx); new `labLayer: array<vec4<f32>, MAX_M>`
  = (opacity, contourPx, tableRow/paletteRows, on). `uniforms.json` count field + `golden.json`.
- `MAX_M` is a compile-time constant (8); the effective M ≤ it. M=1 must stay pixel-identical (golden).

**D7 — Colours.** One colour texture, rows stacked per layer (the table is already 2D, W=4096 wide); each
layer gets a row offset. Defaults **[Q2]**: base = per-cell palette filled at 0.7 (today), halo/nuc
= same per-cell colours as contour at 1.0 (the old-R look), a second segmentation = per-cell palette
contour. Pop colouring (`maskPopTable`) builds one table per segmentation from ITS OWN pops (already the
rule — `popLayers.filter(l => l.vn === vn)`); a file layer reuses its base's table.

**D8 — Picking.** Pick routes are server-side and need one segmentation. Rule: the topmost ticked
*segmentation* that has a cell table (file layers resolve to their base). `pickHighlight.valueName`,
today ignored, routes the outline to that segmentation's layers (pick buffer header carries a layer
mask in `reserved0`). Cockpit / Claude marks / ROI highlights keep working because they already name
the vn.

**D9 — Preview / live.** Today a preview or live run REPLACES the shown mask. New: it replaces its own
segmentation's layer if ticked, else adds a layer on top; the others stay. `previewLabels`/`liveLabels`
stay single objects (one task at a time) — only the merge into the layer list changes.

**D10 — Server: one mask per request, plus a file selector.** `/api/viewer/slab?labels=vn&file=halo`
(absent = base, today's behaviour). Meta gains `labelFiles: {cells: ["", "halo"]}` (suffixes) and
`labelDims`/`storeRevs` per file. No combined endpoint (PX step 5 dropped): bricks/tiles already
fetch per request under `MAX_LABEL_INFLIGHT`; flat fetches M in parallel.

**D11 — Consumers.** viewer look → `labelValueNames` list (already list-shaped downstream); capture
legend lists every layer; `viewerCacheClearMatches` matches any shown layer; landscape keeps the
topmost segmentation. **Movies [Q3]:** the backend renderer draws `first(labelValueNames)` (movie_rail
L583/L827; several = a compare grid). After this the viewer can show what a recording cannot.

## Phases (each lands green on its own; M=1 behaviour unchanged until P5)

| | what | files (approx) | his eyes |
|---|---|---|---|
| P0 | Budget masks in bricks + tiles (D3, M=1). Fixes today's unbudgeted path on its own. | brickAtlas.ts, tileRenderer.ts, brickVolumeRenderer.ts, tests | 3D image: cache chip numbers |
| P1 | Server: `file=` on slab + pick, `labelFiles` meta | viewer_api.jl, image.jl helper, api tests | — |
| P2 | Shaders + uniforms: `labLayer[MAX_M]`, per-layer ids/edges/colours; golden + Python twin (`wgsl_utils.py`, movie host reads mip.wgsl) | mip/tile/brick*.wgsl, label_colour/edge, uniforms.json, golden.json, shaderSource.ts, wgsl_utils.py + tests | — (golden M=1) |
| P3 | Renderers stack M layers (D2), per-layer source ids, alloc/validate, OOM | volumeRenderer, tileRenderer, brickVolumeRenderer, brickLabels, brickLoader, uploadRace/brick tests | 2D + tiles + 3D, forced M=2 |
| P4 | ViewerWindow: layer list replaces `maskName`; per-segmentation tables; pick routing (D8); preview/live merge (D9) | ViewerWindow.vue, viewerLabels.ts, maskPopTable.ts, tileViewer.ts, volumeViewer.ts | — |
| P5 | Panel: checkboxes, file sub-rows, cap setting + warning; settings read no longer forces radio | ViewerPanel.vue, settings.ts, useOverlayAutoShow.ts, Preferences | ylwqS4: cells+halo+specks |
| P6 | Consumers (D11): look, capture legend, cache-clear, landscape | viewerLook.ts, overlayLayers.ts, viewerCacheClearChannel.ts | — |
| P7 | *[Q3]* Movies: real multi-mask overlay in the offline renderer | movie_rail.jl, image_render / Python host | a recording |

PRs: P0 alone (it is a fix); P1–P3 together (no visible change, M=1 golden); P4–P6 together (the feature).

## Risks

- **3D march cost** with M ids per step — bench before P5 ships; if M=4 is too slow, 3D caps lower.
- **Stacked-axis limit**: brick label Z = `bz·M·sz` ≤ 2048 on baseline devices → at bz=128, M·sz ≤ 16;
  the layout picker trades slots for M. Validate, don't assume.
- **VRAM on 1-channel images**: masks dominate (4 B/voxel vs 1–2). u16 ids when max id < 65536 would
  halve it — out of scope, noted.
- **Untestable by me**: every renderer change needs his eyes (P3, P5).

## Open questions

Raised 2026-11-11, not decided. On Q1 the alternative that came up was a separate mask pool (masks
never compete with channels; extra masks fill in slower) — plus u16 ids to halve mask VRAM.


- **Q1** — D4: reserve room for every mask layer the image has (toggles free, less intensity cache), or
  only for the ticked ones (more cache, a tick reallocates and refetches channels)?
- **Q2** — D7 defaults: is "halo = same colours, contour, opaque" right? Should a second segmentation
  default to contour so it does not hide the first?
- **Q3** — Movies in scope (P7), or ship the viewer first and leave recordings drawing one mask?
