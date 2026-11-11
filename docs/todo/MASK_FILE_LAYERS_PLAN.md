# Mask file layers — a segmentation's halo / nuc files drawn with its mask

**Status:** building (2026-11-11), branch `feat/viewer-multi-labels`. The general case — several
*segmentations* at once — is parked in [`MULTI_MASK_PLAN.md`](MULTI_MASK_PLAN.md).

**Ask.** ylwqS4 has `cells → [cells.zarr, cells_halo.zarr]`; the viewer showed only `cells.zarr`
(`img_labels_path` answers `first(filenames)`). The old R viewer drew every file of a segmentation
as its own layer (`inst/py/napari_utils.py`, halo/nuc at full opacity).

## Decisions

**D1 — Derived files ride the base mask's slot, as flag bits.** A segmentation's other files carry
the SAME cell ids as its base (`_halo`: grown from the base, `docs/SEGMENTATION.md` → *Halo*; `_nuc` /
any `matchAs` type: re-mapped to base ids by IoU). So one u32 per voxel can say both "which cell" and
"which file region": no second texture, no budget change, no renderer change. Packing *different
segmentations* this way was rejected in PX (it loses ids); same-id files lose nothing.

**D2 — Encoding (server-side, `compose_label_files!`).** For each requested file `f` and voxel with
base id `b`:
- `f ≠ 0`, `b = 0` → `f | OUT` — the file extends past the cell (a halo ring).
- `f ≠ 0`, `b ≠ 0` → `b | IN` — the file overlaps the cell (a nucleus; a whole-cell halo's interior).
- a HALO packs only its `OUT` part (`inside = false`): it is grown from the cell, so inside the cell
  it repeats it — and a whole-cell halo's `IN` would merge a nucleus outline into one region.
- otherwise unchanged. `OUT = 2³¹`, `IN = 2³⁰` (`LABEL_FILE_OUT_BIT` / `LABEL_FILE_IN_BIT` in
  `shaders/constants.json`); ids are `& LABEL_ID_MASK` (2³⁰−1). No request names a file → the bytes
  are exactly today's.

**D3 — Drawing (one shared include, `label_layers.wgsl`).**
- The *cell* = `raw & LABEL_ID_MASK` → colour, population table, pick bitset. All three strip the
  flags themselves (`labColour`, `labShown`, `labInPick`, `labIsFocus`), so a flagged voxel colours
  as its cell everywhere.
- The *base* drawing is today's: fill or contour (`p.lab.y`), only on non-`OUT` voxels.
- The *file* region (any flag) is always drawn as an **outline**, `max(contourPx, 1)` wide, in the
  cell's colour at the mask's opacity. Halo → a ring outline around the cell; nuc → the nucleus
  outline inside it. Filling it would hide the base mask it annotates.
- `labEdge` stays the one contour loop, instantiated twice (`LAB_EDGE=` name + `LAB_AT=` fetch).
- Pick / focus outline follows the base cell.
- **Plane views only.** File outlines are a per-plane annotation: they draw where a pass takes one
  sample per ray — the 2D plane view (flat, bricks, tiles). A raycast through several planes (3D, or
  a 2D z-window projection) draws the cells exactly as without the files (`labPlaneRaw`): a 3D halo's
  cap is a dome, and outlining it stacked one rim per plane over the cell (result review,
  `test_result_review_halo_3d_rings.py`).

**D4 — Which files: per (image, segmentation), all shown by default** (the old-R default). Persisted
in `cc.viewerLabelFiles` as the HIDDEN suffixes, so a file a later run adds appears on. Toggled from
chips under the segmentation's row in the viewer panel.

**D5 — Durable stores only.** A task preview / live run serves its base store as today (no `files`).
The task preview never writes a halo anyway.

**D6 — Identity.** The shown file list is part of the mask source id (`tileMaskSourceId` `files`),
so toggling a file is a mask change — refetches the mask alone, never the channels
(`docs/ARCHITECTURE.md` → *Mask vs channels*).

**D7 — Wire.** `/api/viewer/slab?labels=<vn>&files=halo,nuc`: suffix = the file's stem after the base
file's stem + `_` (`cells_halo.zarr` → `halo`). Unknown / unregistered / missing / differently-shaped
files are skipped, never a 500. Meta gains `labelFiles: {vn: [suffix…]}` (registered + on disk).

## Phases (one PR)

1. Server: suffix resolution, `compose_label_files!`, `files=` on the slab route, `labelFiles` in meta
   + api tests.
2. Shaders: constants, `label_layers.wgsl`, flag stripping, the four label passes; shader tests +
   Python twin.
3. Frontend: `SlabQuery.labelFiles`, settings bag + cross-window channel, `maskFiles` into the source
   id and the three fetch paths; panel chips.
4. Docs: `SEGMENTATION.md` → *Viewer integration* (still described napari), API route, ARCHITECTURE.

## Not covered

- Movies draw the base mask only (the offline renderer reads `img_labels_path`).
- Pick routes read the base store — a click on a halo ring outside the cell picks nothing.
- Label correction rewrites the base only, so after one the siblings are stale — `docs/TODO.md` →
  *Label correction leaves a segmentation's halo / nuc stores on the old ids*.
- Visual check is his: ylwqS4 in 2D (flat + tiles) and a 3D image with a halo.
