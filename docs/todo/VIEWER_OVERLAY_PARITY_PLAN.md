# Viewer overlay parity — populations from every segmentation, movies that match

Status: P1–P4 BUILT 2026-10-07 in `viewer-pops-all-segs`, awaiting a visual pass in the viewer. Follows #1477 (movies draw the
viewer's tracks, not one segmentation's).

## Problem

The browser viewer shows ONE mask at a time, but nothing else is a property of "the shown
segmentation". #1477 freed the track kinds; the populations are still bound:

- **Viewer** (`ViewerWindow.vue` → `loadOverlays`): fetches one `(valueName, popType)` — the pop
  manager's `gatingCurrent` — and draws its pops only if that ONE pop type's chip is on. Ticking
  `clust` while the pop manager sits on `flow` draws nothing; another segmentation's gated pops never
  draw. The panel's own comment (`ViewerPanel.vue`, POP_TYPE_UI) already states the intent — "flow +
  clust + region coexist", every segmentation's pops shown at once — the napari bridge did that; the
  browser viewer regressed to one.
- **Movie** (`viewer_overlay_closure`): pops of one `(popValueName, popType)`.

Plus four colour mismatches between the viewer and its movie (#1477 reservations, and two found while
reading for this plan):

| | viewer (`buildMultiTrackBuffer`) | movie (`_build_overlay_state`) |
|---|---|---|
| colour by track | `palette[(id + (src+1)·1e7) % 12]` — source-index dependent | `palette[mod1(id, 12)]` — off by one, no source |
| speed | µm hop length incl. z, normalised over √ across ALL sources | squared xy pixel speed, normalised per source |
| tracker gaps | not drawn (`tb − ta ≠ 1`) | drawn, speed ÷ Δt |
| default source colour | `palette[src % 12]` (solid / pop) | grey |
| Tracks-legend overrides | `overrides[vn]`, `overrides[vn::path]`, `overrides[vn::trackclust::path]` | only per-vn, defaulting grey |
| mask + colour-by | fixed per-id palette, never recoloured | recoloured by colour-by whenever set |

## Decisions

1. **Populations are per (segmentation × visible cell pop type).** Every segmentation with a cell
   table — mask or not (`meta.cellTableNames`, new; the masks-only `labelNames` stays the mask
   picker's) — every `CELL_POP_TYPES` chip that is on (flow, clust, region). The pop manager's
   selection stops deciding what is DRAWN; it stays what the pop manager edits.
2. **Keys carry the segmentation.** Pop paths collide across segmentations (`/qc` on OTI and P14), so
   the viewer's transient `hiddenPops` keys `vn::popType::path`; track sources key `vn::path` (as
   today, unchanged). `hiddenTrackPops` is already persisted per `(image, vn)` — every segmentation now
   reads its own.
3. **Cell-track ribbons from every pop payload**, not the pop manager's; the trackclust stand-down
   stays per segmentation.
4. **One colour rule, written once per side.** Track colour = `palette[|track id| % len]` on BOTH
   sides (the viewer drops the source offset — a track's colour no longer changes with source order).
   Speed = µm hop length (x, y, z), consecutive frames only, heat over √ normalised across every
   source of the frame set. Default source colour = `palette[source index % len]` in the viewer's
   source order (per-segmentation, then cell-track, then trackclust).
5. **The movie takes the viewer's overrides map** (`trackSourceColours`, all three key shapes) instead
   of a per-vn colour baked into `trackSources`.
6. **Mask colour comes from `colourLabels`, not colour-by.** The batch `colourLabels` chip already means
   "colour the mask by the colour-by column" (preview: "colour-labels needs a mask picked"), but the
   backend never read it and recoloured on colour-by alone. A viewer look has no `colourLabels` → the
   per-id palette, as the viewer.

## Phases

- **P1 viewer.** `popPayloads: Map<vn::pt, payload>`; `buildPointBuffer` over several payloads
  (merge, re-sort by t); sidebar Populations grouped by segmentation (+ type when >1 on); summary /
  capture snapshot / gated ribbons iterate every payload; refetch on pop-type chip / label set change.
- **P2 look + backend pops.** `viewerLook`: `popTypes` (the chips on), `popAllSegmentations: true`,
  `hiddenTrackPops` per vn. Translator emits `popLayers` (explicit) or the all-segmentations form;
  `viewer_overlay_closure` expands it with the image. Batch keeps its single `(popValueName, popType,
  popsFilter)` — it is a picker of ONE set, and stays one.
- **P3 colour parity.** Track id rule both sides; speed metric + global range + gap rule (backend:
  build every source's state first, one range, then colour); default + override colours from the
  source index / overrides map.
- **P4 mask.** `maskColourBy` from `colourLabels`; resolvers pass colour-by to the mask only then.

## Not in scope

- The landscape tile summary still takes ONE pop layer (the pop manager's, when its type is on) —
  its backend computes one pops summary per tile.
- The colour-by legend shows the first layer's range; each segmentation's numeric range is its own.
- `hiddenTrackPops` keys by path within a segmentation, so the same path under two pop types of one
  segmentation shares a ribbon eye.

- Several masks at once (one label slot in the renderer — see the #1477 audit: ~12–15 files, cap 2–3).
- Fetch cost: one overlays request per (segmentation × pop type), each carrying the cell table. Fine at
  3 × 3; a combined route is the follow-up if it isn't.
