# Cluster UMAP — colour-by & facet-by (populations / image attributes)

Status: in-progress — branch `feat/umap-colour-facet`. **Phase 1 + 2 built** (colour-by cluster /
population / attribute); Phase 3 (facet small-multiples) pending. Works for BOTH `clust` (static/cell)
and `trackclust` (track) embeddings — grain-matched via `pop_df`.

## Goal

Reproduce the two paper figures on the cluster UMAP:

1. **Colour by population** — show where the individually-tracked populations (e.g. gBT-I / gDT-II /
   DCs) fall on the behaviour-cluster embedding.
2. **Facet by image attribute** — split the *same* embedding into small multiples by an image-table
   attribute (e.g. `1 dpi` / `2 dpi`), coloured by population within each facet.

So two orthogonal axes of control on the existing UMAP panel: **colour by** {cluster | population |
attribute} × **facet by** {none | attribute | population}.

## What already exists (reuse — do NOT reimplement)

| Need | Existing seam |
|---|---|
| Attribute discovery | `GET /api/plots/attrs` → `{attrs:[{name,values}]}` (`api/src/plotting_api.jl` `api_plot_attrs`) — already powers summary "compare by attribute" |
| Attr picker + `loadAttrs` | `frontend/src/composables/useSummaryData.ts` (`compareAttr`/`loadAttrs`) — lift attr-discovery into a small shared composable so summary + UMAP share ONE impl |
| uid → attr value join | server `attr_map` rule ("join non-empty values with `.`", `api/src/plotting_api.jl`). UMAP fetches per-image already → build the same map client-side from the project store's `img.attr`; one shared `attrKey()` util so the two can't drift |
| Palette resolution | `paletteRange`/VisProps (`frontend/src/plots/plot.ts`) — already shared |
| Per-point pop membership | endpoint `_live_map(img, vn, pop_type)` + `cells_in_pop` (already used for the `pop` subset param) — extend from one pop to an ordered list |
| Scatter paint | UmapView's own `paintDots(ctx, mapPx, subset)` — it's a **2D canvas, not WebGL**, so small-multiples = call it per sub-rect. No GL-context-per-tile problem |

## Decisions (2026-07-13)

1. **"Population" = true membership, user-picked.** Colour/facet-by-population resolves real per-point
   membership against populations the user picks (any *granularity-compatible* pop_type), NOT just the
   source segmentation — because one segmentation can hold several cell types. The clustering **input
   pops are not recorded** (`clustfeatures.partOf` = member *images*, not pops), so picking is both
   necessary and more flexible (works for any pops, not only the clustering inputs).
2. **Default pop pick = each co-clustered segmentation's root pop** → reproduces figure 1 with one
   click; refine to finer pops for the segmentation→N-cell-types case.
3. **Granularity must match the UMAP grain.** `trackclust` UMAP (one point per track) → colour pops
   must be track-grained (`track`/`trackclust`); `clust` UMAP (per cell) → cell-grained
   (`flow`/`live`/`clust`). The picker restricts pop_type accordingly (labels align: track_id vs cell
   label).
4. **Facet = small multiples inside ONE panel.** Shared extents + shared colour map + a single legend;
   one PDF slot. (Not N separate panels.)
5. **Image-attribute axes are frontend-only.** The client fetches per-image so it already knows each
   point's uid; colour/facet-by-attribute is a client `uid → attr` join via the project store. No
   backend change for attributes.
6. **One new wire channel: `popIdx`.** `[x, y, code]` → `[x, y, code, popIdx]` (16 B/point, always
   emitted; `-1` when no colour pops requested). `code` (cluster) stays for colour-by-cluster; `popIdx`
   feeds colour/facet-by-population; uid (client) feeds colour/facet-by-attribute. First-match wins for
   overlapping pops (documented; input pops are normally disjoint).

## Wire / API changes

`GET /api/plots/umap` (in `api/src/gating_api.jl`, NOT Revise-tracked → **server restart**):
- New params: `colourPopType` (default = UMAP `popType`), `colourPops` (comma list of pop paths; bare
  paths resolve within each vn's own map; empty → `popIdx` all `-1`).
- Per vn: `m = _live_map(img, vn, colourPopType)`; for each requested pop `has_pop`/`cells_in_pop` →
  a `label → popIdx` map (index into the global `colourPops` order, so colours are consistent across
  vns). Assign per point by label; `-1` if in none.
- Body: `[x, y, code, popIdx]` Float32 LE. Response header `X-Colour-Pops` = URL-encoded ordered pop
  list (for legend labels + palette).

## Phases (each independently shippable)

- **Phase 1 — backend + wire.** Endpoint `popIdx` channel + `colourPopType`/`colourPops` + header.
  Frontend: parse 4 floats and record per-point image uid in the fetch loop (no UI change yet — popIdx
  unused, uid unused). Package/API test. Ship. (Server restart to load.)
- **Phase 2 — colour-by (figure 1).** UmapView `colourBy: cluster | population | attribute` selector
  (persisted state). `population` → pop picker → refetch with `colourPops`, colour by `popIdx`,
  legend from `X-Colour-Pops`. `attribute` → attr picker (shared composable) → client uid→attr colour.
  Reuse `paletteRange` + the existing legend/centroid render.
- **Phase 3 — facet-by (figure 2).** `facetBy: none | attribute | population` → partition points into
  facets, render a grid of sub-plots on the one 2D canvas (shared extents + colour map + single
  legend), titled per facet value. Export path already composites the canvas — extend to the grid.

## Invariants / notes
- `api/src/*.jl` not Revise-tracked → restart to test Phase 1.
- Keep colour-by-cluster the default (no regression to today's behaviour).
- Persist every new option in panel `state` (UI convention).
- One renderer: faceting is a *layout parameter* of UmapView's paint, not a second scatter component.
