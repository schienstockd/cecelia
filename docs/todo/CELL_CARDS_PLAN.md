# Cell Cards — snapshot cards for behaviour clusters

Status: **planning** (branch `feat/cell-cards`). Parked plan; durable parts get promoted into
[`docs/UI.md`](../UI.md) → *Interactive plots* and [`docs/PLOTS.md`](../PLOTS.md) as we build.

## Goal

For each behaviour cluster (`trackclust` pop), render a **card**: a cropped image thumbnail of the
medoid cell over its tracked frames, its track path overlaid, and a stat footer with the cluster's
median motility measures. Cards render as a **grid inside one board slot** for side-by-side
comparison; clicking a card opens a per-card detail floater. The metaphor is a trading card
(image on top, numbers underneath), not a Pokemon stat sheet.

## Origin

Original brief in [`docs/archive/cell-cards-prompt.md`](../archive/cell-cards-prompt.md). It
predates the shipped LAYOUT-model canvas and rail contract; the "Card Table = new canvas + new
panel type" framing is dropped — cards fit as a registry-declared InteractiveView.

## Invariants

- **Read-only.** A card is a view of an existing `trackclust` pop over an existing clustering run.
  Never mutates gates, populations, cluster assignments, or the h5ad.
- **Reuse, don't fork.** Grid layout = `plots/imageGrid.ts` (the same helper `FlowMetricsView` /
  `FlowProbabilityView` use). Track path rendering = the primitive powering `TrackPathsView`.
  Image bytes = a NEW endpoint but built on `zarr_utils` (never a bare `zarr.open`). Cluster
  membership = `pop_df` (never a direct h5ad read).
- **Board fit, not new canvas.** Cards live on `LayoutCanvas` (`/analysis`), one grid slot per
  registered `cellCards` view; the "Card Table" is a preset LAYOUT tab (e.g. 1-up with cellCards),
  not a route.

## Locked decisions

0. **A clustering run is ALWAYS a pool; a pool of 1 is still a pool.** The card pipeline treats
   every run as pooled over `(uid, value_name)` pairs from day one — never over a single image. The
   pool has two axes recorded on the run's `{props}.clustfeatures.json` sidecar (`CLUSTER_POOLING_PLAN`):
   `partOf: [uIDs]` (multi-image) and the co-clustered value_names (multi-segmentation, via
   `co_clustered_value_names`). The medoid is picked over the full pool; a card's image comes from
   whichever `(uid, value_name)` its medoid lives in. Confirmed 2026-09-08 — "cluster runs are
   always pooled; ship pooled; a pool of 1 is still a pool." Removes the previous "cross-image
   deferred" reservation.
1. **`cellCards` is an InteractiveView, not a new panel type.** New registry entry in
   `frontend/src/components/canvas/interactiveViews.ts`. `rail: 'clusterPops'` — the
   `PopulationManager` picks which trackclust pops render (the same rail UMAP + cluster panels use).
   `analysisBoard: true`; `popTypes: ['trackclust']`. No changes to `CanvasManagerChrome` or the
   rail contract.
2. **One card = one trackclust pop.** Not one card per raw cluster id. Cards inherit `name`,
   `path`, `colour` from the pop (per fXgbTl: `/Population 1..3`, filter `clusters.movement in
   [0|1|2]`). Renaming a pop renames its card. HMM state is an *input feature* to clustering, not a
   card dimension — confirmed 2026-09-08.
3. **Medoid = nearest to centroid in the RUN's pooled feature space.** Not motility summaries. For
   a `trackclust` pop, the feature matrix = the same columns clustering pooled (e.g. `movement`
   run: the 10 `live.track.*` + 3 `hmm.state.movement.*` + 9 `hmm.transitions.movement.*_*` = 22
   dims), pooled across every `(uid, value_name)` in the run — see Decision 0. Cluster codes come
   from `clusters.{suffix}` per-`(uid, value_name)`; per-pop candidate rows come from `pop_df`; the
   medoid is the row whose feature vector has the smallest euclidean distance to the pool's
   feature-mean. The medoid resolves to a `(uid, value_name, track_id)` triple — the card's image
   comes from that image, its trace from that vn's `pop_df`. Ties broken by cluster-membership
   length (prefer longer tracks). Small pools (per-cluster n<10) still get a card; medoid
   definition unchanged, and the card carries an `n=` note.
4. **Card content = image + trace + stats footer.**
   - **Image**: for the medoid track's frame range, a small horizontal filmstrip of 3–5 frames
     (first / one at max instantaneous speed / last); each frame is a **crop around the track's
     bbox produced by the offline renderer's own primitive** — `render_view_frame` in
     `api/src/image_render.jl` (the same primitive `record_view_movie` calls per frame), with the
     track-bbox as `crop` and a `max_px` downscale to card size. Channels + LUT come from the
     viewer's current view state (the same snapshot `ImageStripView` captures), so cards match
     what the user is looking at.
   - **Trace**: rendered as an `overlay_author` per-t closure fed into `render_view_frame`
     (`track_color_mode = "pop"` picks up the trackclust pop's colour). NOT drawn client-side —
     the same overlay pass the movie renderer uses, so the trace on a card matches the trace on
     a recorded movie by construction.
   - **Scale bar + elapsed timestamp** on the last cell — via `StillOverlay.vue` + the shared
     `niceScaleBar` / `elapsedLabel` in `utils/stillOverlay.ts` (the ONE policy across viewer
     overlay, captured-still strip, animation timeline, offline encoder).
   - **Stats footer**: per-pop median (q25–q75 in a smaller line) for the ten canonical
     `live.track.*` measures (speed, duration, trackLength, displacement, straightness,
     displacementRatio, outreachRatio, meanTurningAngle, overallAngle, asphericity). Same shape
     `get_measure_summary` already returns — no new aggregation logic.
5. **Per-card detail = a `FloatingPanel`, not a new canvas.** Click a card → opens a bigger
   floating panel using the shipped `FloatingPanel` + `panelBounds`/`clampPanel` primitives. Same
   card component, larger; adds full-length filmstrip (every frame) and full stat table. One
   floater open at a time (storage key `cellCards-detail`). No per-card drag on the board grid —
   comparison stays in the grid; individual arrangement happens on demand in the detail floater.
6. **"Card Table" = a preset layout tab, not a route.** Add a template to the analysis-canvas
   template library (a 1-up slot filled with `cellCards`, or 2-up next to a UMAP). No canvas key,
   no `/cards` route.
7. **Backend: one route, frames as board-assets, pool-shaped input.** `POST /api/cell_cards` with
   `{root_uid, value_name, cluster_col, pops: [{path, cluster_ids}], viewState}` → `{pool: [{uid,
   value_name}, ...], cards: [{path, name, colour, n, medoid: {uid, value_name, track_id, frames:
   [t0, t1]}, filmstrip: [{t, assetId}], stats: [{name, median, q25, q75}]}]}`. `root_uid` is the
   image the user opened the analysis board from (used to resolve the run's sidecar and expand
   the pool); the backend derives the full pool from `partOf` + `co_clustered_value_names` — the
   client never enumerates the pool. Per-card `viewState` is applied per-image (channels resolved
   by name/index against each image's own channel list). Frames are rendered via
   `render_view_frame` (with track-bbox `crop` + `overlay_author` closures for the medoid),
   PNG-encoded, and stored as **board-assets** under `settings/board-assets/`, served via
   `/api/board-assets/{assetId}` — the exact sidecar store `ImageStripView` uses. The trace is
   baked into the frame by the overlay pass; no separate trace payload.
8. **Caching: sidecar under each pool member.** `analysis/cell_cards/{value_name}__{suffix}.json`
   written under **each `(uid, value_name)` in the pool** (payload minus the assets: medoid +
   frame range + stats + assetIds). Same content on every pool member — this keeps the "sidecar
   lives next to the run's other outputs" rule (`{props}.clustfeatures.json` is already mirrored
   the same way per `CLUSTER_POOLING_PLAN`), and a card view opened on any pool member reads its
   local copy. Rebuilds when the run's `clusters.{suffix}` mtime is newer on any pool member or
   the pop set changes. Assets are content-hashed under `settings/board-assets/` (existing store
   handles this). Invalidation piggybacks on `correction_staleness` (a trackclust-invalidating
   change already flags derived artefacts — see `correction_staleness.jl:60-61`).

## What already exists — reuse these, don't reimplement

**Frame rendering (backend)**
- `render_view_frame` in `api/src/image_render.jl` — one frame from an OME-Zarr with `crop`,
  `max_px`, channel `specs`, and `overlays`. The offline renderer's per-frame primitive.
- `record_view_movie` in `api/src/movie_render.jl` — timelapse sweep over `render_view_frame`
  (context, not directly reused: cards need per-frame PNGs, not an mp4). Same crop/max_px/overlay
  contract, so a card frame is byte-identical to a frame from a movie of the same view.
- `overlay_author.jl` — resolves populations/tracks into per-t overlay closures for
  `render_view_frame`. `track_color_mode ∈ {track, speed, solid, pop}` — `"pop"` picks up a
  pop's colour, so the medoid trace uses the trackclust pop's colour automatically. **Shared
  palette source** with the frontend (`frontend/src/plots/palettes.json`).
- `python/cecelia/utils/movie_io.py` — `movie_writer` / `coerce_movie_size` / `crop_to_even`. Not
  needed for cards (no video), but the same policy holds for image sizing.
- Board-asset sidecar (`settings/board-assets/`, `/api/board-assets`) — the existing store
  `ImageStripView` uses for its captured PNGs. Cards land here too.

**Overlay chrome**
- `StillOverlay.vue` + `utils/stillOverlay.ts` (`niceScaleBar`, `elapsedLabel`) — the ONE scale
  bar + timestamp helper used by the still strip, the animation timeline, and the WebGPU volume
  viewer. Cards use it verbatim on the last cell (per Decision 4).

**Grid + export**
- `plots/imageGrid.ts` — `gridLayout`, `imageGridSvgFrom`, `imageGridPng`, `gridColumns`. The
  card grid uses this — same helpers as `FlowMetricsView` / `FlowProbabilityView`, same export
  contract, same test pinning (`imageGrid.test.ts` requires every base64-tile view to declare
  `exportFormats`).
- `plots/pdf.ts` `layoutPages` + `plots/boardSvg.ts` — multipage board export; a
  `CellCardsView.exportSvg()` built from `imageGridSvgFrom` slots straight in.

**Strip cell (frontend) — extract as the new common helper**
- `ImageStripView.vue` today owns cell rendering (image + optional overlay legend + scale bar
  chrome + `StillOverlay` band + zoom-to-source). That cell primitive is NOT extracted — its
  markup + persistence live inline in the view. **Extract `components/plots/StripCell.vue`**
  (image + `ViewLegend` + `StillOverlay` overlay row + click-to-open target) as the shared
  primitive; `ImageStripView` and `CellCardsView` both consume it. This is the "make a common
  helper" step that the reuse rule requires.

**Data**
- Rail contract (`components/canvas/canvasManager.ts`) — `clusterPops` → `PopulationManager`.
- Registry (`components/canvas/interactiveViews.ts`) — pinned by `interactiveViews.test.ts`.
- `pop_df` / `pop_df_multi` in `app/src/label_props.jl` — trackclust filter → track IDs.
- `zarr_utils.open_as_zarr` / `read_scale` / `store_compressor(kind)` — reads are covered.

## What does NOT exist yet

- **Pool-aware feature-matrix reader for a clustering run.** The columns used per run are known
  (`featuresByRun[suffix]` — observer surfaces them, but the app doesn't have a first-class Julia
  accessor for "the feature matrix suffix was clustered on"). Helper next to
  `app/src/tasks/clustPops/cluster.jl`: `clustering_features_pooled(root_uid, value_name, suffix)
  -> (df, pool)` where `pool :: Vector{@NamedTuple{uid::String, value_name::String}}` — walks
  `partOf` (from `{props}.clustfeatures.json`) × `co_clustered_value_names` (`population_manager.jl`),
  concatenates each pool member's feature columns from its own label-props table with an added
  `_uid` / `_value_name` tag column, so `medoid_track` returns a triple.
- **`medoid_track(features_df, cluster_ids) -> (uid, value_name, track_id)`** — pooled feature
  centroid, argmin euclidean; ties by track length. Reads the `_uid` / `_value_name` tag columns
  the pooled reader adds.
- **`track_bbox(img, value_name, track_id; pad_px)`** — min/max centroid over the track's frames,
  pixel coords, reusing `pop_df`. Called with the medoid's `(uid, value_name)` — the pooled
  pipeline resolves the CciaImage from `uid`.
- **`CellCardsView.vue`** + **`StripCell.vue`** (the extracted common cell primitive).
- **`POST /api/cell_cards`** orchestration — calls `render_view_frame` per selected frame with
  the medoid bbox + `overlay_author` closures scoped to the medoid track only, PNG-encodes,
  banks as board-assets. Sidecar under `analysis/cell_cards/`.

## Phases

### Phase 0 — contract + fixture status
- Declare the payload type in `frontend/src/components/plots/cellCards.ts` (new) — `Card`,
  `CardsResponse`, `PoolMember`, `CardsRequest`. TS-only for now; the Julia mirror lands in
  Phase 1 when a writer actually exists (a types-only .jl with no constructor is dead code per
  CLAUDE.md).
- Type-drift test in `frontend/src/components/plots/cellCards.test.ts` — pins `medoid` as a
  `(uid, value_name, track_id)` triple and `pool` as `PoolMember[]`, so any collapse back to
  single-image shape fails typecheck.
- Fixture: `test-data/projects/testpr/1/KDIeEm` has no clustering column yet. **Phase 1 adds a
  synthetic `clusters.movement` + `{props}.clustfeatures.json` sidecar to it** — small (few dozen
  ints + a JSON), fits the 1 MB/file cap. fXgbTl stays on Dominik's machine as the manual browser
  target (real intravital data, pool of 1); testpr becomes the headless `test-api` fixture.

**Checkpoint:** TS contract compiles; drift test asserts pool-shaped medoid; Phase 1 knows what
fixture surgery it owns.

### Phase 1 — Julia data pipeline (headless)
- `clustering_features(img, value_name, suffix)` reader (off the existing cluster-run sidecar).
- `medoid_track(features_df, cluster_ids)` — pooled feature centroid, argmin euclidean; ties by
  track length.
- `track_bbox(img, value_name, track_id; pad_px)` — min/max centroid over the track's frames,
  pixel coords. Reuses `pop_df` for centroids.
- **New wrapper** `render_card_frames(img, medoid, ts, viewState) -> Vector{Vector{UInt8}}` in
  `api/src/cell_cards.jl` — thin composition: `track_bbox` → `render_view_frame(zp, t; crop,
  max_px, specs = viewState.specs, overlays_for = overlay_author.build_overlays_for(...,
  track_ids = [medoid.track_id], track_color_mode = "pop"))` per t. NO new frame renderer, NO
  new overlay author, NO new Python. All existing primitives.
- `POST /api/cell_cards` orchestrating: for each pop → resolve medoid → pick 3 frame indices
  (Decision 4) → `render_card_frames` → save PNGs as board-assets → assemble payload.
- Sidecar write/read + staleness join (piggyback on `correction_staleness`).

**Checkpoint:** `test-api` asserts one card per pop on fXgbTl, medoid track id stable across runs,
stat medians match `get_measure_summary` for the pop, PNG assets exist under `board-assets/`.

### Phase 2 — Vue view + shared cell + registry entry
- **Extract `components/plots/StripCell.vue`** from `ImageStripView.vue` — image + `ViewLegend`
  + `StillOverlay` (scale bar / timestamp) + click-to-zoom-to-source. Refactor `ImageStripView`
  to consume it (proves the extraction is byte-neutral before adding the second caller).
- `frontend/src/components/plots/CellCardsView.vue` — grid via `imageGrid.ts`; each cell wraps
  `StripCell` + a stat footer built from the pop's medians.
- Register in `interactiveViews.ts`: `id: 'cellCards'`, `rail: 'clusterPops'`,
  `analysisBoard: true`, `popTypes: ['trackclust']`, `exportFormats: ['png','svg']`.
- Detail floater `CellCardDetailPanel.vue` — `FloatingPanel` + `panelBounds` + reuse `StripCell`
  for the full-length filmstrip.
- Tests: `interactiveViews.test.ts` passes (rail is one the board renders);
  `imageGrid.test.ts` detector still passes (tiles export at native size); a new
  `StripCell.test.ts` pins the extraction (ImageStripView and CellCardsView render identical
  markup for the same cell payload).

**Checkpoint:** on `/analysis` with fXgbTl, the tab shows three cards from `/Population 1..3`
in their pop colours, medoid trace baked in by the offline overlay pass. Clicking a card opens
the detail floater. `ImageStripView` still renders identically (extraction was clean).

### Phase 3 — layout preset + polish
- Add "Card Table" as a template in the layout template library (`ANALYSIS_CANVAS_PLAN` Decision
  8): 1-up filled with `cellCards`, and 2-up `cellCards + umap` for the natural comparison.
- Multipage PDF export verified — one page per tab, cards render as SVG when the tab exports
  vector.
- Add row to `docs/inventory/FRONTEND.md` (new shared `TrackTraceCell`).
- Promote durable parts to `docs/PLOTS.md` (the view + registry entry) and `docs/UI.md` (the
  Card Table template).

**Checkpoint:** browser-verified by Dominik (per the "real-data visual validation" rule — cards
are a rendering surface, so no test proves biological correctness).

## Open questions

- **Channel + LUT provenance.** Same viewer view-state snapshot `ImageStripView` captures
  (`POST /api/viewer/thumbnail` today). Cards send that same `viewState` in the request. The
  detail floater has a "re-render with current viewer state" button.
- **Filmstrip frame selection.** 3–5 frames is guess-work. Fixed indices (first/mid/last) vs.
  motion-derived (first/max-displacement/last)? Provisional: first, one at max instantaneous
  speed, last; the detail floater shows the full sequence.
- **Zoom-to-source parity.** `ImageStripView` cells persist `snapshot`+`imageUid` so a click
  reopens the exact view months later. Cards inherit this via `StripCell` — the medoid track's
  bbox + view state is enough for zoom-to-source to land on the medoid cell at the right
  camera. Free feature; verify it works on fXgbTl.
- **Card sizing.** `imageGrid` uses uniform cells; a wide cluster range (n=125 vs n=14 on fXgbTl)
  means the same card real estate for very different sample sizes. The `n=` note is enough;
  don't scale card size by n.
- ~~**Cross-image cards.**~~ Resolved 2026-09-08 by Decision 0 — pool-first from day one. One
  global medoid per cluster (over `partOf` × co-clustered value_names). Alternative "one medoid
  per image" is out of scope; each card is a single cell.

## References

- [`docs/archive/cell-cards-prompt.md`](../archive/cell-cards-prompt.md) — original brief
- [`docs/todo/ANALYSIS_CANVAS_PLAN.md`](ANALYSIS_CANVAS_PLAN.md) — the LAYOUT-model board this
  view plugs into
- [`docs/todo/CANVAS_MANAGER_RAIL_PLAN.md`](CANVAS_MANAGER_RAIL_PLAN.md) — the rail contract the
  entry declares
- [`docs/todo/CLUSTERING_PLAN.md`](CLUSTERING_PLAN.md) / [`CLUSTER_POOLING_PLAN.md`](CLUSTER_POOLING_PLAN.md) — clustering machinery cards read from
- [`docs/todo/CROP_PANEL_PLAN.md`](CROP_PANEL_PLAN.md) — user-driven 3D crop; NOT the card thumb path
- [`docs/todo/ANIMATION_PLAN.md`](ANIMATION_PLAN.md) — the still-strip + board-assets + view-state
  provenance patterns cards inherit; DIFFERENT surface (user-authored figures), same primitives
- `docs/POPULATION.md` → *trackclust*, `docs/DATAMODEL.md` → *clusters.{suffix}*
- `frontend/src/components/canvas/interactiveViews.ts`, `frontend/src/plots/imageGrid.ts`
- fXgbTl (`zolIMa`) — three-pop test case, 22-dim `movement` feature space
