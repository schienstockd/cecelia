# Landscape complementary stats — plan

**Status:** feature-complete. Phases 1–6 shipped 2026-09-20/21
(`#1089`, `#1096`, `#1099`, `#1100`, `#1105`, `#1112`, `#1144`, Phase 6
Z-awareness on `feat/bidir-landscape-z-aware`). HMM/motifs
(former Decision 5) explicitly dropped 2026-09-21 — clustered tracks
are populations, so behaviour information rides the existing `pops`
bag from Phase 2b when a cluster-derived pop is ticked visible.
Drafted from
[`docs/archive/opus-audit-landscape-complementary-stats.md`](../archive/opus-audit-landscape-complementary-stats.md).
Written to be picked up cold by another session.

## Goal

The landscape overlay ([`BIDIR_CONTEXT_PLAN.md`](BIDIR_CONTEXT_PLAN.md) PR #6, shipped 2026-09-20
in #1083 / #1086) attaches per-tile category labels (`dark` / `bright-uniform` /
`bright-textured` / `edge` / `mixed`) to captures. A live session on `4kS67f`
(image `LUkCpP`, 2026-09-20) showed that the category dimension **mostly restates what
the RGB composite already shows**. Claude read the pixels and reached the correct
finding ("T-cell segmentation is under-called on this frame") without the landscape;
`bright-textured` labels on the interior didn't move the analysis.

The audit's decision: don't drop the landscape, **extend it** with per-tile fields
that are genuinely complementary to the pixels — not decorative encodings of them.

## Design principle (non-negotiable)

**A field belongs on a tile only if it is either (a) not visible in the composite
at all, or (b) currently on-screen but ambiguous by eye.** If a proposed field can
be answered by looking at the image, it doesn't belong on the tile. Grep this rule
before adding any new tile field.

Concrete examples:
- Per-channel `{mean, snr}` — a yellow tile could be high-green + high-red, medium
  + medium, or one channel burning under LUT boost. Composite can't tell. **In.**
- `segCount` per tile — eye can't reliably count 5 vs 12 objects in a downsampled
  frame. **In.**
- `pops` membership — invisible without a population overlay saying which is which.
  **In.**
- A per-tile "brightness" number that already agrees with luminance the eye reads.
  **Out.**

## Scope: mirror the RGB's own visibility rule

Only layers actually toggled on in the viewer at share time go into the tile — the
same rule that already governs which channels appear in the RGB composite.
Consequences:

- **No per-image capability-detection needed.** Read whatever layer-state stores
  drive the viewer's rendering, snapshot those. Existing hooks:
  `stores/settings.ts::getLabelVisibility`, `getTrackVisibility`, `getPopVisible`,
  `getColourBy` + per-channel visibility state in `stores/viewer.ts`.
- **Sparse by construction.** A tile has no `pops` key if the population layer
  isn't on, not `pops: []`. Envelope size tracks what's visible, not a fixed
  max schema.

## Proposed per-tile shape

```
tile = {
  category,                              // unchanged — keep the frontend-computed
                                         //   heatmap for scrub-time preview
  channels: {                            // only currently-visible channels
    <channelName>: { mean, snr },        // untangles colour-blend ambiguity
  },
  segCount,                              // only if seg layer on
  pops: [{ popId, name, count }],        // only if population/gating layer on
  tracks: {                              // only if tracks/props layer on
    count, meanDuration, meanSpeed,
  },
  sourceRun: {                           // per FIELD (see Decision 4), not per tile
    pops: <analysisRunId>,
    tracks: <analysisRunId>,
  },
}
```

Only fields that were reachable at snapshot time appear; `sourceRun` only carries
entries for the fields that are present.

## Locked decisions

Numbered so code and other docs can cite them (`Decision 5`).

1. **Compute moves to the backend for augmented fields.** The current
   frontend-only `utils/landscape.ts` reads the already-decoded RGB composite —
   sufficient for `category`, insufficient for per-channel / seg / pops / tracks.
   New backend endpoint `POST /api/viewer/landscape/compute` receives a
   visibility-snapshot from the frontend, does the reads (zarr for per-channel;
   `label_props` for seg / pops / tracks; gating engine for pop membership),
   returns the augmented `{grid, tiles, legend}`.
2. **Frontend keeps the cheap category layer** for scrub-time preview + immediate
   toggle feedback. When the user shares (capture-in), the frontend calls
   `/api/viewer/landscape/compute` and snapshots the AUGMENTED landscape into the
   capture envelope. Two cadences: (a) frontend category on every scrub /
   density change (cheap, ≤ 100 ms per Decision 16 of the parent plan);
   (b) backend augmented on Share (budget: 500 ms – 2 s at native res, one-shot).
3. **Sparsity by visible layers is authoritative.** The visibility snapshot the
   frontend sends is the ground truth for what fields the response may carry.
   Fields absent from the snapshot MUST be absent from the tile — no
   "server knows this is available, add it anyway."
4. **`sourceRun` is per FIELD, not per tile.** Different fields come from
   different runs (`pops` from the current gating run, `tracks` from the
   tracking run). One `sourceRun` bag at the tile / landscape level with
   per-field keys is simpler than sprinkling `sourceRun` into each nested
   object, and reads cleanly against "which run produced this number."
5. **No dedicated HMM / motif tile fields (dropped 2026-09-21).**
   Originally Phase 6 was to add `tracks.hmmStates` and `tracks.motifs`
   conditional on the run existing. Superseded: a track cluster IS a
   population in Cecelia (per `[[project_gating_popmanager]]` and
   `[[project_clustering_design]]` — `clust.hmm.*` etc.), so ticking a
   cluster-derived pop as visible already carries behaviour information
   through the existing `pops` field from Phase 2b. Preserves Decision 3
   (sparsity by visibility) without needing a "which HMM run is
   authoritative" resolver that doesn't exist yet. If a downstream reader
   ever needs "was this pop derived from clustering," it can grep the pop
   path (`clust.*`) or the pop type — no new envelope schema.
6. **Category stays even after the augmented fields land.** Occasional
   grounding value — a tile marked `dark` is a canvas-margin cue Claude reads
   without needing per-channel numbers. Cost is trivial; removing it after
   audits confirm it never earns its keep is easy.
7. **Envelope size cap: ratchet at the measured ceiling; original 500 KB
   estimate was low.** Measured 2026-09-20 via Phase 5 ratchet test:
   a 32×32 v2 envelope with every tile carrying 4 channels + segCount +
   4 pops + tracks + sourceRun serialises to ~640 KB — pops are the driver
   (~250 KB alone). Bare v1 category is ~100 KB. Ratchet test in
   `frontend/src/utils/landscape.test.ts` asserts < 700 KB, leaving room
   for one small future field before it fires; if it does, either shrink
   the schema (shorter pop paths / names) or move the density cap for the
   augmented layer down from 32. Original 500 KB soft budget kept as an
   aspirational target for typical (not worst-case) captures where pops
   don't occupy every tile.
8. **Legend + tile schema versioning.** The augmented landscape gains a
   `schemaVersion: 2` field on the envelope (v1 = category-only, current
   ship). Consumers (Kiwi, MCP `get_capture`, any future reader) branch on
   the version. Never mutate v1 tiles in-place on read.

## Explicitly out of scope (this pass)

- Anything not currently toggled visible in the viewer, even if computed and
  available for the image. Consequence of the visibility rule; recorded here
  so a future reader doesn't add "load all pops the image has" without
  revisiting Decision 3.
- Full track paths — presence + summary stats only (`tracks.count`,
  `meanDuration`, `meanSpeed`), not the geometry. Track geometry belongs on
  a separate "trajectory anchor" surface if that need ever surfaces; a
  landscape tile shouldn't inline hundreds of coordinates.
- Convergence with the module-page multi-panel capture sidecar shape (see
  *Cross-piece linkage* below). Related but orthogonal per the user's
  direction 2026-09-20 — track them independently for now, revisit if a
  third structured-sidecar surface appears.

## Validation before / alongside building

**Phase 0 (before Phase 2):** The disagreement test on 3–5 current
category-only captures. Find a tile that reads `mixed` or `edge` in a spot
that looks unambiguous by eye, and check whether Claude's own visual read
of the same frame agrees. Agreement everywhere = category is decorative in
the well-rendered-composite case, and the real leverage is in
channels/segCount/pops as this plan proposes — confirmation to have before
committing the backend-compute lift. Not a blocker; can run in parallel.

Dominik's eyeball time, not code time.

## Phased build sequence

Each phase independently mergeable.

1. **Phase 0 — disagreement test.** No code. Eyeball audit on 3–5 shared
   captures from real projects (`fXgbTl`, `LUkCpP` on `4kS67f`, one from
   `zolIMa`). Record findings in this file's status section. Confirms /
   disconfirms Decision 6 (keep category). ~1 hour.

2. **Phase 1 — per-channel stats (`channels`). SHIPPED 2026-09-20 in #1089.**
   `POST /api/viewer/landscape/compute` handler that reads the shown t / z at a
   pyramid level ≥ 512 px on the long side; per visible channel emits mean + SNR
   per tile (SNR = mean / max(σ, floor)); snapshots into the capture envelope
   alongside `category` and bumps `schemaVersion` to 2. MCP `get_capture`
   docstring names the new field.

3. **Phase 2a — `segCount`. SHIPPED 2026-09-20 in #1096.** The compute endpoint
   grew an optional `labelsValueName` in the body — when set to the currently-shown
   labels vn (from the frontend `getLabelVisibility` / `labelName` computed), each
   response tile carries `segCount = <int>`: how many segmented objects have
   centroids inside that tile at the shown t. Bins by level-0 pixel dimensions
   (`image_geometry.sizeX/Y`), consistent with Phase 1's whole-frame tiling.

4. **Phase 2b — `pops`. SHIPPED 2026-09-20 in #1099.** The compute endpoint grew
   optional `popValueName` + `popType` — when both non-empty (frontend reads them
   from `cc.gatingCurrent`, gated by `popsPanelOn` = `getPopVisible(setUid, popType)`),
   the backend resolves visible pops via `resolve_pops(img, popType; value_name=vn)`
   (same authoritative resolver `overlay_author` uses), builds a `label → tile` map
   from label_props centroids once, then counts each pop's labels per tile. Each
   tile with any member pops gets `pops = [{path, name, count}]` for the pops with
   count > 0 in THAT tile (sparse per Decision 3).

5. **Phase 3 — `tracks` summary. SHIPPED 2026-09-20 in #1100.** Compute endpoint
   grew optional `tracksValueName` — backend reads label_props with
   `[track_id, live.cell.speed]` + centroids, one pass for whole-lifetime
   `num_cells` per track, second pass bins cells at t and emits
   `tracks = {count, meanDuration, meanSpeed}` per tile (sparse — empty tiles
   have no `tracks` key). `meanSpeed` is INSTANTANEOUS per-cell speed averaged
   in the tile at t (absent when segmentation has no speed obs); `meanDuration`
   is per-track full-lifetime frame count. Behaviour information not
   given a dedicated field — clustered-track pops (`clust.hmm.*` etc.)
   already ride via the `pops` bag when ticked visible (Decision 5,
   dropped 2026-09-21).

6. **Phase 4 — `sourceRun` provenance (this pass, 2026-09-20).** Response body
   grows optional top-level `sourceRun` — sparse per-field bag naming what
   produced each augmented field. Per-field values:
     `segCount = {valueName, labelsVersion}` — vn + resolved vN
     `pops     = {valueName, popType, gatingMtime}` — same fingerprint
                 `_pop_df_mtime` uses (on-disk mtime stringified, "∅" when
                 the gating file is absent)
     `tracks   = {valueName, labelsVersion}`
     `channels = {valueName, imageVersion, level}` — pyramid level actually read
   Frontend `augmentLandscape` grew a third optional `sourceRun` arg and
   attaches it to the merged `LandscapeResult` (never emits `sourceRun: {}` —
   same sparsity rule as tile fields). MCP `get_capture` docstring names the
   bag as the "which run produced this number" answer. ~180 lines.

7. **Phase 5 — envelope-size ratchet test (this pass, 2026-09-20).**
   Synthetic 32×32 max-density v2 envelope (every tile: 4 channels +
   segCount + 4 pops + tracks + sourceRun) serialised via `JSON.stringify` +
   `TextEncoder`; assert byte length < 700 KB (measured ~640 KB today; +60 KB
   slack). Fails when a future field lands and pushes past the ceiling.
   Amends Decision 7 with measured numbers — original 500 KB estimate was
   optimistic; pops (~250 KB at max density) dominate the payload.
   ~60 lines in `frontend/src/utils/landscape.test.ts`.

8. **Phase 6 — Z-awareness: honour the viewer's plane/volume mode
   (this pass, 2026-09-21).** Pre-Phase-6, centroid-based fields collapsed
   across ALL Z (a tile at z=5 slice-view reported cells from every z), and
   channels always read a single plane even when the viewer was showing a
   MIP. Both silent mismatches. Fix mirrors the gating page's
   `pick-rect` z-scope pattern: the frontend snapshots `mode.value`
   (`'plane' | 'volume'`) + inclusive `zLo`/`zHi` on the compute POST —
   plane sends `zPlane ± 1` (matches the pick-rect ±1 default), volume
   sends the slab-slider `zRange`. Backend:
     • channels — plane mode reads the single plane at `z` (unchanged);
       volume mode reads slab `[zLo, zHi]` + per-pixel MIP across Z.
     • segCount / pops / tracks — extend the label_props read to include
       `centroid_z` (already provided by `view_centroid_cols`), filter
       rows by `zLo ≤ round(centroid_z) ≤ zHi` in both modes. 2D images
       have no `centroid_z` column so the filter is a no-op — zero
       behaviour change for still images. Track `duration_by_track`
       (per-track lifetime frame count) intentionally does NOT apply
       the Z filter — "how long has this cell been alive" is a whole-
       track property, not a per-slice one.
   Response body grows optional `viewport: {renderMode, zLo, zHi}`
   (top-level, sibling to `sourceRun`) so a reader can tell whether a
   `segCount: 8` came from a plane-mode slab-of-3 vs a volume-mode MIP.
   MCP `get_capture` docstring updated. Tests: new
   `_bin_centroids_to_tiles Z filter` testset covering no-op / plane /
   volume / no-overlap / NaN-drop / length-mismatch. Ratchet still under
   700 KB (viewport is 3 scalars). ~200 lines total.

## Cross-piece linkage

**Module-page multi-panel captures** (branch `docs/multipanel-capture-plan` in
sibling worktree `cecelia-multipanel-plan`, plan doc not yet committed):
related but orthogonal (Dominik, 2026-09-20). Both work is under the general
umbrella of "structured sidecar attached to a PNG capture," but the two
schemas serve different queries — landscape carries spatial tile aggregations,
module-page carries per-panel provenance. Track independently. If a third
structured-sidecar surface appears, revisit whether a shared convention pays
back the abstraction cost (rule-of-three, [`docs/MAINTAINABILITY.md`](../MAINTAINABILITY.md)).

**Behaviour cards** (`project_behaviour_cards_plan` memory): HMM states +
motifs surface on cards. Not mirrored onto landscape tiles — clustered-track
pops from cards' upstream clustering are populations, so they already ride
the `pops` field when ticked visible (Decision 5, 2026-09-21).

**BIDIR PR #7 (Blackboard)**: shipped. If a Blackboard entry attaches a
capture (`attach_capture_ids`) and the capture carries a `schemaVersion: 2`
landscape, the entry reader must not crash on the new fields — verified
by `get_capture` passing envelopes through verbatim (already the pattern).

## References

- Source audit (do not act on directly):
  [`docs/archive/opus-audit-landscape-complementary-stats.md`](../archive/opus-audit-landscape-complementary-stats.md)
- Parent plan: [`BIDIR_CONTEXT_PLAN.md`](BIDIR_CONTEXT_PLAN.md) — Decision 14 (landscape),
  Decision 15 (correlation eval), Decision 16 (compute budget).
- Shipped ancestor PRs: #1083 (landscape overlay), #1086 (snapshot into capture),
  #1087 (density 32 + polish).
- Frontend visibility hooks: `frontend/src/stores/settings.ts`
  (`getLabelVisibility`, `getTrackVisibility`, `getPopVisible`, `getColourBy`).
- Backend readers: `python/cecelia/utils/zarr_utils.py::open_as_zarr` (mandatory
  entry per CLAUDE.md); `app/src/label_props.jl`.
- Sibling multipanel work: branch `docs/multipanel-capture-plan` in worktree
  `cecelia-multipanel-plan` — related but orthogonal.
