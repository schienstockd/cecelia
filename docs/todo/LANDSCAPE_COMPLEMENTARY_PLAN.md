# Landscape complementary stats — plan

**Status:** building. Phase 1 (channels) shipped 2026-09-20 in #1089; Phase 2a
(`segCount`) building 2026-09-20. Drafted from
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
    hmmStates: [{ state, count }],       // only when the track's HMM run exists
    motifs: [{ motifId, count }],        // only when a motif discovery run exists
  },
  sourceRun: {                           // per FIELD (see Decision 4), not per tile
    pops: <analysisRunId>,
    tracks: <analysisRunId>,
    hmm: <analysisRunId>,
    motifs: <analysisRunId>,
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
   tracking run, `hmm` / `motifs` from their own runs — see the sibling
   `[[project_behaviour_cards_plan]]` for the HMM/motif surface). One
   `sourceRun` bag at the tile / landscape level with per-field keys is
   simpler than sprinkling `sourceRun` into each nested object, and reads
   cleanly against "which run produced this number."
5. **HMM / motif fields are conditional on the run existing**, not just on
   the tracks+props layer being on. A tile carries `tracks.hmmStates` only if
   an HMM run has been executed for this image / vn AND the layer is on. Same
   for `motifs`. Avoids `hmmStates: []` sprinkled across every tracked image
   that never went through HMM.
6. **Category stays even after the augmented fields land.** Occasional
   grounding value — a tile marked `dark` is a canvas-margin cue Claude reads
   without needing per-channel numbers. Cost is trivial; removing it after
   audits confirm it never earns its keep is easy.
7. **Envelope size cap: soft budget 500 KB per capture, hard cap none.**
   Baseline (`category` + `channels` × 3 visible channels) at 32×32 = ~150 KB.
   Full-featured (all layers on) at 32×32 = ~400 KB. If a specific project
   habitually blows past 500 KB, revisit the density cap for the augmented
   layer (frontend category can stay at whatever the user picked).
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

3. **Phase 2a — `segCount` (this pass, 2026-09-20).** Same compute endpoint
   grows an optional `labelsValueName` in the body — when set to the
   currently-shown labels vn (from the frontend `getLabelVisibility` /
   `labelName` computed), the response carries `tiles[i].segCount = <int>`:
   how many segmented objects have centroids inside that tile at the shown t.
   Uses `label_props(img; value_name=vn) |> view_centroid_cols |> as_df`,
   filtered by `centroid_t` when present; bins by level-0 pixel dimensions
   (`image_geometry.sizeX/Y`), consistent with Phase 1's whole-frame tiling.
   Sparse per Decision 3 (no `segCount` key when labels layer off). ~250 lines.

4. **Phase 2b — `pops` (next slice, unscheduled).** Per-tile population
   membership. Bigger than 2a because it needs the gating engine to resolve
   pop labels for the current run, plus a per-pop-per-tile aggregation. Frontend
   snapshot grows `visiblePops` (from `getPopVisible`). Emit
   `tiles[i].pops = [{popId, name, count}]` sparse per visible pop. Reuses the
   same centroid binning as 2a. ~400 lines.

5. **Phase 3 — `tracks` summary.** count / meanDuration / meanSpeed from
   the track-props view; `hmmStates` / `motifs` gated by run existence
   (Decision 5). Reuse the same tile-index / centroid-filter code from
   Phase 2a/b. ~350 lines.

6. **Phase 4 — `sourceRun` provenance.** Per-field bag; every phase that
   introduced a computed field also adds its `sourceRun` key. If Phase 1–3
   ship without it, add here in one sweep. ~150 lines.

7. **Phase 5 — legend + envelope-size ratchet test.** Assert that a
   `schemaVersion: 2` envelope with all layers on stays under Decision 7's
   500 KB soft budget on a representative frame. Fails when a future field
   is added without weighing it against the budget. Add landscape envelope
   size to the frontend cssScenarios / assertion suite. ~50 lines.

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
motifs are surfaced there. If cards ship a "which HMM run is authoritative"
resolver, Phase 3's `sourceRun.hmm` should read from it rather than a second
resolver.

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
