# Behaviour cards — shared spine for cellCards / motifCards / hmmCards

Status: **DONE (2026-09-22)** · Phase 1 shipped 2026-09-20 (#1076) · Phase 2 shipped 2026-09-21
(#1084) · Phase 3 + motif-cards fixes + Q1–Q4 closed shipped 2026-09-22 (#1173). All three
families live on the analysis board: `cellCards` (`CLUSTER_PANELS`), `motifCards` /
`hmmStateCards` (`INTERACTIVE_VIEWS`, `boardGroup: 'clustering'`, `rail: 'none'`).

## Goal

One shared "behaviour card" component with three concrete families:

- `cellCards` — one card per **trackclust pop** (already shipped headless, Phase 1;
  frontend Phase 2 pending) — the medoid TRACK of the cluster.
- `motifCards` — one card per **motif class** — the medoid INSTANCE (short sub-track window)
  of that class.
- `hmmCards` — one card per **HMM state** — the track SEGMENT most representative of that
  state (per Nature Comms 2025 Fig 4c). "Most representative" = the state-run whose owning
  track spends the largest fraction of its life in this state (see Decision 3).

The three today would be ~80% duplicate. Rule of three (`feedback_rule_of_three_no_deferring`):
extract the shared spine first, then each family is a thin instance.

## Origin

- `CELL_CARDS_PLAN.md` — Phase 1 shipped 2026-09-08 (`feat/cell-cards-phase1`), Phase 2 pending.
- `MOTIF_DISCOVERY_PLAN.md` — P2 slice 4 named a `motifCards` InteractiveView but deferred code
  until the extraction question was answered.
- 2026-09-19 chat: Dominik pushed back on shipping motifCards standalone — "would we then not
  also need an HMM card. and then it requires basically a common basic component for all three."
  He confirmed HMM cards is a real want (Fig 4c reference:
  https://www.nature.com/articles/s41467-025-57193-y#MOESM1).

## Invariants

- **Read-only.** Never mutates gates, populations, cluster/motif assignments, or the h5ad.
- **Reuse, don't fork.** The shared spine is a **template method + strategy** — every card family
  shares filmstrip rendering, stat footer, board-assets banking, and the Vue view; the family
  supplies the strategy (what defines a card, what's the medoid, what's the frame range, what
  overlay mode).
- **Board fit, not new canvas.** All three families are `InteractiveView`s on `LayoutCanvas`.
  Their `rail` differs — `cellCards` rides `clusterPops` (existing); `motifCards` and `hmmCards`
  ride whichever rail the population source uses (see Decision 6).

## The three families

|                | `cellCards`                                | `motifCards`                            | `hmmCards`                                                     |
|----------------|--------------------------------------------|-----------------------------------------|----------------------------------------------------------------|
| One card =     | one trackclust pop                         | one motif class (`motif.class` level)   | one HMM state (integer code)                                   |
| Card source    | `trackclust` pop path                      | `motif.class` categorical value         | `live.cell.hmm.state.movement` (or the specific hmm column)    |
| Medoid =       | track closest to cluster centroid          | motif INSTANCE closest to class centroid in DTW space | STATE-RUN belonging to the track that spends the highest fraction of its life in this state (Fig 4c pattern) |
| Frame range    | full track                                 | motif window `(t_start, t_end)`         | the state-run's `(t_start, t_end)` inside its parent track     |
| Overlay mode   | `track_color_mode="pop"`                   | `track_color_mode="motif_class"`  (new) | `track_color_mode="hmm_state"`  (new)                          |
| Stats footer   | per-pop median of 10 `live.track.*` measures | per-class median of the 3 motif features + `motif.distance` | per-state median of the 3 HMM features + `live.cell.speed`     |
| Suffix param   | clust suffix                               | motif suffix (`{props}.motiffeatures.json` key) | the HMM column name (there IS no per-run suffix on HMM)        |

## Locked decisions (draft — Dominik to react)

Numbered so code/other docs can cite them.

1. **Extract the shared spine BEFORE writing motif/hmm cards.** Refactor `CellCardsView.vue` +
   `cellCards.ts` first into `CardsPanelBase.vue` + `cardsPanel.ts`, wire `cellCards` as its
   first instance (byte-neutral refactor, proven by the existing cell-cards tests + a
   fixture-based rendering snapshot). Then `motifCards` and `hmmCards` land as thin family
   configs, not as new panels.
2. **Family = a config, not a subclass.** `CardsPanelBase` reads a `CardFamily` from a small
   registry (`frontend/src/components/plots/cardFamilies.ts`), keyed by view id. Config:
   ```ts
   interface CardFamily {
     endpoint: string                              // '/api/cell_cards' | '/api/motif_cards' | '/api/hmm_state_cards'
     overlayMode: 'pop' | 'motif_class' | 'hmm_state'
     requestBody: (props, popsPayload) => object   // family-specific request shape
     shownPops: (props) => PopPayload[]            // resolves rail selection → server pops
   }
   ```
   Adding a fourth family (region cards, spatial-neighbourhood cards, etc.) later = one entry.
3. **`hmmCards` medoid = the state-run whose owning track has the highest fraction of its cells
   in that state.** Ties broken by run length (prefer longer runs). Rationale: matches Fig 4c
   — the cards there each show a track SEGMENT that is dominantly one state, not a state's
   longest run globally (which could sit inside an atypical track). Alternative candidate
   ("longest state-run across the pool") deferred; if Dominik prefers it, one line changes.
4. **`motifCards` frame range = `(t_start, t_start + windowSize - 1)` of the medoid instance.**
   Motif window is a fixed 8 frames by default (`MotifDiscoveryParams.windowSize`). No context
   padding beyond the window unless a card-level `padFrames` param opens up later. Trace
   overlay covers the whole window; the trace's colour = the motif class colour from the
   palette.
5. **Shared server helper: `render_medoid_filmstrip(img, uid, vn, track_id, tspan, overlay_mode,
   viewState) -> Vector{Vector{UInt8}}`** in `api/src/behaviour_cards.jl` — a single call site
   for the pattern "resolve the (uid, value_name) image, compute the track bbox over the frame
   range, render N frames via `render_view_frame` with the right overlay closure". Both new
   endpoints call it verbatim; `cell_cards.jl` refactored to call it too (removes duplicated
   render loop).
6. **Rail:** unchanged for `cellCards` (`clusterPops`). `motifCards` and `hmmCards` ride
   `rail: 'none'` — motif classes / HMM state values are h5ad obs values, not populations; there
   is no rail to hang a picker off. The server enumerates classes / states from the h5ad and
   returns one card per class. Once `motifs` pop_type lands (`MOTIF_DISCOVERY_PLAN.md` P2
   slice 2), motif classes become populations on the shared `pops` rail like any other, and the
   panel can pick up a rail selection — but that's a migration, not a blocker for Phase 2.
   *Revised 2026-09-20: the initial draft said `livePops` — a rail that doesn't exist. The
   population rail is just `pops`; a separate one per pop_type isn't a Cecelia concept.*
7. **Three endpoints, not one:** `POST /api/cell_cards` (existing), `POST /api/motif_cards`,
   `POST /api/hmm_state_cards`. Rationale: the request shape differs (pool = value_name-set for
   cellCards; single (uid, vn) for motif/hmm cards initially), the medoid algorithm is different
   per family, and the endpoint name pays for itself in observability (task logs, board-assets
   naming, cache invalidation). The shared code is behind the endpoints, not at the URL layer.
8. **Board-assets naming carries family:** `settings/board-assets/{family}__{suffix_or_col}__{card_id}.png`.
   No change to the store; the family prefix keeps cache invalidation family-local (a new
   motif run doesn't invalidate cell-card PNGs and vice versa).
9. **HMM cards do NOT need a matching HMM plan entry.** The HMM state columns are already
   authoritative (`live.cell.hmm.state.movement` etc.); `hmmCards` is a NEW consumer, not a new
   producer. No `HMM_STATES_PLAN.md` needed.
10. **Motif cards CAN ship before the `motifs` pop_type lands.** The panel picker enumerates
    `motif.class` values discovered from the h5ad (see Decision 6). Once the pop_type ships,
    the picker switches to the pop-tree source with no shape change.

## What already exists — reuse these

- `render_view_frame` (`api/src/image_render.jl`) — one PNG per t, with crop + overlays.
- `overlay_author.jl` — `track_color_mode ∈ {track, speed, solid, pop}`. Adding `"motif_class"`
  and `"hmm_state"` is a strategy per mode (colour resolved from a per-cell obs column).
- `StripCell.vue`, `StatBox.vue`, `CellCardDetailPanel.vue`, `PlotSpinner.vue`, `imageGrid.ts`
  — all family-agnostic today. Not touched by the extraction.
- Board-assets sidecar (`settings/board-assets/`, `/api/board-assets/{id}`) — the existing store
  used by `ImageStripView` and `CellCardsView`. Every family lands PNGs here.
- `cellCards.ts` types → generalise to `cardsPanel.ts` (see Phase 1).
- `pop_df` — feature-table + track-id filter, used by every family's medoid algorithm.
- Existing tests: `cellCards.test.ts`, `interactiveViews.test.ts` — extraction must keep both
  green.

## What does NOT exist yet

- `overlay_author` mode `"motif_class"` — reads `motif.class` per t and paints the medoid track
  segment in that class's colour. Palette lookup: extend `frontend/src/plots/palettes.json` +
  the shared Julia palette source so a card matches the plot's colour.
- `overlay_author` mode `"hmm_state"` — same idea, reads `live.cell.hmm.state.<col>`.
- `medoid_motif_instance(features_df, motif_class_values, dtw_matrix)` — the motif discovery
  runner already computes medoids per class (Decision 5 in `MOTIF_DISCOVERY_PLAN.md`); this
  helper reads that from the `.motiffeatures.json` sidecar. **No new DTW computation.**
- `medoid_state_run(cell_state_col, track_ids, ts, state_value)` — for each track, count fraction
  of cells in `state_value`; pick the track with the highest fraction; return that track's
  contiguous state-run bounds for `state_value`.
- `api/src/behaviour_cards.jl` — the shared render helper (Decision 5).
- `motif_cards.jl` + `hmm_state_cards.jl` — thin endpoints on top of the helper.
- `CardsPanelBase.vue` + `cardsPanel.ts` — refactor of the existing cell-cards code.
- `cardFamilies.ts` — the config registry (Decision 2).

## Phases

Independently-shippable. Phase 1 is the extraction only (byte-neutral for cellCards). Phases 2
and 3 are the new instances.

### Phase 1 — extraction (byte-neutral for `cellCards`)

- Refactor `CellCardsView.vue` → `CardsPanelBase.vue` + a thin `CellCardsView.vue` that supplies
  the cell family config.
- Move `Card` / `CardsResponse` / `PoolMember` from `cellCards.ts` → `cardsPanel.ts`; add `family:
  "cell" | "motif" | "hmm_state"` to `Card` for family-local footer rendering.
- Backend: extract `render_medoid_filmstrip(...)` into `api/src/behaviour_cards.jl`; refactor
  `cell_cards.jl` to call it. Cell-cards endpoint payload unchanged.
- Tests: `cellCards.test.ts` unchanged, must pass; add `cardsPanel.test.ts` pinning
  `CardFamily` shape and the family registry (drift test).

**Checkpoint:** on `/analysis` with fXgbTl, the existing `cellCards` view still renders three
cards from `/Population 1..3` — byte-identical to today. Extraction was clean if this is true.

### Phase 2 — `motifCards`

- **Medoid resolution runs at query time from h5ad obs (`motif.class` / `motif.distance` /
  `motif.instance_id`)** — no `{props}.motiffeatures.json` sidecar dependency for medoids. The
  runner writes those obs columns today; medoid = the instance with the lowest mean
  `motif.distance`. Zero DTW re-computation; no re-run needed to see cards.
- **No new `overlay_author` mode.** Trace is coloured via `render_medoid_filmstrip`'s
  `trace_colour` arg — one `RGB{N0f8}` per card, resolved via `colour_by_palette(pop_map,
  "motif.class", …)` so the palette matches whatever the frequency plot uses.
- **`api/src/motif_cards_api.jl`** — `POST /api/motif_cards`. Body: `{ projectUid, rootUid,
  valueName?, maxPx?, padPx? }`. When `valueName` is omitted, server picks the first
  segmentation whose h5ad has `motif.class` (walks `versioned_keys(img.label_props)`). Response
  = `CardsResponse` (same shape as `/api/cell_cards`). Sidecar cache under
  `analysis/motif_cards/{value_name}.json`, keyed on cells-h5ad mtime + discovered class set.
- **Register `motifCards` in `interactiveViews.ts`** (not `CLUSTER_PANELS` — motif cards aren't
  cluster panels), `boardGroup: 'clustering'` so it lands in the same picker section as
  cellCards, `rail: 'none'` per Decision 6.
- **`MotifCardsView.vue`** = ~20-line wrapper around `CardsPanelInner` (a content-only spine
  extracted from `CardsPanelBase` so views that mount through `InteractivePanel` — which draws
  its own CanvasPanel — don't double-wrap).
- **`motifFamily` in `cardFamilies.ts`** — endpoint, request-body builder, empty-state copy,
  `requireShownPops: false`, `requireSuffix: false`, footer label transform.
- Tests: `test-api` end-to-end assert deferred (no committed fixture with motif columns yet);
  frontend types pin `motifFamily` shape via `cardsPanel.test.ts`.

**Checkpoint:** on `/analysis` with `4kS67f` / EaMaVq, dropping a Motif cards slot shows N cards
(N = classes in the run), each rendering the medoid instance's 8-frame filmstrip with the
motif-class colour trace.

### Phase 3 — `hmmCards`

- Add `overlay_author` mode `"hmm_state"`.
- `hmm_state_cards.jl` endpoint: implements `medoid_state_run` (Decision 3), calls the shared
  helper. Reads the picker's HMM column selection to know which state values to render cards for.
- Register `hmmCards` in `interactiveViews.ts` + `cardFamilies.ts`.
- Frontend: `HmmStateCardsView.vue` = wrapper.
- Tests: `test-api` on a tracked HMM-run fixture — one card per state value, medoid is the
  track/run predicted by the fraction-of-life rule.

**Checkpoint:** on `/analysis`, the tab shows K cards (K = HMM states), each cropped to the
representative state-run — visually mirrors Fig 4c of the Nature Comms 2025 paper.

## Open questions

1. **~~Rail for motif/hmm cards~~** — CLOSED. Decision 6 (revised 2026-09-20) shipped both
   families with `rail: 'none'`; `livePops` was speculative. When `motifs` pop_type lands, the
   panel can pick up a rail selection then; today the server discovers content from the h5ad.
2. **~~HMM cards' fraction denominator~~** — CLOSED (2026-09-22). Shipped with **cell count**:
   `fraction of cells in state / total cells in track`. Validated on 4kS67f/EaMaVq T:
   cell-fraction picks 88-97%-dwell tracks (clean exemplars of the state); run-count fraction
   picks tracks at ~50% mere-transition. All three states pick different medoids under the two
   denominators — cells is the right definition for "a card that best exemplifies this state".
3. **~~Motif cards' overlay when a cell belongs to overlapping instances~~** — CLOSED. The
   runner writes `motif.class` per-cell as "highest-confidence wins" (Decision 9); the card
   overlay reads that same column, so overlap resolution is by construction. Validated on
   4kS67f/EaMaVq post-runner-fix (2026-09-22).
4. **~~Layout preset naming~~** — CLOSED. Three presets, mirroring the three view registrations
   (`cellCards`, `motifCards`, `hmmStateCards`). A single "Cards" preset would need a
   family picker inside the panel, which is exactly what Decision 2 (family = a config) avoids.

## References

- `docs/todo/CELL_CARDS_PLAN.md` — the shipped Phase 1; every non-family decision there
  (invariants, board fit, sidecar location) applies verbatim to this plan.
- `docs/todo/MOTIF_DISCOVERY_PLAN.md` — Decision 5 (medoids on disk in the motif sidecar) is
  the input for Phase 2's medoid resolution.
- `docs/PLOTS.md` — plot registry + InteractiveView registration conventions.
- `frontend/CLAUDE.md` — `PRIMITIVES.md` / `COPY.md` mandatory lookups; a new card family that
  reinvents `StripCell` / `StatBox` / `CellCardDetailPanel` is a bug.
- Nature Comms 2025, Fig 4c: https://www.nature.com/articles/s41467-025-57193-y#MOESM1 — the
  visual pattern hmmCards mirrors.
- User memory: `feedback_rule_of_three_no_deferring`, `feedback_generalise_themes`,
  `feedback_analysis_board_two_registries`.
