> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. No
> `docs/todo/*_PLAN.md` exists for it yet.
>
> **Outcome:** not run yet (2026-11-25). Read against origin/main 4452532, several premises have moved:
>
> - **Per-track aggregates are not a new primitive.** `track_props` (`app/src/tracking/track_props.jl`)
>   already computes `.mean/.median/.sum/.qUp/.qLow/.sd` of any cell column per track on read (never
>   stored, so task 3's store/invalidation question mostly falls away). A categorical column gets the
>   within-track fraction per category, which is `fraction_in` once membership is a column. Cluster tracks
>   already feeds those aggregates. Missing: population membership as a column, and min/max/first/last.
> - **Tracks can be gated on morphology/channels in the backend.** `/api/gating/channels` returns
>   `cellMeasures` + `trackAggregates` and `pop_df` resolves `area.median` (`api/src/gating_api.jl:583`,
>   `pop_df.jl:277`). Only the frontend axis picker (`GatePlotPanel.vue`, `measureGroups.ts`) hides
>   them; `gating.ts` stores `trackAggregates` and nothing reads it.
> - **The `tracked` filter population already exists** as the derived `_tracked`
>   (`pop_df.jl:571`, `track_id gt 0`, parent ∩ tracked, evaluated per query, offered under every
>   tracked (sub)population by `tracked_pop_parents`; `docs/POPULATION.md` §derived pops). But it
>   only reaches TRACK pickers, so the addendum's ask stands at cell grain: derived pops are
>   registered for `live` only (`derived_pop_paths`), a cell plot asks for `flow`
>   (`plot_pop_types`, `picker.jl:41`), and a `live` family is treated as a track family that folds
>   `P/_tracked` into `P`; module pickers class `_tracked` as track-grained (`is_track=true`), so
>   `_accept_permits` drops it from cell scopes. `docs/POPULATION.md` says "cell plots keep the
>   `_tracked` leaf"; the code does not offer it there (maintainer confirmed in the GUI). Hence the
>   hand-made filter workaround. The fix is to surface `_tracked` at cell grain, not new machinery.
> - **Flicker has a second cause the brief misses:** a track belongs to a population if *any* of its
>   cells do (`pop_df.jl:125`), so a flickering track counts in every sub-population it touches.
> - Partly existing: per-segment colouring is the Tracks `cell` colour mode (by an obs column, not by
>   population); a Viterbi-decoded HMM per track (`app/src/behaviour/hmm.jl`) is the existing
>   persistence model; track diffmap/path shipped (`docs/todo/TRACK_BEHAVIOUR_AXES_PLAN.md`).
> - Not cited: `docs/todo/MULTI_POP_TRACKING_PLAN.md` (shipped; `track_source` stamping) and
>   `MULTI_POP_TRACKING_ORPHANS_PLAN.md`, `PLOTTING_CANVAS_AND_TRACK_DF_PLAN.md` (track pop type),
>   `CLUSTER_POP_PROPOSALS_PLAN.md` (track-class pops overlap trackclust pops),
>   `CONTEXTUAL_HINTS_PLAN.md` (clarity first, hints as fallback).
>
> Data behind it: lab box, project MERTK (`KiTRYL`), image `7HDUcJ`. Tracking ran on
> `Bcells/qc/Kat-/frags` only: 4,561 tracks, 50,053 of 706,661 cells. Untracked cells are
> `track_id = NaN`, and `track_source` holds the tracked pop's uid. Tracks are short (median 8
> timepoints, p10 5), so a fraction moves in steps of 1/5–1/8 and a purity threshold of 0.7 is coarse.

# TRACK_SUBPOP_GATING_PROMPT

**Task type:** audit + design proposal. Do NOT implement. Output: `docs/todo/TRACK_SUBPOP_GATING_AUDIT.md`.

## Background

Tracking can be run on a population that has sub-populations, e.g. `frags` with children
`mem-TOM+ nuc-GFP+`, `mem-TOM- nuc-GFP+`, `mem-TOM- nuc-GFP-`, `mem-TOM+ nuc-GFP-`
(gated on the Populations panel, under `qc > Kat- > frags`).

Observed gaps:

1. **Track colouring.** "Colour by: population" in the Tracks layer only knows the tracked
   (parent) population. Tracks cannot show sub-population colours.
2. **Track gating.** The track plot axis picker only offers `live.track.*` measures. Cells can
   be gated on morphology, channels and positions; tracks cannot.
3. **Track clustering.** "Cluster tracks" offers Track measures, Morphology, Channels and
   Behaviour as features. Gated populations are not usable as features; only raw values are.
4. **Gate flicker within a track.** Segmentation is imperfect and signal fluctuates, so a
   fragment that is mostly TOM+GFP+ shows TOM+GFP- timepoints. Per-object gating re-votes noise
   at every frame. Users need a track-level call that is robust to this.

## Proposed design (to be audited, challenged, and refined)

One primitive, three consumers.

**Primitive: per-track aggregates**, computed once after tracking from the per-cell table:

- `fraction_in(population)` for each sub-population (fraction of timepoints in that gate)
- per-feature aggregates (median default; also mean, min, max, std, first, last) for morphology,
  channels, position and time
- purity = max fraction, track class = argmax fraction with a threshold, else "ambiguous"

**Consumers:**

- **Colouring.** Add a sub-population depth selector next to "Colour by: population":
  Parent (default) / +1 / +2 / Leaf. Depth is capped at what exists under the tracked
  population. Add "Colour by: track class" (dominant call) alongside per-segment colouring
  (raw flicker, useful for QC).
- **Track gating.** Aggregates become plot axes with an aggregation dropdown beside the axis
  picker (not a multiplied list). Gates on them define track populations like cell gates.
  Median intensity is the preferred classifier; it is robust to single-frame flips and gate
  threshold placement. Fractions inherit gate thresholds and are less stable near them.
- **Track clustering.** Add a "Populations" section to "Cluster on" with one chip per
  sub-population fraction, usable together with behaviour features. Note fractions sum to 1
  (compositional redundancy); advise on scaling or dropping one.

## Audit tasks

1. **Map existing machinery.** Find and describe, with file paths and function names:
   - how tracking is run on a population and how tracks are stored (table schema, IDs, link
     back to per-cell rows and population membership)
   - how the Tracks layer colouring works ("Colour by", palette source, segment vs point)
   - how track measures (`live.track.*`) are computed and exposed to the plot axis picker
   - how cell-level gating exposes morphology/channels/positions, and why tracks do not
   - how "Cluster tracks" builds its feature chips and matrix (Leiden, UMAP, HMM touchpoints)
   - how populations and sub-populations are represented and queried (tree, depth, leaf)
2. **Reuse vs new.** For each piece of the proposal, say what already exists, what can be
   extended, and what is genuinely new. Prefer extending the cell-gating machinery to a
   second "entity" (tracks) over building a parallel track-gating system.
3. **Aggregate store.** Where should per-track aggregates live (computed task output, cached
   table, lazy)? Invalidation when tracking, segmentation, gates or populations change.
   Cost at realistic scale (e.g. 100k+ tracks, many timepoints). Handling of tracks with few
   timepoints, gaps, and merges/splits if supported.
4. **Correctness risks.** Gate-threshold dependence of fractions; ties in argmax; tracks
   spanning population changes legitimately (real biology vs flicker); timepoint weighting
   (equal vs size-weighted); compositional effects in clustering; behaviour of depth selector
   when siblings have different depths.
5. **Alternatives.** Briefly evaluate temporal smoothing before voting (sliding-window mode),
   hysteresis/enter-exit thresholds, and HMM state persistence. Say whether any justifies
   being in a first version or should stay later.
6. **Workflow weave.** Describe the current user path from segmentation, gating, tracking,
   to track measures/clustering, then propose where the new controls sit so the user never
   has to learn "aggregates" as a concept. Requirements:
   - one obvious place to choose how a track gets its population (dominant class, with the
     purity threshold visible and a default that works)
   - sub-population colour depth is a small control in the existing Colour by row
   - track gating reuses the same Populations panel and plot interface as cells; the only new
     thing the user sees is an aggregation dropdown on the axis
   - clustering gains one "Populations" section in the existing "Cluster on" list
   - plain-language labels and the contextual-hint conventions already used in the app; no
     new jargon in the GUI
7. **Consistency check.** Flag overlaps or conflicts with existing ideas and shipped features
   (channel lens populations, cluster-population co-driving, figure spec, GUI clarity and
   contextual hints audits, track diffmap/continuous clustering). Note anything that should
   be sequenced or merged.
8. **Tests and QC.** What tests protect aggregate correctness (known flicker fixtures,
   purity edge cases), and whether a QC readout (e.g. percent of ambiguous tracks) belongs in
   the UI.

## Addendum: tracked vs untracked population dots

Observed: with tracks and gated populations both shown, population dots appear for every gated
cell, including cells that were not tracked. The user cannot tell tracked from untracked cells.

Proposed: a toggle on the population dots, "Tracked only", so dots follow the tracked set.
Audit and propose:

- How the viewer decides which cells get population dots, and how to join that to track
  membership (cell ID to track ID) cheaply at render time.
- Default on or off when a tracks layer is visible for the same population (lean: on, with the
  toggle visible).
- A lighter alternative to hiding: dim or outline-only for untracked cells, so the mismatch stays
  visible but is clearly distinguishable. Recommend one.
- Whether untracked cells should be countable (e.g. "104503 gated, 98210 tracked") somewhere
  existing, such as the Populations panel row, as a QC readout.
- Behaviour with sub-populations and with the depth selector, so dot colours and track colours
  stay consistent.
- Where the toggle lives so it reads as one concept with the track colour controls.

## Addendum: tracked cells as a filter population

Question: should gated cells that are tracked become a new population type or a filter
population, so individual cell properties (morphology, channels, position) can be plotted and
gated for tracked cells?

Proposed: a **filter population**, not a new type. Audit and propose:

- Definition as a rule, `parent ∩ tracked` ("cells in tracks of population X"), evaluated
  dynamically so it follows re-tracking. Stored as a rule, not a frozen ID list. Assess whether
  a "freeze" option is needed for reproducibility and recommend yes/no.
- Appears as a child in the Populations tree (e.g. `frags > tracked`) and reuses existing cell
  plotting, gating and colouring with no separate code path. Check what in the current
  population model blocks or supports a rule-based child.
- **Non-destructive:** untracked cells are never dropped from the parent population or the
  underlying data. The filter is a view; the parent keeps all gated cells. Untracked cells
  stay visible and countable (gated vs tracked), and remain available for QC.
- Note the selection bias of per-cell stats on `tracked` (cells the tracker could follow);
  propose a one-line UI note.
- Generalise the same filter type to track-level calls, e.g. "cells in tracks classed
  TOM+GFP+ with purity >= 0.7", so cells inherit the track's dominant class (the flicker
  correction applied at cell level). Check the cost of resolving cell to track to class.
- The "Tracked only" dot toggle becomes a shortcut to showing this child. Confirm the two
  concepts do not diverge in code.
- Counts (`gated / tracked`) come from the filter size; reuse in the Populations panel row.

### Existing workaround and its problem

The Populations panel already supports "New filter population" with conditions. Today a user
can create `tracked` under a gated population with a condition on `track_id` (e.g. `track_id gt
<value>`). Problems: it must be created by hand under EVERY gated (sub)population that was
tracked, it is easy to forget or get out of sync, and `track_id gt <value>` is a proxy for
"is tracked" that depends on how untracked cells are encoded.

Audit and propose a version with zero manual repetition:

- A **scope**, not N populations: one "Tracked only" setting at the tracked parent that applies
  implicitly to all descendants (plots, gating, counts, dots, colouring). Compare against
  alternatives: (a) auto-created `tracked` child under every tracked (sub)population kept in sync
  by the system; (b) the scope above; (c) a filter-population option "apply to all children".
  Recommend one, with reasons (simplicity for the user, tree clutter, sync cost, how it
  interacts with gates users add later).
- Replace the `track_id` proxy with a first-class "is tracked" condition (and later "track
  class is X, purity >= Y"), verifying how untracked cells are encoded today (NaN, 0, missing).
- New sub-populations gated later under a tracked parent must pick up the scope automatically.
- Check whether the existing filter-population mechanism can be reused as the implementation
  so no second code path is created.
- Keep the plain-language label and one place to switch it, consistent with the "Tracked only"
  dot toggle.

## Constraints

- Keep the user model simple: cell populations in, track populations out, one threshold.
- Do not break existing track measures, existing clustering results, or saved gates.
- Prefer defaults that are right for most users over exposing options.
- No new heavy dependencies. Flag any licence issues.
- Respect the hardening phase: say which parts are small and safe versus feature-sized.

## Deliverable format

`docs/todo/TRACK_SUBPOP_GATING_AUDIT.md` with:

1. Current machinery map (paths, functions, data flow)
2. Gap analysis against the proposal
3. Recommended design (data model, API, UI), with any changes to the proposal and why
4. User workflow walkthrough, written as the user would experience it
5. Risks and open questions needing a decision from D
6. Phased build plan (smallest useful slice first), with rough size per phase
7. Test plan

Be direct. Cite code locations. Mark anything inferred rather than verified.
