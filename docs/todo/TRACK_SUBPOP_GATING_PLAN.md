# TRACK_SUBPOP_GATING_PLAN

Status: planning (2026-11-25) · branch `docs/track-subpop-gating` (#2513) · no phase built.
Brief: [`../archive/TRACK_SUBPOP_GATING_PROMPT.md`](../archive/TRACK_SUBPOP_GATING_PROMPT.md) (its
banner lists the premises that turned out wrong). Reviewed once by a fresh agent (2026-11-25); its
corrections are folded in.

## Goal

A tracked population with sub-populations (`qc > Kat- > frags` with four TOM/GFP quadrants) should
give the user, without learning a new concept:

1. tracked cells as a population at **cell** grain under every tracked (sub)population, on every cell
   plot, and **gateable** (a gate drawn under "tracked" is stored and follows re-tracking);
2. tracks gated on aggregates of cell measures (median TOM per track) from the track axis picker;
3. one call per track for which sub-population it belongs to, robust to frame-to-frame flicker, used
   for colouring, gating, clustering, and for handing the call back to the cells.

## What exists (verified at a40efa29c unless marked)

| Piece | Where | State |
|---|---|---|
| Per-track aggregates of any cell column | `app/src/tracking/track_props.jl` (`track_props`, `track_aggregate_measures`) | **built**, on read: `.mean/.median/.sum/.qUp/.qLow/.sd`; categorical → within-track fraction per category |
| Track gating on cell aggregates | `api/src/gating_api.jl:580-670` returns `cellMeasures` + `trackAggregates`; `pop_df.jl:277` resolves `area.median` | backend **built**; the gate axis picker sends motility only (`GatePlotPanel.vue:491`, `measureGroups.ts:84`); `gating.ts:322` stores `trackAggregates`, nothing reads it |
| Tracked cells as a rule pop | `_DERIVED_POPS["_tracked"]` (`pop_df.jl:571`, `track_id gt 0`, `is_track=true`, registered under `live`), injected per query (`_inject_derived_pops!`, `allow_list.jl:150`), offered where informative (`tracked_pop_parents`) | **built**; transient, never stored |
| Tracked family on cell plots | `plotDefinitions/cell_properties.json:9,20` (`{live, cell, "Tracked"}`); `plot_pop_types("live","cell")` injects `P/_tracked`; `pickerRows` (`plots/series.ts`) folds it into a row named `P` | **built** on Behaviour and the Analysis board; **missing on Phenotype**, which offers `flow`/`clust` only — the screenshot that started this plan |
| Module pickers | 15 of 17 `accepts` lists include `track`, so `_tracked` is offered (tagged track/tracked) and resolves at cell grain | **built**; `bayesian_tracking` and `branching` (`accepts: ["live"]`) drop it — tracking's input must keep dropping it (it reads the `flow` map, `bayesian_tracking.jl:86`) |
| `pop_df(img,"flow",["/qc/_tracked"])` | `pop_df.jl:96` (`has_pop` → `continue`) | **silently empty**; resolves under `live` only. Gating reads map flow+`_tracked` → `live` (`read_pop_type`, `mixed_resolution.jl:68`); `pop_df`/plot data do not |
| Gating under `_tracked` | `add_pop!` → "parent not found" (`mutations.jl:30`) | **impossible today**: the derived node is not in the stored tree |
| Untracked encoding | `python/cecelia/utils/tracking_utils.py:16` | `track_id = NaN`; no column if never tracked; `track_source` = uid of the pop tracked, or the whole-segmentation marker |
| Track ↔ population membership | `pop_df.jl:125` | a track is in a pop if **any** of its cells are |
| Track ribbons per pop | `_pop_has_authored_tracks` (`allow_list.jl:226-244`) | ribbons when `track_source` is the pop's uid, whole-seg, or missing (legacy). With per-pop tracking a sub-pop gets none. Attribution guard = ORPHANS P0, built |
| Track colour modes | `VwTracksSection.vue:70` | track / speed / source / population / colour by (per step, any obs column) |
| Cluster tracks features | `clustTracks/cluster.jl:107`, `ParamRenderer.vue:465` | base measures; the task aggregates all six; no population features |
| Persistence model | `app/src/behaviour/hmm.jl:321` (Viterbi per track) | motility states only |
| Tracked count | `overlaySummary` (`viewerOverlays.ts:440`) counts `track > 0` per payload | viewer-wide, not per pop |
| Doc | `docs/POPULATION.md:463-470` | contradicts itself: folding "whatever the plot's grain", then "cell plots keep the `_tracked` leaf" |

Real data (lab box, MERTK `KiTRYL`/`7HDUcJ`): 4,561 tracks on `frags`, 50,053 of 706,661 cells
tracked; track length median 8, p10 5, max 93 timepoints.

## Decisions (proposed 2026-11-25 — D to lock; ★ = needs D's call)

1. ★ **Tracked cells on Phenotype.** Phenotype gets the Tracked family the Behaviour page already
   has (one line in `cell_properties.json` + spec tests). On a cell plot the row reads
   **`P › tracked`**, not `P`: there they are different cells (fix the fold for cell grain, and the
   doc's contradiction). Alternative: inline `P › tracked` rows in the FLOW list (≈7 touchpoints:
   `pop_df` routing, flow enumeration, `is_track_pop`, `_accept_permits`, labels, tests) — not
   recommended, it puts the same cells under two keys (`flow::` and `live::`) on boards.
2. ★ **Gating under tracked = materialise on first use.** When the user draws the first gate under
   `P › tracked`, `P/_tracked` is written into the flow tree as a stored **rule** child (filter
   `track_id > 0`, reserved name allowed for this one writer), so its children persist and it
   follows re-tracking. Nothing is created until someone gates there, so no tree clutter and no
   sync. A gate added later under `P` is unaffected; one under `P/_tracked` is tracked-only by
   construction. Rejected: auto-creating under every tracked pop (brief's (a)); a global scope (b);
   "apply to all children" (c) — all three change counts invisibly.
3. **No freeze option.** The rule follows re-tracking; reproducibility comes from chain/version
   pinning, as for every other gate.
4. **Aggregates stay computed on read.** No stored table. The class threshold (Decision 5) is stored
   on the tracked pop in the gating file, so `_gating_inputs_stamp` already invalidates on change.
   At 100k tracks it is one `groupby` per requested base (inferred; time it in P3).
5. **Track class.** Among the children of the tracked population one level down (Decision 8):
   fraction of the track's timepoints whose cell is in each child; the class is the child with the
   largest fraction if that fraction is **> threshold** (strictly), else **mixed**. Default threshold
   0.5 (a strict majority): tracks are 5–8 timepoints, so a higher default leaves a large share mixed
   for one or two flips. Denominator: every timepoint of the track, including cells that are now
   outside every child (gates moved after tracking) — they count against purity. A gap is not a
   timepoint; two cells of one track in one frame (a merge) count once each. A tie is mixed.
6. **Gate-based class and intensity gating are both offered.** Class inherits the child gates and
   drives colouring and the class pops. Track gates on `CHANNEL.median` (P2) are the robust
   classifier near thresholds.
7. **"Any cell" track membership stays.** Changing it would move saved track gates. Class membership
   is a separate derived pop **under the tracked parent**, one per child: GUI label **mostly
   `<child>`**, rule `track class == child`. It must sit under the parent, not the child: a derived
   pop is parent ∩ filter, and under the child the flickered frames are exactly the ones dropped. At
   cell grain it hands the track's call to every cell of the track. Needs `DerivedPopSpec` to grow a
   parameter (which child) and a computed column; `pop_category` / `trackedOwner` must learn the new
   leaf.
8. **Level.** The class uses the children one level under the tracked pop by default; a level picker
   appears only when deeper levels exist. A leaf above the chosen level stands for itself.
9. **Tracked-only dots.** In the viewer, a **Tracked only** switch beside the Tracks colour row.
   Recommended: **dim** untracked cells rather than hide them (the mismatch stays visible); on by
   default when that population's ribbons are drawn. Implemented by the existing
   `filterPayloadByTracks`, client-side, no join. Same concept as `P › tracked`, so they are named
   the same.
10. **Selection bias.** `tracked` = cells the tracker could follow. One line in `docs/POPULATION.md`
    and the row's tooltip; nothing more in the GUI.

## Workflow, as the user sees it

1. Gate `frags` and its four quadrants; track `frags` (unchanged).
2. On Phenotype the family switch gains **Tracked**; rows read `frags › tracked`,
   `mem-TOM+ nuc-GFP+ › tracked`, … with counts. Drawing a gate under one stores it there.
3. In the viewer, **Tracked only** dims untracked dots.
4. On a track gating plot the axis picker gains Morphology/Channels; choosing `nuc-GFP` shows
   **median ▾** beside it.
5. Tracks **Colour by** gains **sub-population** (each step in its cell's colour — raw flicker) and
   **dominant sub-population** (one colour per track, grey = mixed) with **majority > 50 %** beside it.
6. Under `frags` the lists gain **mostly mem-TOM+ nuc-GFP+**, …
7. Cluster tracks gains a **Populations** group: one chip per child, value = fraction of time in it.

## Phases

| Phase | What | Size |
|---|---|---|
| **P1** | Tracked family on Phenotype; `P › tracked` label at cell grain; `POPULATION.md` fixed. Tests: spec test; Julia picker test on `fixture_for(:cells, :movie, :tracked)`; label tests (TS + Julia `pop_display_labels`) | small |
| **P2** | Track axis picker offers cell-measure aggregates + aggregation dropdown (frontend only). Default `median` | small |
| **P3** | **Tracked only** dot switch (Decision 9) + per-pop `gated / tracked` count | small |
| **P4** | Gating under tracked: materialise on first gate (Decision 2); `add_pop!` path, reserved-name writer, `read_pop_type` routing; tests for re-tracking and a later gate under `P` | medium |
| **P5** | Per-track sub-pop fractions + class on read — reuse the categorical branch of `track_props` (`track_props.jl:131`, already count/`nrow` per category) by feeding child membership as one categorical column built with `pop_membership` (`gating_engine.jl:270`); overlapping sibling gates (not quadrants, but possible) need a rule — first child in tree order, or one indicator column per child. Threshold stored on the tracked pop; QC readout: % mixed tracks. Synthetic flicker fixture, golden values from the definition | medium |
| **P6** | Tracks Colour by sub-population + dominant sub-population; level picker; recording parity (`ViewerDrawSpec`) | medium |
| **P7** | **mostly `<child>`** derived pops (Decision 7) at cell and track grain | medium |
| **P8** | Cluster tracks **Populations** group (fractions; docs note on compositional sums, no auto-drop) | small |

P1–P3 are independent and safe in the hardening phase. P5 gates P6–P8.

## Risks and open questions for D

- Decisions 1 and 2 (★).
- **Threshold default** (Decision 5): measure % mixed on `7HDUcJ` in P5 before locking 0.5.
- **Fractions near a gate line** move with the gate; the median-intensity track gate is the answer
  for borderline pops — say so in `docs/POPULATION.md`.
- **Real switching vs flicker**: a fragment that loses TOM is biology and the class hides it.
  Per-step colouring (P6) keeps it visible; sliding-mode smoothing, hysteresis and an HMM on gate
  state stay out of v1 (the HMM machinery exists for a later phase).
- **Clustering on fractions**: they sum to 1 per track; one chip is redundant — docs note only.
- **Overlap with trackclust populations** (`CLUSTER_POP_PROPOSALS_PLAN.md`): both are "tracks of kind
  X"; keep the **mostly** naming distinct from cluster names.

## Test plan

- P1: `cell_properties.json` spec test; picker test (Tracked family on Phenotype offers
  `/qc/OTI/_tracked` on a tracked fixture, nothing on an untracked one); label tests.
- P2: `measureGroups` with `popType: 'track'` + `cellMeasures` → Morphology/Channels groups; axis
  name round-trip `mean_intensity_1.median`.
- P3: `filterPayloadByTracks` dim mode; per-pop counts.
- P4: gate under `/qc/_tracked` persists, survives reload, follows re-tracking (more/fewer tracked
  cells), `add_pop!` still rejects `_` names from users.
- P5: `track_pop_fractions` on a synthetic table: `TTTFT` → T at 0.8; `TTFF` → mixed (0.5 strict);
  a 5-frame track with a gap; a cell outside all children; a tie.
- P6–P8: draw-spec recording parity; ParamRenderer group test; browser test for layout fixes.
