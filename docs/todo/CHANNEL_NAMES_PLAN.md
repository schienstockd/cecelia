# Channels are identified by name, everywhere

**Status:** built (2026-11-16), in review. Branch `feat/channel-names-converge`, worktree
`cecelia-channel-names-converge`. One PR — no partial convergence ships. See *Built* at the end.

## Goal

A task param, a stored feature list, or a plot request names an intensity measure by **channel name**
(`CD4`, `nuc_CD4`, `CD4.mean`). Each image resolves that name to its own raw column
(`mean_intensity_<i>`) in **one resolver**. The raw, index-based column name is a storage detail of the
h5ad, not an identity.

## Why — the report that started it

User on 0.2.12: *"Unhandled error in task — hmm_fit_states: DataFrame is missing column(s)
[mean_intensity_1, mean_intensity_2]"*, "not all the time". Trace:

- The `labelPropsColsSelection` picker shows channel names but **stores the raw column**
  (`mean_intensity_1`).
- `behaviour.hmm_states` reads through `pop_df_multi`, whose default renames intensity columns to
  channel names, so the frame holds `CD8`, not `mean_intensity_1`.
- `_require_cols` therefore throws whenever an intensity channel is modelled on an image that has
  channel names. Motion-only models never hit it, which is the "sometimes".
- It also has no per-image channel matching, so pooling images with different channel orders would
  model different markers under one column.

The other consumers each solved "which column is this channel on this image" separately, one bug at a
time:

| Mechanism | Where | Identity in params |
|---|---|---|
| `channel_index` / `channel_indices` | pixel tasks (segment, cleanup) — Aug, `86910e5de` | **name**, resolved per image |
| `_channel_name_to_raw` in `as_df` | Julia reader (exact case, last-wins on duplicates) | name, if a caller passes one |
| `_channel_name_to_raw` in `LabelPropsView` | Python reader — no production caller passes `channel_names` | dead |
| `match_channel_columns` / `match_segment_channel_columns` | gate copy (#1901), clustPops, clustTracks (`rebase_track_aggregates!`), heatmaps (`_pop_df_by_channel_name`) | **raw on the first image**, re-indexed per image |
| nothing | hmm_states, roiDiscovery | raw, applied by index |

There are three resolvers with three sets of rules (case-sensitive vs not, last-wins vs `:ambiguous`).
The MCP side adds a fourth disagreement: `get_measure_summary` hands agents **channel names**
(`ai/measures.jl`), which clustPops and roiDiscovery cannot resolve. clustPops then **fills the column
with 0.0** and logs a WARN (`cluster_run.py`), which silently corrupts the clustering.

Not deliberate. Pixel tasks got it right first; measure tasks kept the index because the h5ad columns
are indexed, and per-image matching was retrofitted per caller.

## Decisions (2026-11-16)

1. **A channel is identified by its name** in every param, stored feature list, plot request and
   sidecar. Same model as pixel tasks.
2. **One resolver**, `resolve_channel_columns(names_or_raw, var_names, channel_names)`, in
   `label_props.jl`. It returns `(; cols, problems)` and handles:
   - plain names (`CD4`), prefixed names (`nuc_CD4`), and per-track aggregates (`CD4.mean`). An
     aggregate is recognised only by the known `_TRACK_NUM_AGG_SUFFIXES`, so a channel called
     `CD45.2` stays a name.
   - **case-insensitive** matching (the `mem-TOM` / `mem-Tom` case from #1901).
   - **`:absent` / `:ambiguous` problems**, never last-wins. Ambiguous means the name sits on two
     channels of this image, or it also names a non-intensity column (a channel called `area`).
   - **anything that is not a channel name passes through** unchanged (`area`, `live.cell.speed`,
     `clusters.x`, raw names).

   `as_df` (Julia) uses it, and the Python `LabelPropsView` mirrors the same rules.
3. **Legacy raw params are canonicalised once.** `canonical_channel_columns(img, vn, cols)` maps raw
   intensity columns to channel names using the image they were picked on. Each task's params pass
   through it at run start, against the reference segment (the first pooled one, the same reference
   `match_segment_channel_columns` used). Saved chains and old run params keep working and mean what
   they meant. An image with no channel names keeps raw names: there is nothing to match, and every
   image then agrees by index, as before.
4. **Pooled readers need no per-caller matching.** With names, `pop_df` / `pop_df_multi` /
   `track_props` resolve per image and return columns **under the requested name**. Deleted:
   `match_segment_channel_columns`, `_pop_df_by_channel_name`, `_match_track_channels`,
   `rebase_track_aggregates!`, and the per-segment `featureCols` remap in `cluster_run.py`.
5. **Python runners stay dumb.** Julia resolves each segment's raw columns and hands them over with the
   output names. A runner never matches channels. A missing column is an **error**, not a zero fill.
6. **Stored lists hold names**: clustfeatures `features`, `behaviourAxes` features, hmmfit measures.
   Old sidecars holding raw names are canonicalised **on read** against their own image (a sidecar is
   per image, so its own names are the right ones). The API serves names, and the frontend shows them
   with no `nameMap` lookup.
7. **Gates stay raw, per image.** A gate is drawn on one image's columns, and the gating engine
   evaluates raw columns. Gate **copy** is "canonicalise on the source, resolve on the target", built
   from the two primitives. `match_channel_columns` is then a thin composition, not a third resolver.
8. **The picker stores names.** `labelPropsColsSelection` value = `channelLabel(col)`. Display is
   unchanged; only the stored value changes. A stored legacy raw value still shows its channel label.

## Phases (commits on one branch)

- **P1 — resolver + reader.**
  - `resolve_channel_columns`, `canonical_channel_columns` and `describe_channel_problems`.
  - `as_df` switched onto the resolver; the old `_channel_name_to_raw` removed.
  - Python `LabelPropsView` given the same rules.
  - Tests: case, ambiguity, aggregates, `CD45.2`, shadowing, no names.
- **P2 — tasks.**
  - hmm_states (the reported bug), clustPops, clustTracks and roiDiscovery canonicalise params and
    read by name.
  - Per-caller matching deleted; `cluster_run.py` zero-fill turned into an error.
  - roiDiscovery resolves `rankBy` / `groupBy` per image in Julia before the handoff.
  - Golden / typed_params tests updated; a regression test pools two images with swapped channel order
    and modelled intensity.
- **P3 — stored lists + readers.**
  - Writers store names.
  - `_clustfeatures_*`, hmmfit and behaviour-axes readers canonicalise on read.
  - `cell_cards.clustering_features_pooled` resolves through the reader.
  - Heatmap request by name (`plot_data.jl`).
  - MCP `get_cluster_summary` gets names for free.
- **P4 — frontend.**
  - The picker stores names.
  - `useClusterContext` / `ClusterHeatmapPanel` / `behaviourAxes` drop the raw→display `nameMap` for
    features (keeping `labels` for region runs).
  - Persisted heatmap feature picks are canonicalised on load.
- **P5 — gate copy** on the primitives; `match_channel_columns` reduced to the composition. Docs:
  `DATAMODEL.md`, `POPULATION.md`, `MODULES.md` (picker), `REPL.md`, `app/CLAUDE.md` *Channel names →
  indices* extended to measure columns.

## Out of scope

- Renaming the h5ad columns themselves. Storage stays index-based.
- Gate storage (Decision 7).

## Built (2026-11-16)

P1–P5 in one branch. Notes where the build differed from or added to the plan:

- **`pop_df` cache key now includes the channel names.** A by-name read keys the same before and
  after a channel rename (a ccid.json edit, so no h5ad mtime changes), so it would have kept serving the
  old column. This was latent before too (stale column labels after a rename). Found by the heatmap
  test's "same order after a rename" case.
- **Absence is checked at set level, not in the resolver.** `resolve_channel_columns` passes an
  unknown name through (it may be an obs column). `resolve_segment_channel_columns` reports `:absent`
  for a name that is a channel on the first segment and on no channel of another.
- **The set-level entry is `pooled_channel_columns(imgs, pops, cols…)`** (+ `pop_segments`,
  `mixed_resolution.jl`). hmm_states and clustTracks re-parse their params with the named columns.
- **hmmfit records are not renamed on read.** A run with a raw intensity measure on an image with
  channel names never completed (this bug), and on an image without names there is nothing to rename.
- **roiDiscovery** passes `readCols` (name → this image's column) to its runner, which reads those
  columns and keeps the names.
- **Frontend:** `labelMap` is now only a region run's labels (features arrive named). The heatmap
  migrates a saved raw pick with the image's channel names before reconciling it.
- **Cell cards read a track run's features like clustering did** (found on the way, fixed here):
  `clustering_features_pooled` (`cell_cards.jl`) read them from `__tracks.h5ad`, which holds no on-read
  aggregates (intensity, HMM), so those came back NaN and the medoid ignored them. It now reads through
  `track_props` (motility ⊕ aggregates), with the features named like the API names them.
