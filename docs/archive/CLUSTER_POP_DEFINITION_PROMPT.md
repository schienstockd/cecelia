> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> population design lives in [`docs/POPULATION.md`](../POPULATION.md); Claude's gating tools in
> [`docs/ai-assist/OBSERVER.md`](../ai-assist/OBSERVER.md).
>
> **Outcome:** not yet run. Archived 2026-10-31. Checked against `origin/main` on archiving:
> - #1852 (`add_gate`) has **merged**; #1856 is **still open** (`feat/gate-check`, now based on
>   `main`). Everything the brief cites from #1856 exists only on that branch: `register_gating_tools`,
>   `GATING_ROUTES`, `set-own-gate`, `delete-own`, `_own_subtree`, `claude_made`, `not_claude_made`,
>   `dryRun`, `gate_examples`, `_render_still`, `_example_picks`.
> - `cell_cards.jl` is `app/src/cell_cards.jl`, not `api/src/`.
> - **Q1 is largely answered in the docs.** Cluster-defined populations already exist as three stored
>   filter pop types — `clust`, `trackclust`, `region` — each a `Population` with
>   `filter_measure="clusters.{suffix}"` (`regions.{suffix}` for region), `filter_fun="in"`,
>   `filter_values=[ids]`, in its own sidecar (`gating/{vn}__clust.json` etc.), created via `pop/add`
>   with a `filter` dict ([`POPULATION.md`](../POPULATION.md) table + "stored filter pops"). Naming one
>   pop over several ticked IDs is already how clusters are pooled; written set-wide, no undo.
> - **The "definition union" hypothesis already exists**: gate, filter and boolean are the three
>   membership kinds on one `Population`, all via `pop/add`. The open question is putting Claude's rails
>   (stamp, own-only, dry-run) on `pop/add`'s filter path, not inventing a union.
> - **"Pooling" collides with [`CLUSTER_POOLING_PLAN.md`](../todo/CLUSTER_POOLING_PLAN.md)**, which
>   means pooling one run *across co-clustered segmentations* (`co_clustered_value_names`, built) — not
>   merging clusters into a population.
> - Q6 partly exists: observer `get_cluster_summary` already returns each run's per-cluster profile
>   (the heatmap's z-scored means; `app/src/ai/behaviour_clusters.jl`). `get_region_clusters` gives
>   region sizes only, no composition profile. `cluster_markers` / `cluster_examples` exist nowhere.
> - Q7's diffmap notes are archived ([`DIFFMAP_TRACK_COMPARISON.md`](DIFFMAP_TRACK_COMPARISON.md) +
>   `_RESULTS`); the live design is [`TRACK_BEHAVIOUR_AXES_PLAN.md`](../todo/TRACK_BEHAVIOUR_AXES_PLAN.md)
>   (#1823 merged).

# CLUSTER_POP_DEFINITION_PROMPT

Read-only brainstorm + audit. Do not edit code, do not open a PR. Output is a recommendation doc.

## Context

PR #1856 (stacked on #1852) gave Claude a propose / judge / revise loop for gates:

- one gating toolset registered on both MCP servers (`mcp/cecelia_mcp/gating_views.py`, `register_gating_tools`)
- Claude-scoped writes only: `add-gate`, `set-own-gate`, `delete-own` (403 `not_claude_made` unless the population and everything under it has a Claude `createdBy`)
- `dryRun` on every write
- `gate_examples` (boundary / random modes, raw | outlined crops) so Claude can judge a gate on the cells

Proposal: extend the same idea to **populations defined from Leiden clusters**, for **cells, tracks and regions**.

Decisions already made:

- Claude **does not run clustering**. It only defines populations on top of existing clusters.
- Definition order goes **either way**: cluster then gate, or gate then cluster (clustering run on a gated population).
- It should be a **natural extension of the gating population definition**, not a parallel system.
- Same rails as gates: Claude-stamped (`via: claude`), own-only edits, dry-run, evidence before accept.

**Why:** clustering often yields tens of clusters, and it is hard to see which belong together and how to pool them into meaningful populations. The core job is **grouping and pooling clusters** (finding groups, proposing merges, naming the pooled result), not just labelling single clusters. Must work for **cells, tracks and regions** (the Cluster regions module) from the start; none are deferred.

Working hypothesis (challenge it): one `add_pop` with a definition union (`gate` | `clusters`), where a cluster node records its source (clustering ref, parent population, cluster IDs).

## Read first

- Root `CLAUDE.md`
- `docs/inventory/MCP.md`, `JULIA_API.md`, `JULIA_APP.md`, and the POPULATION / API / OBSERVER docs
- `mcp/cecelia_mcp/gating_views.py`, the two servers' clients and allow-lists (`GATING_ROUTES`)
- `api/src/gating_api.jl`: `set-own-gate`, `delete-own`, `_own_subtree`, `claude_made`, `_with_popmap_lock`, `load_pop_map`, `boolean_dependents`, `_persist_and_broadcast!`
- `app/src/config/profile_settings.jl` (`author_stamp`)
- `api/src/cell_cards.jl` (`pick_card_example`), `behaviour_cards.jl` (`render_medoid_filmstrip`)
- Wherever cluster populations (cells, tracks and regions) are currently created and stored
- `api/src/kiwi_turn.jl` tool exclusion table
- My notes on the diffmap / continuous-axes idea (`DIFFMAP_TRACK_COMPARISON`) before answering Q7

## Questions

1. **Current state.** How are cluster-derived populations represented today (cells, tracks, regions)? Can a population already be defined by cluster IDs? What is the persisted shape, and what does a cluster ID refer to (run, parent, stability across re-runs)?
2. **Definition shape.** Definition union on one route set vs a sibling `add-cluster-pop`. What does each cost in the popmap schema, `boolean_dependents`, persistence, MCP schemas, and tests? Recommend one.
3. **Staleness, both directions.**
   - Gate edited or deleted under which a clustering ran: cluster children no longer match.
   - Re-clustering under gate children.
   What must be recorded to detect each? Where is the check surfaced (`dryRun` reply, app marking)? Which edits can Claude cause, and which only the user?
4. **Ownership on mixed chains.** Does the "population and everything under it is Claude's" rule hold when gates and cluster nodes interleave? Walk through the edge cases (Claude cluster pop under a user gate, user gate under a Claude cluster pop, merged cluster sets).
5. **Judging tools, cells.** `cluster_markers` (top differential markers) and `cluster_examples` (centroid-near vs interface cells, same raw | outlined sheet). What can reuse `_render_still`, the sheet composer, `_example_picks`? What is new? Per-call cost.
6. **Pooling with tens of clusters.** What lets Claude (and me) see groups? Use the views the app already has: the cluster **heatmap** (cluster x marker/feature, ordered) and the **UMAP** (clusters coloured or overlaid, with a region crop). Which of these can Claude read as an image, which as numbers, and which need a new endpoint? Also: cluster-to-cluster similarity (marker profile for cells, behaviour feature profile for tracks), dendrogram or ordering, merge candidates with evidence, "what separates A from B" tools? How is a pooled population recorded (union of cluster IDs plus its source) and how are overlapping or nested pools handled?
7. **Tracks and regions, same tools.** Evidence for a track cluster (medoid filmstrip, behaviour summary). For tracks the evidence also includes **HMM states** (state composition per cluster) and the **diffmap**. For regions: what is the evidence (cell-population composition per region, spatial view crops, any region features the app already computes) and how are region populations defined and stored? Design one generic "cluster features + examples" interface so cells, tracks and regions share the tools. Given the diffmap / continuous-axes idea, does it survive replacing hard Leiden+UMAP+HMM clusters?
8. **Confabulation guard.** Claude names a cluster from a marker list with no validation. What evidence should be stored with the label, and how does the user accept, reject or edit a ✦ proposal quickly? What is the minimum that makes a wrong label cheap to catch?
9. **Servers and surfaces.** Observer vs autonomous server, project lock, label-set prefix, measured-segmentation check, the Kiwi tool lists, the in-app Claude access table, the run and co-drive recorders (dry-run = look, not write). What needs to change in each?
10. **Duplication / fanout.** What existing helpers should this reuse rather than copy? Any second copy of cluster-population logic that would drift?
11. **Slicing.** Smallest useful first PR, then the order of follow-ups. What is checkable headlessly on UMq4v3, and what needs a live session?

## Output

- Recommendation first (3–6 lines), then answers by question number.
- Cite evidence as `file:line`. Mark each claim **verified** (read in code) or **assumed** (not checked).
- List the open decisions only I can make, with your pick for each.
- Name the top 3 ways this design could go wrong, and the test that would catch each.
- Terse. No restating the context above.
