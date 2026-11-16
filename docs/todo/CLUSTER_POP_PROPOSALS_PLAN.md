# Plan — Claude-proposed cluster populations

Status: **write built** (2026-11-16) — `add_cluster_pops` → `POST /api/gating/pop/add-cluster-pops`. Was
parked on a dataset (2026-10-31); the maintainer un-parked the write because grouping clusters is what
cluster populations are for, and the write itself can be checked on our data. Whether Claude's groupings
hold up on tens of clusters still needs one: the Call for Datasets entry `cluster-grouping-highplex`
(`frontend/src/lib/callForDatasets.ts`) is now a TEST ask. Remaining: slices 1 and 3. How the built part
works lives in `docs/POPULATION.md` (*One population per cluster*, *Run stamp*, *Claude groups a run*).
Audit behind it: [`../archive/CLUSTER_POP_DEFINITION_REPORT.md`](../archive/CLUSTER_POP_DEFINITION_REPORT.md)
(archived; this plan holds what was decided from it).

## Problem

Clustering a high-plex panel gives tens of clusters; grouping and naming them is the hard part.
Cluster populations already exist (`clust` / `trackclust` / `region`: stored filter pops over
`clusters.{suffix}`, `docs/POPULATION.md`), and Claude can already read each cluster's profile
(`get_cluster_summary`) and suggest groupings in chat. What is missing is Claude writing a proposal
(✦) the user accepts, renames or deletes — the gate rails of `add-gate`, for clusters.

## Decisions

1. ~~**The write waits for the data.**~~ Overturned 2026-11-16 by the maintainer: the write ships; the
   dataset checks the proposals' quality, not the write.
2. **A sibling route, not `pop/add`.** Built as `POST /api/gating/pop/add-cluster-pops` — a whole GROUPING per
   call (`groups: [{name, clusters}]`, several clusters per population), create-only, set-wide
   on the server (the run's `partOf` images × co-clustered value_names), only cluster ids no other
   pop in the run holds, `dryRun`. `pop/add` stays off the MCP allow-lists (it takes any pop type).
3. **Ownership is the gate rule.** `createdBy` / `updatedBy` as for gates: a user's edit makes it
   theirs (built with the gate fix, `stamp_edit!`). No `set-own-cluster-pop`: change = delete + propose.
4. **Confirmed by the maintainer 2026-11-16, built:** names drawn from the top-|z| features until the user
   renames (guidance); a re-run on the same suffix with named pops warns (stale icon, the run stamp)
   rather than blocks; cluster-id exclusivity enforced server-side for GUI ticks too.

## Slices

1. **Reads (buildable now):** region profiles via `_cluster_profile(…; pop_type)`; `popType` on
   `gating/examples` + `cells-image`; per-cluster spread / nearest-cluster on profiles.
2. **The write — BUILT** (2026-11-16): the route above, `delete-own` taking a pop type, the run stamp
   for staleness. Not built: a cohesion number in the `dryRun` reply.
3. **Heatmap cluster ordering** for the user.
