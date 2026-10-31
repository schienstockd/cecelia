# Parked plan — Claude-proposed cluster populations

Status: **parked on a dataset** (2026-10-31). Waiting for a multiplex image that clusters into 10+
groups — the Call for Datasets entry `cluster-grouping-highplex` (`frontend/src/lib/callForDatasets.ts`).
Audit behind it: [`../archive/CLUSTER_POP_DEFINITION_REPORT.md`](../archive/CLUSTER_POP_DEFINITION_REPORT.md)
(archived; this plan holds what was decided from it).

## Problem

Clustering a high-plex panel gives tens of clusters; grouping and naming them is the hard part.
Cluster populations already exist (`clust` / `trackclust` / `region`: stored filter pops over
`clusters.{suffix}`, `docs/POPULATION.md`), and Claude can already read each cluster's profile
(`get_cluster_summary`) and suggest groupings in chat. What is missing is Claude writing a proposal
(✦) the user accepts, renames or deletes — the gate rails of `add-gate`, for clusters.

## Decisions

1. **The write waits for the data.** On our own data no named pool uses a cluster id above 3, so the
   proposal step cannot be checked yet. Until a 10+-cluster dataset arrives: chat proposals only.
2. **A sibling route, not `pop/add`.** `POST /api/gating/pop/add-cluster-pop` — create-only, set-wide
   on the server (the run's `partOf` images × co-clustered value_names), only cluster ids no other
   pop in the run holds, `dryRun`. `pop/add` stays off the MCP allow-lists (it takes any pop type).
3. **Ownership is the gate rule.** `createdBy` / `updatedBy` as for gates: a user's edit makes it
   theirs (built with the gate fix, `stamp_edit!`). No `set-own-cluster-pop`: change = delete + propose.
4. **Proposed, not yet confirmed by Dominik:** names drawn from the top-|z| features until the user
   renames; a re-run on the same suffix with named pops warns (stale icon) rather than blocks;
   cluster-id exclusivity enforced server-side for GUI ticks too.

## Slices

1. **Reads (buildable now):** region profiles via `_cluster_profile(…; pop_type)`; `popType` on
   `gating/examples` + `cells-image`; per-cluster spread / nearest-cluster on profiles.
2. **The write (on the dataset):** the route above, `delete-own` taking a pop type, a run fingerprint
   for staleness, and a cohesion number in the `dryRun` reply.
3. **Heatmap cluster ordering** for the user.
