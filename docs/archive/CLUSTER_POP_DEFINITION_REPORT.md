> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was investigated at the time. It is not a description of how the code works now. Current population design lives in [`docs/POPULATION.md`](../POPULATION.md). Brief: [`CLUSTER_POP_DEFINITION_PROMPT.md`](CLUSTER_POP_DEFINITION_PROMPT.md).

# CLUSTER_POP_DEFINITION_REPORT

Audited 2026-10-31 against `origin/main` @ `5af29afe` and the open `origin/feat/gate-check` (#1856) @ `582852ae`.
**V** = verified in code / on disk. **A** = assumed, not checked.

## Verdict

- **Possible:** yes, and cheaply. Cluster pops are already stored filter pops with a `createdBy` slot. A Claude write is one sibling route next to `add-gate`, plus making #1856's helpers take a pop type (they are hard-wired to `"flow"`).
- **Desirable:** **the small version only.** Most of the value is in *seeing* which clusters belong together. Claude can already do that in chat from `get_cluster_summary.profiles`, the heatmap's own matrix. A Claude-written pool only saves the user a few ticks in a tick grid. It also adds three risks gates don't have: the write is set-wide with no undo, cluster-ID exclusivity is enforced only in the client, and a re-run silently renumbers the IDs under a name Claude chose.
- **The data does not show the "tens of clusters" pain yet (V, this machine).** Across every dev project, the largest cluster ID in a named pop is 3. There is no cell-level `__clust.json` anywhere. 6 of 7 cluster sidecars still have the default "Population N" names. The one hand-named pool is `Meandering = [1, 2]` (4kS67f).
- **Recommended scope:** **Slice 1 = reads only.** Add region profiles, let the examples / cells-view tools take a pop type, and add a guidance line: "propose pools in chat with the z evidence; the user ticks". **Slice 2 (the ✦ write)** only once a high-plex dataset with ≥10 clusters actually shows up. Even then it is create-only, takes only IDs nobody owns, and is checked for staleness.

## Answers

### Q1 — Current state
- `clust`/`trackclust`/`region` are stored filter pops in their own sidecar. Each is a `Population` with `filter_measure="clusters.{sfx}"` (or `regions.{sfx}`), `fun="in"` and `values=[ids]` — `docs/POPULATION.md:125-133` **V**, `app/src/gating/popmanager/persistence.jl:125-133` **V**. On-disk shape confirmed in `4kS67f/1/yFNlw2/gating/B__trackclust.json` **V**.
- Pooling already exists: one pop with several ticked IDs (`Meandering: values [1,2]`) **V**. Pops are created via `pop/add` + `filter` (`api/src/gating_api.jl:1195-1233` **V**) and ticked via `pop/update` `filter.values` (`gating_api.jl:1393-1437` **V**).
- **A cluster ID refers to** the column `clusters.{suffix}` of one run on one segmentation set. It is comparable across `partOf` images and co-clustered vns (`docs/POPULATION.md:176-212` **V**). It is **not stable across re-runs**: the runner overwrites the column (`python/cecelia/utils/clustering_utils.py:250-317` **V**). Leiden relabels by size on any input change (**A**, scanpy behaviour). `random_state=0` makes an identical re-run reproduce (`clustPops/cluster_run.py:97` **V**).
- **What the run manifest does not record:** which population the run clustered (`popsToCluster`). The clustfeatures entry holds only `features, partOf, family, labels, behaviourAxes` (`app/src/tasks/clustPops/cluster.jl:91-104` **V**).
- **Parent:** a cluster pop's parent must be a pop in the **same sidecar** (`app/src/gating/popmanager/mutations.jl:28` **V**). So "gate, then cluster" is linked through the task param, not through the pop tree.
- **Exclusivity:** one cluster ID per pop within a run is enforced **client-only** (`frontend/src/components/canvas/PopulationManager.vue:215-228` **V**). The server accepts overlapping pools.
- **Set-wide:** writes are replayed **client-side** over `mirrorUids` (`frontend/src/stores/gating.ts:262` **V**, `composables/useClusterContext.ts:124` **V**).
- **No undo:** cluster pops are not undoable (`gating_api.jl:342-354`, `is_gating_pop_type` **V**; `POPULATION.md:656` **V**).

### Q2 — Where the rails go: on `pop/add`'s filter path, or a sibling route
- **Sibling route.** #1856 deliberately **removes** the `REQUEST_VIA=="claude"` stamp from `pop/add` (diff of `api/src/gating_api.jl` main:1224-1227 → `feat/gate-check:api/src/gating_api.jl:1221` **V**). It also keeps `pop/add` off both allow-lists, because `pop/add` also takes gates, booleans and any pop type (`feat/gate-check:mcp/cecelia_mcp/client.py` ALLOWED_ROUTES comment **V**). Putting rails on `pop/add` would undo that decision and put a Claude branch into the GUI's own route.
- **Shape:** `POST /api/gating/pop/add-cluster-pop` `{projectUid, imageUid, valueName, popType: clust|trackclust|region, suffix, name, ids, parent?, colour?, dryRun?, evidence?}`. Body:
  1. Calls the same `add_pop!(…; filter_measure, filter_fun="in", filter_values, created_by=author_stamp())`.
  2. Resolves the target images from `partOf` and `co_clustered_value_names`.
  3. Refuses IDs another pop in the run already owns (409, naming the owner).
  4. Validates every image, then writes every image.
- **Cost:**
  - Popmap schema: zero (filter + `createdBy` exist, `persistence.jl:32,88` **V**).
  - `boolean_dependents`: unchanged. `delete-own` already checks it (`feat/gate-check:api/src/gating_api.jl:1361-1363` **V**).
  - Persistence: the same `_persist_and_broadcast!`, once per image.
  - MCP: one tool in `register_gating_tools` plus one entry in `GATING_WRITE_ROUTES` (`feat/gate-check:mcp/cecelia_mcp/gating_views.py:116-125,251` **V**). Both servers pick it up.
  - Tests: one API suite in the shape of `api/test/suite/gating_add_gate.jl` (**A** on effort).
- **Generalise, don't copy:** `_claude_gate_body`, `_own_subtree` and `_claude_gate_reply` (`feat/gate-check:api/src/gating_api.jl:1284-1332` **V**) hard-code `"flow"` at `:1258,1320,1323,1343,1359,1368` **V**. Give them a `pop_type` argument. `delete-own` then works for cluster pops for free.
- **No `set-own-cluster-pop`:** to change a pool, delete it and propose a new one. That keeps "own-only" trivially true.

### Q3 — Staleness
- **(a) Gate edited or deleted after a clustering ran on it.** The cluster column is a snapshot. Cells that left the gate keep their ID, and cells that entered have none. Nothing records the link today (Q1). This is also a **#1856 gap**: `set-own-gate` and `delete-own` refuse only `boolean_dependents`, so Claude can move its own gate after the user clustered on it (**V**, `:1336-1373`).
  - *Record:* at run time, write `source: {pops, specHash per pop}` into the clustfeatures entry (`_write_clust_features!`, `cluster.jl:91` **V**).
  - *Surface:* refuse with 409 in `set-own-gate` / `delete-own`, and show a `missingReason`-style icon on the cluster pops.
- **(b) Re-run on the same suffix under existing pops.** IDs are renumbered and the pops silently point at other clusters. This already applies to the user's pops (**V**: nothing checks it).
  - *Record:* a run fingerprint (a run id, or the per-ID sizes) in the clustfeatures entry. Store it on each cluster pop at write time, from the GUI as well as from Claude.
  - *Surface:* the `dryRun` reply, the popmap reply's per-pop warning, and the manager icon.
- **Which edits Claude can cause:**
  - The observer cannot run tasks, so it causes neither (a) nor (b).
  - The autonomous server can `run_task`, so it can cause (b), and on #1856 it can cause (a) via `set-own-gate`.
  - The user causes both.

### Q4 — Ownership on mixed chains
- **Gates and cluster pops never share a tree** (`mutations.jl:28` **V**). So "user gate under a Claude cluster pop", and the reverse, cannot happen inside one tree. The cross-type edge is the task param (`popsToCluster`, `popsToTrack`), and `_own_subtree` can't see it. That edge needs the Q3a record.
- **Inside the cluster sidecar:**
  - A Claude pool under a user pool: allowed, the same as add-gate under a user parent.
  - A user pop under a Claude pool: the pool is locked (`_own_subtree` **V**).
  - Merged sets: Claude may take only **unowned** IDs. Taking an ID from a user pool would edit the user's pop.
- **Hole (V, main and branch):** `created_by` is set only in `add_pop!` (`mutations.jl:55` **V**) and never cleared. The user's `pop/update`, `set-gate` and `rename` keep the stamp, so `claude_made` stays true. Claude can then `set-own-gate` / `delete-own` a ✦ pop **the user has since edited**, which overwrites their edit. This applies to #1856 gates now and would carry over to cluster pops. Fix: any GUI mutation of a stamped pop marks it as adopted (or clears `via`).

### Q5 — Judging tools for cells
- **`cluster_markers`: not needed as a new tool.** `get_cluster_summary.profiles` already holds the per-cluster z-scored means over the run's features. It is the heatmap's own `plot_summary_data` call (`app/src/ai/behaviour_clusters.jl:111-133` **V**). "What separates A from B" is the difference between two columns, and Claude can compute it. Missing: within-cluster spread and between-cluster distance. Add both as fields on the profile, not as a new tool.
- **`cluster_examples`: generalise `gate_examples`.**
  - It hard-codes `_live_map(img, vn, "flow")` (`feat/gate-check:api/src/gating_views_api.jl:258` **V**). The same holds for `cells-image` via `read_pop_type(…, "flow")` (`:114` **V**). With a `popType` param, random mode works on a cluster or pool path.
  - `_example_picks(mode, n, ins, dist)` (`:341` **V**) is reusable as is: `ins` = in the cluster, `dist` = margin to the nearest other cluster centroid. That gives "centroid-near" and "interface" sheets.
  - `_render_still` (`:192` **V**) and `_example_sheet` (`:389` **V**) have no pop knowledge, so both are reusable.
  - **New:** the margin computation. Extract it from `medoid_track`, whose distance loop is general but whose return value is tied to `_track_id` (`app/src/cell_cards.jl:112-153` **V**).
- **Cost per call:** 2 renders per tile; at most 8 tiles per row and 2 rows, so at most 32 `render_view_stills` calls. Not measured (**A**).

### Q6 — Pooling with tens of clusters
- **Numbers (already exist):** `get_cluster_summary.profiles` is the heatmap as data, for clust and trackclust **V**.
- **Region profiles are missing.** `_cluster_profiles` reads only `family="clusters"` (`clustering_colour.jl:64-72` default **V**), and `get_region_clusters` returns only sizes (`mcp/cecelia_mcp/server.py:724-738` **V**). The heatmap body already supports `region` (`frontend/src/utils/clusterHeatmapBody.ts`, `ClusterPopType` **V**). So the same `plot_summary_data` call with `pop_type="region"` should return the composition profile (**A**, not run).
- **Images:** the heatmap and UMAP can't be read as images by MCP. `add_analysis_board` puts them in front of the user, not in front of Claude. Claude doesn't need the image when the matrix is available.
- **Ordering and similarity:** there is no dendrogram or row ordering anywhere (exhaustive grep for `dendrogram|hclust|linkage|seriation|rank_genes_groups` across app/api/frontend/python/mcp: **0 hits**, **V**). Claude can compute correlation or distance from the profile.
  - If the user should see it too: add a cluster-order option to the heatmap, which is a frontend change. For Claude: an `order` field and a `nearest` field on each profile.
- **Recording a pool:** one filter pop, `values = union of IDs`, `filter_measure` = the run (already the case).
  - **Overlapping pools:** blocked by exclusivity (enforce it server-side in the new route).
  - **Nested pools:** "T cells ⊃ CD8 ⊃ cluster 4" means a child filter pop under a parent filter pop. That is allowed by `add_pop!`, but none exist on disk (nested=0 **V**). The tick UI is flat per run (A).
  - **Checking a pool:** heatmap per-population mode (`category="pop"`, `clusterHeatmapBody.ts` **V**) shows the pool's own profile.

### Q7 — Tracks and regions, same tools
- **Tracks:**
  - Evidence: the profile (which already includes HMM-state and transition features when the run used them, `TRACK_BEHAVIOUR_AXES_PLAN.md` Decision 6 **V**) plus a medoid filmstrip.
  - `render_medoid_filmstrip` (`api/src/behaviour_cards.jl:77` **V**) and cell cards exist, but the observer reaches them only via a board plot (`server.py:461` **V**).
  - Per-cluster HMM composition has a panel (`frontend/src/modules/cluster/clusterPanels.ts:45` **V**) but no MCP read. `get_behaviour_summary` is per image (**V**).
- **Diffmap:** it **strengthens** pooling rather than replacing it. Leiden clusters are contiguous segments along one path, and the PAGA chain is 0–1–2–3 (`docs/archive/DIFFMAP_TRACK_COMPARISON_RESULTS.md:67-69` **V**). The real hand-made pool `Meandering = 1+2` is an adjacent pair (`:19` **V**).
  - So for tracks, "which clusters go together" becomes "which adjacent path segments". `obs['path.{suffix}']` is a track measure, and adding its per-cluster mean to the profile gives the ordering for free (**A**: not checked whether `path.*` is in the run's features).
  - Cells have no continuum (Decision 1 **V**).
- **Regions:**
  - Evidence: composition (fractions of `basisPops` in the neighbourhood; `clustRegions/cluster.json:36` **V**, plus `labels` in clustfeatures), and a spatial crop of a region pop via cells-image once it takes a `popType`.
  - Region pops are stored like the others (`B__region.json` **V**).
- **One interface:** `cluster_profile(run, popType)` + `cluster_examples(run|pop, popType, mode)`. Profiles are numbers for all three. Examples are crops for cells and regions, and a filmstrip for tracks.

### Q8 — Confabulation guard
- **Store with the pop:**
  - `createdBy` (author_stamp, already there).
  - `evidence: {run fingerprint, ids, top features with z at proposal time}`.
  - A **name rule**: phenotype words must come from the top-|z| feature names ("CD69hi CD8") unless the user supplied the word. Same stance as today's "a NAME is the user's call — propose, never assert" (`server.py:705` **V**).
- **Accept / reject:** the ✦ already renders on cluster-mode rows (same row template over `visiblePops`, `PopulationManager.vue:565,607` **V**).
  - Reject: delete, since there is no undo for cluster pops.
  - Accept: rename, or any edit, which adopts the pop (the Q4 fix).
  - Edit: tick, as today.
- **The minimum that makes a wrong label cheap to catch:** the ✦ tooltip shows the evidence line, and the `dryRun` reply carries a **cohesion number** computed server-side: the largest distance between pool members vs the nearest distance to a non-member. Claude can't fake that number, and a bad pool shows up before anything is written.

### Q9 — Servers and surfaces
- **Both servers:** register the tool once in `register_gating_tools` (`feat/gate-check:mcp/cecelia_mcp/gating_views.py:251` **V**) and add the route to `GATING_WRITE_ROUTES` (`:124` **V**). Following the memory note, a new MCP tool also needs `guidance.py`.
- **Autonomous server:**
  - The project lock is `_request`'s (**V**).
  - The label-set prefix (`_gating_guard`, `feat/gate-check:mcp/cecelia_mcp/autonomous.py` **V**) must hold for **every vn the set-wide write touches**, not just the request's.
  - The measured-segmentation check passes trivially once a run exists.
- **Kiwi:** `add_cluster_pop` goes in `KIWI_EXCLUDED_TOOLS => "write"`. The examples and profile reads go in `KIWI_READ_TOOLS` (`api/src/kiwi_turn.jl` **V**).
- **In-app access table:** in `frontend/src/lib/claudeOverview.ts` TERMINAL_MODE (branch adds "Gates, marked ✦" **V**), change it to "Gates and cluster populations, marked ✦".
- **Recorders:** add `add_cluster_pop` to `GATE_TOOLS` in `scripts/agent_eval/run_record.py`. `is_action` already treats `dry_run` as a look (**V**, branch). `codrive_record._wrote` inherits that (**V**).
- **Guarantees test:** `mcp/tests/test_guarantees.py` covers the allow-list (**A** on exact assertions).

### Q10 — Duplication and fanout
- **Reuse:**
  - `add_pop!`, `_persist_and_broadcast!`, `boolean_dependents`, `author_stamp`.
  - The #1856 helpers, given a `pop_type` argument.
  - `_clustfeatures_entry` / `_clustfeatures_part_of` / `co_clustered_value_names` for the targets.
  - `_cluster_profile` (with a family/pop-type argument) for regions.
  - `_example_picks` / `_render_still` / `_example_sheet`.
  - The distance loop from `medoid_track`, extracted.
- **Second copies that would drift:**
  - **Set-wide targets.** The client's `validUids` (`useClusterContext.ts:124`) vs a new server loop. Make one Julia `cluster_run_targets(img, suffix, family)` and point the client at it later.
  - **Exclusivity.** Client-only today (`PopulationManager.vue:215-228`). Put the owner check in Julia, and let the GUI's `pop/update` use it too or accept a known divergence.
  - **Profile family.** Don't write a second region aggregation; pass `pop_type` to `_cluster_profile`.

### Q11 — Slicing and where it can be checked
1. **Slice 1 (reads, no writes):**
   - Region `profiles` (via `_cluster_profile` + `pop_type`).
   - `popType` on `gating/examples` + `cells-image`.
   - Per-cluster `spread` / `nearest` on profiles.
   - A guidance line.
2. **Slice 1b (#1856 follow-ups, needed whether or not this ships):**
   - Adopt-on-user-edit, so `claude_made` drops after the user changes a pop.
   - Record `source` in clustfeatures and refuse 409 in `set-own-gate` / `delete-own`.
3. **Slice 2 (write, on trigger):**
   - `add-cluster-pop`: create-only, set-wide, unowned IDs only, `dryRun` + cohesion number.
   - `delete-own` generalised.
   - The Q3b run fingerprint.
4. **Slice 3:** heatmap cluster ordering (visible to the user).

- **Headless on UMq4v3: nothing.** `zolIMa/1/UMq4v3` has only `gating/Bcells.json` and `labelProps/Bcells{,__tracks}.h5ad`, with no clustfeatures and no cluster sidecars (**V**, `ls`). Checking there would need a `clustTracks` run, which is a write.
  - Use **4kS67f** instead. It has trackclust `movement` with the named pools Scanning=[0], Meandering=[1,2], Directed=[3]; region runs `default`/`immune`; and a cell run `immune` (**V**).
  - Read-only checks there: region profiles, example sheets on `/Meandering` (trackclust) and on region pops, and the cohesion number on Meandering vs [0,3].
  - Write checks go to a copy under an isolated `CECELIA_DEV_DIR`.
- **Needs a live session:** the ✦ and evidence tooltip in the cluster-mode manager, the set-wide refresh across open images, the stale icon, and judging whether the example sheets look right.

## Open decisions only Dominik can make
1. **Is the ✦ write wanted at all, or only reads plus chat proposals?** *Pick:* reads now; the write waits for a ≥10-cluster dataset.
2. **Pool names:** may Claude use cell-type words (for example "Tregs"), or only feature-derived names until the user renames? *Pick:* feature-derived only.
3. **Adoption:** does any user edit of a ✦ pop make it the user's (so Claude loses own-only access)? *Pick:* yes. It also fixes #1856's gates.
4. **Re-run on the same suffix with named pops present:** warn only, or block the run? *Pick:* warn, with the stale icon on the pops (Dominik's rule: repair, don't warn, so offer "re-map by profile" later).
5. **Server-side exclusivity for GUI ticks too?** *Pick:* yes, one owner check for both paths.

## Top 3 ways it goes wrong, and the test that catches each
1. **A re-run renumbers IDs, and a Claude-named pool now covers different cells.** *Test (api):* fixture run + pop with fingerprint → permute the `clusters.{sfx}` codes → assert the popmap reply flags the pop as stale. Watch it fail on main first, where nothing flags it.
2. **The set-wide write is half-done, or it steals an ID.** *Test (api):* two-image `partOf` fixture where a user pop on image B owns ID 2 → `add-cluster-pop ids=[1,2]` returns 409 and both sidecars are byte-identical afterwards. A second case: B's sidecar is unwritable → nothing is written on A.
3. **A confabulated name is accepted unread, or Claude overwrites a pool the user edited.** *Tests:* (a) api: the user `pop/update`s a ✦ pop → `delete-own` returns 403 `not_claude_made`. This fails on #1856 today. (b) agent_eval probe on 4kS67f: ask Claude to pool the `movement` run, then assert that every name term is a top-|z| feature (or the user's word) and that the cohesion margin of each proposed pool is positive.
