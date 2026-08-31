> **ARCHIVED — not authoritative, do not act on this.** Audit output for the ask in
> `octree-rendering-audit-prompt.md`. Current renderer design lives in
> `docs/todo/BRICK_INTEGRATION_PLAN.md` (which supersedes `KILN_BRICK_PLAN.md`).

# Octree rendering — audit outcome

**TL;DR — NO.** An octree does not pay for itself against the current brick scheduler on the data
Cecelia actually renders. Two numbers drive the call:

1. **On the four intravital / bench stores, empty-brick fraction is 0 %** at 128×128×z_native
   granularity. Empty-space skip — octree's core value — has nothing to skip.
2. **f8gzA2's 200 ms drawP95 (the audit's motivating regression) is already resolved on
   `feat/brick-followup` B0** — the intersect-list over-fetch guard
   (`MAX_INTERSECT_BRICKS = 256`, coarsen-until-fits) dropped it to **2.8 ms** without any
   hierarchical data structure change (bench5, 2026-08-29, per
   `BRICK_INTEGRATION_PLAN.md` §Decision 2 / B4).

The remaining cost that would justify a hierarchical structure — genuine near/far depth variation
producing meaningful mixed-LOD-per-frame wins — is unmeasured, but structurally implausible for
Cecelia's data (thin XY-heavy volumes) and would be a large architectural cost to buy back.

Conditional flip is documented in §Recommendation.

---

## 1. Sparsity — measured

Source: `/tmp/…/scratchpad/sparsity.py`, opened via `zarr_utils.open_as_zarr` (per project
convention), five stores, first timepoint, up to 5 channels each. Threshold per-channel =
`max(4, p1_nonzero)` — 4 counts of DN floor, or the 1st percentile of non-zero voxels, whichever
is higher. Threshold reported in the table. **Brick-empty** = fraction of 128×128×8 blocks whose
MAX voxel intensity < threshold — that mirrors what a hierarchical scheme could actually skip
(a whole brick, not a voxel).

Voxel-empty is bookkeeping; brick-empty is what matters for octree payoff.

| Store | Shape (T,C,Z,Y,X) | dtype | Level | Channels | thr | voxel-empty % | **brick-empty %** | bricks empty/total |
|---|---|---|---|---|---|---|---|---|
| **SispLk** (IBEX pathology) | 1,4,38,7293,7848 | u8 | L5 (227×245) | c0–4 | 4 | 22–99.6% | **0.0%** | 0/4 |
|  |  |  | L3 (911×981) | c0–4 | 4 | 22–99.6% | **~1.6% (c1 only)** | 5/64 for c1; 0 elsewhere |
| **f8gzA2** (pathological large-XY) | 1,25,1,16898,20329 | u16 | L5 (528×635) | c0–4 | 4–16 | 26–41% | **12.0%** | 3/25 (all channels) |
|  |  |  | L3 (2112×2541) | c0–4 | 4–17 | 26–41% | **21.5%** | 73/340 (all channels) |
| **Dml3RG** (MERTK intravital, OLifi6 set) | 181,4,31,1024,1024 | u16 | L0 (only level) | c0–3 | 20 | 89.6–99.5% | **0.0%** | 0/256 |
| **fXgbTl** (MERTK intravital, OLifi6 set) | 31,4,31,418,434 | u16 | L0 (only level) | c0–3 | 20 | 86–99.5% | **0.0%** | 0/64 |
| **FtGoJO** (UZD4d7 Lymphoid set) | 1,6,26,2024,2024 | u16 | L3 (253×253) | c0–4 | 4 | 8.4–68.4% | **0.0%** | 0/16 |
|  |  |  | L2 (506×506) | c0–4 | 4 | 8.4–68.4% | **0.0%** | 0/64 |

Coverage note: OLifi6 (MERTK) intravital pair = Dml3RG + fXgbTl. UZD4d7 (Lymphoid IBEX) =
FtGoJO + SispLk + f8gzA2 — five stores across the two sets. All stores were reachable; nothing was
substituted or dropped.

**Reading:**

- **Intravital (Dml3RG, fXgbTl):** voxel-empty is very high (86–99.5%), but signal is
  ubiquitous per-block — every brick has at least one cell in it. Brick-empty = 0. This is the
  classic intravital shape (thin distributed cell signal in a soaked-through volume) and octree
  can save nothing here.
- **Bench/lymphoid (SispLk, FtGoJO):** cells fill the tissue slab; both voxel-empty and
  brick-empty are low. Nothing to skip.
- **f8gzA2 alone is meaningfully sparse at the brick level (12% at L5, 21.5% at L3).** It is
  the large-XY IBEX with genuine background regions of empty slide. **This is the one store
  where an octree's empty-space skip would fetch fewer bricks** — but see §5, f8gzA2's real
  bottleneck is not fetch bandwidth.

Threshold sensitivity: the intravital thresholds (max(4, p1) = 20) look high vs the u16 range but
are calibrated to the actual per-channel noise floor (max intensity 129–494 counts on these images).
Loosening to threshold=4 would move voxel-empty by a few points but brick-empty stays 0 — the
determining fact is whether any voxel in a 128×128×z_native block hits the tissue max, and every
block does.

## 2. Where per-frame cost actually goes on f8gzA2 today

**Direct measurement of the split (intersect-list vs fetch vs page-table upload vs shader
sampling) does not exist in the saved bench blobs.** `bench-f8gzA2-brick-2026-08-29_*.json`
records only `drawMs` per frame — the whole `draw()` callback — not the sub-phases.

What is documented in `BRICK_INTEGRATION_PLAN.md` §"What we know now":

- **Flat renderer's `drawP95` = 0.1–0.2 ms across all five stores.** That measures CPU-side
  submission only (no GPU timestamp query in the bench harness). So brick's `drawP95` is not
  render cost — it is `tickScheduler` + intersect-list + `pageTableCpu` upload work happening
  inside `draw()`.
- **f8gzA2 pre-B0: drawP95 = 200.9 ms; drawMedian = 63 ms; frames from a 62 s session, 193
  samples.** From my re-analysis of the raw blob: min = 4 ms; p75 = 110 ms; p95 = 201 ms;
  p99 = 275 ms; max = 371 ms.
- **f8gzA2 post-B0: drawP95 = 2.8 ms** (per `BRICK_INTEGRATION_PLAN.md` §Decision 2 / B4). The
  fix was `MAX_INTERSECT_BRICKS = 256` in `brickScheduler.ts:287–324` — SSE picks a fine level,
  then `guardIntersectCost` walks coarser until the core viewport's brick count fits, and the
  intersect list stops fanning out over a 20 K × 17 K plane.

| Cost bucket (f8gzA2 pre-B0) | Measured? | Evidence |
|---|---|---|
| Shader sampling (raw GPU render) | **no** | bench harness has no GPU timestamp query; flat's 0.1 ms baseline is CPU submission |
| Intersect-list computation | **implied dominant** | B0 shrunk intersect list; drawP95 dropped 200 → 2.8 ms |
| `pageTableCpu` upload | **partially** | pageTable is rebuilt on level swap (`brickVolumeRenderer.ts:524, 737`); a smaller intersect list also means a smaller resident set and fewer rebuild triggers |
| Fetch / network (`bytesFetched`) | **yes** | 2.85 GB fetched over the pre-B0 session; but fetch is off the main thread and doesn't show up in `drawMs` directly |

**What is unknown vs the current renderer:**

- Post-B0 breakdown of the residual 2.8 ms drawP95 across submission / pageTable upload /
  scheduler work.
- Whether the residual GPU sampling cost — never measured on either renderer — sits above or
  below the audit's 5.3 ms MIP budget on f8gzA2 shape.

To split them properly needs a WebGPU timestamp query pass (`GPUQuerySet` +
`timestamp-query` feature) around `computePassEncoder.end()` / `renderPassEncoder.end()`,
plus a CPU-side breakdown of `tickScheduler` vs `writeBuffer(pageTableBuffer)` in
`brickVolumeRenderer.ts`. Not necessary to *decide* the octree question — the intersect-list
guard already brought drawP95 into the interactive range without a new data structure — but
necessary to *predict* what an octree would remove.

## 3. Near/far depth variation — does the workload use mixed LOD?

**Structurally the current shader picks ONE LOD per frame.** `brickShader.ts:159–211`
(`atlasSample`) looks up in one page-table (`pt`) with a `prevPt` fallback ONLY for holes not yet
fetched at the target level. The scheduler `pickBrickLevel` (`brickScheduler.ts:181–200`) picks a
single level from SSE math (`log2(distanceUm / (voxelSizeUm * focalPx))`), takes the finer of xy
and z desired levels for anisotropic Z, then clamps to the floor and coarsens for over-fetch. There
is no per-brick level in the current design.

Adding per-brick LOD (the octree ask) would require:

- A hierarchical page table (parent/child slot pointers), or per-brick level tags read from `pt`
- A tree walk in the ray-step (currently one `pt[idx]` load per step; would become up to
  `log2(nLevels)` chained lookups until a resident brick is found)
- A residency policy that can hold parent + child slots concurrently, not the current one-level
  atlas

**Would it earn its cost on Cecelia's data?**

- The intravital stores are `nZ ≈ 31` and thin (physical Z half-depth ~30 µm × few voxel_z). Near
  vs far ratios in a single frame span roughly 1:2 to 1:5 — one octave of LOD gap at most, and
  the SSE picker already covers that with one-level-per-frame + zoom-triggered swaps.
- The two multi-level pathology stores (SispLk, f8gzA2) are XY-heavy with nZ ∈ {4, 1}. Depth
  variation within a frame is essentially zero — the frustum spans the whole thin slab. Mixed
  per-depth LOD buys nothing structural.
- The wide-XY case (f8gzA2 fit distance) is *within-frame LOD contention across XY*, not depth.
  That's exactly what `guardIntersectCost` already solves without a hierarchy — pick one level
  that fits the intersect budget across the visible extent.

Camera-pattern reasoning (not measured usage stats): Cecelia's viewer defaults to
near-orthogonal to the sample slab (`VIEW_HALF_ANGLE`, standard raycast basis in
`brickShader.ts:139–150`); mixed-LOD-per-frame only pays when a frustum spans a wide near-far
range, which is a deep-Z + strong perspective pattern the viewer doesn't produce.

## 4. Cost of adding octree — grounded in the current code

What today's brick renderer already carries (kilnbrick worktree, `feat/brick-followup`):

- Physical brick atlas + slot LRU (`brickAtlas.ts`, `brickAtlasTexture.ts`)
- Flat page table (`pageTable.ts`, `pageTableCpu: Uint32Array` in
  `brickVolumeRenderer.ts:102`) — single-level indirection
- SSE + hysteresis LOD picker per frame (`sseLod.ts`, `brickScheduler.ts:181–200`)
- Halo prefetch (Chebyshev ring), core-vs-halo distinction for the over-fetch guard
- Prev-level fallback in the shader (`brickShader.ts:174–197`) — hole-fill only, not per-depth
  LOD

What an octree would add (concrete, not aesthetic):

- **Hierarchical page table.** Parent/child pointers instead of a flat grid. `pageTableCpu` grows
  from `gridNxL0 * gridNyL0 * gridNzL0` × 4 B to a tree with children pointers; per-level upload
  and dirtying discipline (`brickVolumeRenderer.ts:524, 737`) all need rework.
- **Parent/child streaming coherence.** The residency layer today evicts one slot per LRU;
  hierarchical residency has to keep parents around when children are drawn from them, or a
  child's parent gets evicted mid-frame and the ray-step falls off the tree. The LRU-clobber fix
  (#702) that dropped Dml3RG's atlas thrash addresses one level; adding parent-preserve
  constraints reopens that class of bug.
- **Tree walk per ray-step.** Today: one `pt[idx]` load per step. With octree: up to
  `log2(nLevels)` chained loads until a resident node is found, with divergent branches across
  neighbouring pixels — WGSL uniform-control-flow discipline gets harder. On a raycast that
  already ships prev-level fallback (`brickShader.ts:174–197`) at ~40 lines of shader, this
  roughly doubles the sampling section.
- **Scheduler.** `pickBrickLevel` picks one level per frame; a per-brick picker means walking
  the visible box at multiple levels and materialising a mixed-level intersect list. The
  over-fetch guard (`MAX_INTERSECT_BRICKS`) that currently gates one level's fan-out has to
  become a budget across a level *distribution*.

Rough shape: **weeks of design + build + regression-hunt** across scheduler, atlas, page table,
shader, and bench harness, against a payoff that §1–3 say is near zero for the data we render.

## 5. Does octree fix the two open bugs?

### f8gzA2 200 ms drawP95 regression

**No — because it's already fixed by a non-hierarchical mechanism.** The B0 intersect guard
(`MAX_INTERSECT_BRICKS = 256`, coarsen-until-fits, `brickScheduler.ts:309–324`) shrank the
intersect list at wide zooms and drew drawP95 down to 2.8 ms without changing the data structure.
An octree could plausibly do the same by picking a coarse level for the "far half" of the
viewport, but it addresses the same symptom (intersect list too big at the SSE-desired level) with
much more moving code. The mechanism that would fire — "sample a coarser brick at large SSE
distance" — is what the flat scheduler *already does* at frame granularity.

### "No LOD fallback → blank frame on zoom" (P5 bricking)

**No, because the current renderer already has a prev-level fallback in the shader**
(`brickShader.ts:174–197`) and a bootstrap-to-floor + Frankenstein hole-fill in the scheduler
(B0 / B0.5, `BRICK_INTEGRATION_PLAN.md`). Blank-on-zoom is a fetch-latency + page-table
coherence problem, not a data-structure-choice problem. Octree would give richer fallback
(sample any ancestor), but the current one-level-back path already covers the visible zoom step
in practice (fixed 2026-08-29 with the LRU-clobber + prev-level touch bias + page-table rebuild
on level swap).

## Recommendation

**NO — do not pursue an octree/hierarchical data structure at this time.**

Reasoning tied to §1–4:

- §1: the empty-space-skip payoff is 0% on 4/5 sampled stores; only f8gzA2 has meaningful
  brick-empty fraction (12–21%), and f8gzA2's problem is not fetch bandwidth. Intravital data
  has 86–99% *voxel*-empty but 0% *brick*-empty — the geometric shape of the signal defeats
  block-level skip.
- §2: f8gzA2's cost location is the intersect-list, already addressed by `MAX_INTERSECT_BRICKS`
  in the current flat page table. No hierarchy needed to fix the audit's motivating regression.
- §3: mixed-LOD-per-frame is structurally not what Cecelia's viewer produces (thin slabs,
  near-orthogonal camera). The one within-frame LOD problem (f8gzA2 wide zoom) is already
  handled by coarsen-until-fits at frame granularity.
- §4: implementation cost is high — hierarchical page table, streaming coherence, shader tree
  walk — against a payoff §1–3 say is small.
- §5: both named open bugs are addressed by the existing mechanism, not by a data structure
  swap.

### Conditional flip

The recommendation would flip if any of these become true:

1. **Future stores are much sparser at the brick level.** Not "sparser voxels" — sparser
   *blocks*. If Cecelia takes on cleared-tissue whole-organ light-sheet or lattice light-sheet
   with vasculature-only staining and brick-empty rises above ~50%, the empty-skip win
   materialises. Threshold to re-run the check: any new store type with dtype ≥ u16 and
   voxel-empty > 95%, measure brick-empty before deciding.
2. **Deep-Z volumes with strong perspective become the norm.** If nZ starts routinely exceeding
   nXY (e.g. > 1024 z-planes at < 1024 xy), the frustum spans a genuine near/far range and
   mixed-LOD-per-frame becomes value-add rather than symmetry-breaking. The kilnbrick plan
   already anticipates this ("not a Z-streaming device") — octree becomes worth a look when
   that assumption stops holding.
3. **The post-B0 f8gzA2 residual (2.8 ms drawP95) turns out to be dominated by a hierarchical
   fetch pattern.** Requires the timestamp-query bench pass in §2. If the split shows
   `pageTableCpu` upload is > 1 ms and the upload grows with the intersect list, a hierarchical
   page table might amortise it. That's a big *if*.

## What this audit is NOT

- Not a recommendation to retune `brickThr`, `MAX_INTERSECT_BRICKS`, or brick size (128 × 128 ×
  brick_z) — those are separate open questions in `BRICK_INTEGRATION_PLAN.md` §Open questions.
- Not a claim the current renderer is at 5.3 ms MIP budget on f8gzA2 — perf ledger is pending in
  `BRICK_INTEGRATION_PLAN.md` B5. This audit only argues that closing that gap doesn't need
  octree.
- Not a green-field "is octree better in the abstract" — it is worse *for Cecelia's actual data
  and viewer*.

## References

- Prompt: `docs/archive/octree-rendering-audit-prompt.md`
- Renderer plans: `cecelia-kilnbrick/docs/todo/KILN_BRICK_PLAN.md`,
  `cecelia-kilnbrick/docs/todo/BRICK_INTEGRATION_PLAN.md`
- Bench blobs: `~/Downloads/TMP/bench/bench-{image}-{mode}-2026-08-29_*.json`
- Sparsity data: scratchpad `sparsity.py` / `sparsity.json` (per-store per-channel numbers)
- Code: `frontend/src/utils/brickScheduler.ts`, `frontend/src/utils/sseLod.ts`,
  `frontend/src/lib/webgpu/brickVolumeRenderer.ts`, `frontend/src/lib/webgpu/brickShader.ts`
