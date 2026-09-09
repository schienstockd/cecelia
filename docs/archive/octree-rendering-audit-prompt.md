# Octree Rendering — Audit Only

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

## Goal

Decide whether an octree (or any hierarchical/sparse spatial structure)
would meaningfully help the volume renderer, given what the current
brick scheduler actually struggles with — not as a green-field "is
octree better" question. Output is a recommendation + evidence, not code.
Implementing anything is explicitly out of scope for this task.

## Why this is being asked

Current design (fixed-grid bricks + page table, per-frame SSE level
pick, `brickThr` intersect cap) has known open problems:
- f8gzA2: 200ms drawP95 regression, still unresolved, tied to large
  single-level XY volumes routing into bricks mode
- Brick size (128×128×min(brick_z,nZ)) never locked — flagged
  "not a locked decision yet" in `KILN_BRICK_PLAN.md` Open Question #2
- `brickThr` tuned against two reference screenshots, not a cost model

Before reaching for a new data structure, need to know: are these
tuning/calibration gaps in the current scheme, or actual structural
limits that only a hierarchical/sparse structure would fix?

## What to actually check

1. **Sparsity in real data.** Pull a few representative stores
   (SispLk, Dml3RG, fXgbTl, and at least one from an actual
   intravital/immunology acquisition, not just the bench set) and
   measure: what fraction of the volume (per channel, per level) is
   empty/background vs. tissue-occupied? Octree's core value is
   skipping empty space — if these volumes are mostly full (thin,
   dense tissue slabs, which is the expected shape for intravital
   microscopy), that value doesn't exist here regardless of how well
   an octree is implemented.
2. **Where does per-frame cost actually go today** in the f8gzA2
   pathology and similar large-XY cases — intersect-list computation,
   fetch/network, page-table upload, or shader sampling? An octree only
   helps the categories it actually addresses (empty-space skip,
   mixed-LOD-per-frame); if the cost is dominated by something else
   (e.g. raw fetch bandwidth of resident tissue, not wasted background
   sampling), octree doesn't fix it.
3. **Does the workload have genuine near/far depth variation** — i.e.
   would a single frame benefit from sampling different LODs at
   different depths simultaneously? Check actual camera/view usage
   patterns in the viewer (typical zoom range, whether users tend to
   view roughly perpendicular to a flat tissue slab vs. oblique/deep
   angles). If views are mostly near-orthogonal to a thin volume, mixed
   per-frame LOD isn't buying much over current per-frame-uniform SSE
   level pick.
4. **Cost of adding it.** Rough estimate of what an octree would
   actually require on top of the current architecture: hierarchical
   page table vs. flat, parent/child streaming consistency, tree-walk
   per ray-step vs. current flat lookup, and how much of the existing
   brick/atlas/scheduler code would need to change vs. be replaced.
   Weigh this against findings from 1–3 — don't produce this in
   isolation as a "here's what it'd cost" without the payoff side.
5. **Would it help mitigate the currently-open bugs** — specifically
   f8gzA2, and the "no LOD fallback → blank frame on zoom" issue from
   the original P5 bricking commit. Direct answer, not just plausible.

## Output

A short writeup with:
- Sparsity measurements (numbers, not impressions) across the sampled
  stores
- Where per-frame cost actually goes today, for the pathological case
- A clear yes/no/conditional recommendation on whether octree is worth
  pursuing, with the reasoning tied to 1–4 above, not architecture
  aesthetics
- If conditional: what would need to be true (e.g. "if future stores
  are much sparser than what we sampled") for the answer to flip
- Explicitly separate: "this would help X open bug" claims need to
  point at a specific mechanism (e.g. "skips N empty bricks currently
  fetched") not just "more sophisticated LOD structure generally helps"

## Explicitly not in scope

- Any implementation, prototype, or code change
- Retuning `brickThr`/brick size — that's separate, already-tracked work
  (B1 rebaseline, Open Question #2)
- Deciding this before the sparsity/cost-location data exists — don't
  let architectural intuition substitute for the measurement in 1–2
