> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. The
> claim-time side of this question lives in [`docs/todo/IMAGE_ANALYSIS_RECITAL_PLAN.md`](../todo/IMAGE_ANALYSIS_RECITAL_PLAN.md).

# RECITAL_IMAGE_CHECKS_AUDIT_PROMPT

Audit task. Read-only: do not modify code, config or docs. Write findings to `RECITAL_IMAGE_CHECKS_AUDIT.md` at the repo root.

## Why

Recital's `result_review` (PR #1921) reviews diffs for silent wrong results and writes a failing test as evidence (`reproduced`), or reports `inferred` if it cannot. I want to know what a recital check specific to **image analysis** would look like, and how much of it already exists in Feijoa (Julia + Vue). The families below come from my own design sketch, not from reading the code. **Treat them as hypotheses. Check against the actual repo and correct me.**

## Proposed check families

| Family | Example check | Catches |
|---|---|---|
| Metamorphic | Flip, transpose or shift the image; outputs transform the same way | Axis-order bugs, off-by-one, edge handling |
| Synthetic ground truth | Render N spheres/particles with known positions and speeds; check counts, centroids, track velocities within tolerance | Missed or merged objects, wrong tracking |
| Units | Change pixel size or time interval; physical outputs scale exactly | µm/px, Δt, anisotropic-z mistakes |
| Conservation | Label count before/after cleanup minus removed equals after; population counts sum to parent; no duplicate IDs per frame | Silent drops, double counting in gating or merging |
| Channel/ID integrity | Permute channels; result follows the permutation | Channel swaps, wrong population assignment |
| Tiling (patchworks-type) | Segment whole vs tiled and merged; compare labels | Seam artefacts, merge errors |
| Provenance | Parameters recorded in history match what actually ran | Stale or wrong parameters in chains |
| Golden regression | Example dataset outputs match stored values within tolerance | Drift after refactors |

Two modes:
1. **Code-time:** triggered by diffs touching segmentation, cleanup, tracking, gating or clustering. The reviewer picks relevant families and writes tests on small synthetic data.
2. **Run-time:** after a real run, check outputs against invariants that need no ground truth (conservation, ID integrity, units present, sane size and speed distributions) and flag outliers.

## Tasks

1. **Coverage map.** For each family, find existing tests, fixtures, QC measures, assertions or validators in the repo (Julia tests, R/legacy tests if still present, CI, per-task QC, `segment-cleanup-qc` work). Mark each: COVERED / PARTIAL / ABSENT, with `path:line` evidence. Say what exactly is checked and what tolerance is used.
2. **Fixtures.** Is there any synthetic image or tracking generator (spheres, particles, drifting objects, known-label volumes)? Where, how realistic, how reusable by a reviewer agent? If none, say what the smallest useful one would be and where it would live.
3. **Pipeline stage map.** For each stage (import, segmentation, cleanup, tracking, population/gating, clustering and behaviour, export/figures), list which families apply and which are cheapest to implement. Include units handling and axis conventions (T/Z/C/Y/X order, anisotropic voxels) as specific risk points.
4. **Past incidents.** Search git history and PRs for bugs that were silent wrong results (e.g. fix commits mentioning axis, unit, off-by-one, label, ID, channel, offset, scaling). For each, name which family would have caught it and whether any existing test did. This is the evidence base for prioritising families.
5. **Recital integration.** Read how `result_review` is invoked and what it receives. Answer:
   - How would a reviewer pick families from a diff? Is there a path-to-stage mapping today?
   - Could it run synthetic-data tests in the existing test harness within its time and cost budget (about 70 s, about $0.87 per run as I recall; verify)?
   - What changes to the prompt, hooks or test layout are needed, and how big are they?
6. **Run-time mode.** What per-task QC measures exist today? Which invariants could be checked after a real run without ground truth? How does this relate to the planned Segment/Cleanup QC agents, and does it duplicate them?
7. **Protocol link.** Which synthetic tests could double as an "installation check" in the Nature Protocols paper (user runs them to confirm the install works on their machine)? List candidates and their runtime.
8. **Build order.** Rank families by value/effort given the findings, with file-level scope for the top 3.

## Output format

`RECITAL_IMAGE_CHECKS_AUDIT.md`:

1. Summary (10 lines max): what exists, what is missing, top 3 recommendations.
2. Coverage table: family | status | evidence | gaps.
3. Fixtures inventory.
4. Pipeline stage map.
5. Past incidents table: incident | commit/PR | family that would catch it | caught by existing test?
6. Recital integration findings and required changes.
7. Run-time mode findings.
8. Protocol installation-check candidates.
9. Build order with scope and effort.
10. Open questions for me.

## Rules

- Cite `path:line`, commit or PR for every factual statement about our code.
- If something cannot be found, write NOT FOUND. Do not infer it exists.
- Do not trust my family list. Add, merge or drop families if the code and history say so.
- Do not trust the PR description over the code; if they disagree, report both.
- No new features beyond what the evidence supports.
- Keep it terse. No filler.
