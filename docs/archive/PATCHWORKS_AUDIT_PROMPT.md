> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> segmentation design lives in [`docs/SEGMENTATION.md`](../SEGMENTATION.md); what we take from
> patchworks is in [`docs/todo/PATCHWORKS_IDEAS_PLAN.md`](../todo/PATCHWORKS_IDEAS_PLAN.md).
>
> **Outcome:** not run as written. Archived 2026-10-29. No `docs/audits/PATCHWORKS_AUDIT.md`; a source
> read of patchworks went into the plan instead.
> Several premises are out of date:
> - Cecelia already tiles segmentation and stitches seams. Both tiles predict the overlap band and
>   a cut cell is matched by IoU (`_stitch_tile_seams`, `SEGMENTATION.md` → tile seam stitching).
>   That is the overlap gate the merge concern asks about.
> - Cecelia is itself GPL-3.0-or-later, so the MIT-vs-GPL question is smaller than it reads here.
> - `INVENTORY.md` is now an index; the family detail lives in `docs/inventory/*.md`.
> - "IT mandated cloud-only compute" no longer holds: there is also a lab GPU box.

# Patchworks Audit (Opus)

**Mode: read-only audit. Do not change code in either repo. Output is one report file.**

Audit `imcf/patchworks` against Cecelia. Decide what, if anything, we adopt, and what we learn from how patchworks was built with LLMs.

## Setup

```bash
git clone https://github.com/imcf/patchworks /tmp/patchworks
cd /tmp/patchworks && git log --oneline | wc -l   # expect ~711 commits
```

Cecelia is the current repo. Read `CLAUDE.md` and `INVENTORY.md` first, per the discovery-first rule.

## Background (from a README/PyPI pass, source NOT yet read)

patchworks: "Tiled processing of arbitrarily large images, any image, any function." Python. Tiles an image, runs `fn(tile) -> labels`, merges to globally consistent labels, stores everything in one OME-Zarr. Treat everything below as claims to verify against the source.

- **Merge algorithm (claimed):**
  1. Stage each tile's labels to a temp Zarr exactly once (avoids 3-4x recompute of `fn`, e.g. Cellpose).
  2. Scan thin slabs at tile borders for touching label pairs.
  3. scipy connected components on the pairs gives a relabel LUT.
  4. Apply the LUT to all tiles in parallel.
  - Zarr-native, no dask task graph.
- **Other features:** empty-tile skipping; `tile_shape="auto"` sized from RAM/VRAM; X/Y-only pyramids with Z full-res, built level by level from disk; labels at `image.zarr/labels/labels`; object tables and relations between label images (nucleus -> cell); napari review queue of likely-wrong objects with corrections written back to tables; Snakemake/Slurm workflow; plugins for Cellpose, PlantSeg, watershed, DoG, Noise2Void.
- **Concerns to verify:**
  - The merge rule only says "touching label pairs". In dense tissue, distinct touching cells across a tile border may be wrongly merged unless there is an overlap/IoU gate.
  - **Licence:** older PyPI releases say MIT, the GitHub repo now says GPL-3.0. Establish the actual history from the git log and LICENSE.
  - **Stability:** version numbering looks inconsistent across PyPI listings.
  - **Provenance:** the README states large parts were vibe coded.

## Cecelia context

- Julia backend (Cecelia.jl), Vue 3 frontend. Data: OME-Zarr + H5AD. Python retained for deep-learning inference.
- Zarr.jl reads measured 2.1x faster than Python zarr (PR #649).
- Segmentation: Cellpose pinned to 4.2/Cellpose-SAM; `coastal` (self-supervised, optical-flow UNet) is the default for live-cell intravital.
- IT mandated cloud-only compute.
- Design principle: Cecelia must function fully without Claude.
- Existing planned work that may overlap: population manager, spatial analysis, correction/QC wizard, decision-tree for correction sequences.

## Phase 1: Audit patchworks (source)

Answer with file:line references.

1. **Merge:** read the merge/stitching implementation. What exactly makes two border labels "the same object"? Is there an overlap/IoU threshold, a minimum contact area, or a size check? Construct (on paper or in a scratch test outside both repos) a case where it merges two distinct touching cells. Does the Known Pitfalls table or the tests cover it?
2. **Staging and ID scheme:** block-encoded IDs, sequential relabel, memory profile of the LUT at high object counts.
3. **Empty-tile skipping and auto tile sizing:** the heuristics used, and their failure modes.
4. **Pyramid builder:** correctness of X/Y-only downsampling for label images (must be nearest/mode, not averaged).
5. **Relations and object tables:** schema, how relations between label images are computed, how corrections flow back.
6. **Review queue:** how "most likely to be wrong" is scored.
7. **Plugin contract:** signature, dtype/shape conventions, what a Julia or PythonCall-wrapped function would need to satisfy.
8. **Quality signals:** run the tests if they run cheaply. Compare the `benchmarks/` claims against what the code does. Flag dead code, duplicated logic, and anything that reads as unreviewed LLM output.

## Phase 2: Audit Cecelia (overlap and gaps)

1. Does Cecelia already have tiled segmentation or label merging anywhere (Julia or Python)? Where?
2. What happens today for an image too large for memory at the segmentation step?
3. Where are label relations (e.g. nucleus -> cell) currently computed and stored, and could the patchworks relation model replace or inform it?
4. Does Cecelia have any equivalent of a likely-wrong review queue? How does it relate to the planned QC wizard?
5. Check `INVENTORY.md` "Families" before proposing any new module, so we do not create a parallel implementation path.

## Phase 3: Adopt / adapt / skip

For each candidate item produce a row:

| Item | Adopt as dependency / Reimplement in Julia / Reimplement in Python / Skip | Effort (S/M/L) | Risk | Where it lands in Cecelia | Why |

Constraints:
- Adopting patchworks as a dependency requires an explicit **licence analysis**: MIT vs GPL-3.0 history, and what GPL-3.0 would mean for Cecelia's own licence when called via PythonCall or as a subprocess. State uncertainty, do not give a legal conclusion.
- Default preference is reimplementing ideas natively (Zarr.jl) over adding a young dependency.
- **Attribution:** for every idea or code reused, specify the exact attribution to add (file header, `NOTICE`/credits entry, docs mention), following how we handled TrackMeasures.jl / celltrackR attribution. No verbatim code copying until the licence question is settled; clean-room reimplementation from the described algorithm is the fallback.
- If nothing is worth adopting, say so plainly.

## Phase 4: LLM-usage forensics

patchworks discloses heavy LLM authorship (711 commits). Mine the repo for how it was done and what transfers to our Claude Code workflow.

Look at:
- `git log`: commit message style, co-author trailers, commit size distribution, churn on the same files (rework hotspots), bug-fix commits following feature commits, revert patterns.
- Presence and content of any agent config (`CLAUDE.md`, `AGENTS.md`, `.cursorrules`, `.github/` instructions or prompts) and plan/spec docs in `docs/`.
- Guardrails: `.pre-commit-config.yaml`, `.markdownlint-cli2.yaml`, `cliff.toml` (changelog generation), CI in `.github/`, test coverage shape, benchmarks.
- The "Known pitfalls" table: do the entries look like LLM failure modes that were turned into documented rules and tests? (e.g. the 3-4x recompute.)
- How the vibe-coding disclosure is worded and what it promises users.
- Evidence of drift: inconsistent versioning, README vs PyPI vs repo disagreement (licence), duplicated helpers, hand-rolled replacements for existing libraries.

Then compare to our workflow (`CLAUDE.md` discovery-first rule, `INVENTORY.md`, phase prompts, Opus-plans/Sonnet-executes, PR review for component drift) and answer:
1. What did they do that prevents drift or regressions that we do not?
2. What failure modes visible in their history are we exposed to?
3. Is the disclosure practice worth adopting for Cecelia, and in what form?
4. Concrete changes to our `CLAUDE.md`, CI, or prompt templates. Max 5, each with the evidence that motivated it.

## Output

Write `docs/audits/PATCHWORKS_AUDIT.md` (only this file) with sections: Summary (10 lines max), Phase 1-4 findings, Adopt/adapt/skip table, Open questions for the human. Evidence over opinion: every claim cites a file, commit hash, or line. Mark anything you could not verify as UNVERIFIED.

## Check-ins

Stop and ask before: running anything that installs packages into Cecelia's environment, touching the network beyond the clone, or concluding "adopt as dependency". Re-read this prompt before starting Phase 3.
