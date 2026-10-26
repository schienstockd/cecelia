> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run.
>
> **Outcome:** plan + P1 on branch `feat/roi-triage` (2026-10-26) → [`docs/todo/ROI_TRIAGE_PLAN.md`](../todo/ROI_TRIAGE_PLAN.md).
> Built in Feijoa as a viewer side-rail section, not a separate panel; no `roi_generate` MCP tool (MCP
> cannot start tasks); `unsure` added to the label types; nuc/mem are polygons in one TOM×GFP plane,
> not single-channel gates; the macrophage mask is paired with a mask-free density cue because a plain
> CD169 threshold misses dim TBMs. The brief said "plan before implementing"; the user asked for both
> in one pass.

# Opus task: design and audit ROI auto-discovery + Claude triage for B-cell fragments

You are working in the Cecelia repo (`schienstockd/cecelia`). Read `CLAUDE.md` and follow its conventions. This is a **brainstorm + audit + plan** task first. Do not implement until the plan is reviewed.

## Problem

Intravital 3D time-lapse of B cells and macrophages. We cannot reliably track the many small green/magenta fragments. **Segmentation is done and accepted as-is; do not audit, question, or propose changes to it.** Goal: **automatically find regions of interest (ROIs) where something interesting happens to fragments** (engulfment, blebbing, fragmentation, uptake), then have Claude triage those ROIs from images + structured data, so the user reviews only the top few.

Claude is **not** the tracker and **not** a coordinate source. It labels pre-computed candidates.

## Data facts

- 3D + time. Meshes available (`volume_mesh`). Projections can't distinguish "inside" from "overlapping", so contact/engulfment ROIs need z-slices or orthogonal views.
- Channels: green = B-cell nuclear marker (`nuc`), magenta = B-cell membrane marker (`mem`), yellow = macrophage marker. CD169 (`CD169-Kat`) is the macrophage intensity measure.
- Segmentation is B cells only. Macrophages appear as objects only through channel spillover (the CD169-high objects in the volume_mesh vs CD169 plot).
- Populations (gated via the Populations panel):
  - `/qc` → `cells`, `frags`
  - `/qc/frags` → `nuc`, `mem`, `small`, `big`
  - Gate features seen: `volume_mesh`, `solidity`, `ellipticity_interm_oblate`, `CD169-Kat`
- Existing tooling: `get_landscape`, viewer MCP (`seek_viewer`, `viewer:mark`, `select_on_plot`), Kiwi assistant.

## What Claude can and cannot do (design around this)

- Good: qualitative reading of crops (engulfed vs free debris vs intact, change between frames), explaining reasoning.
- Weak: precise pixel coordinates, counting many small dots, identity across frames.
- Therefore: **numbers come from the pipeline, judgement comes from Claude.** Always pass structured data with images and let Claude cross-check one against the other.

## Proposed design to audit (challenge it, don't just accept it)

### 1. Macrophage mask
Evaluate adding a dedicated macrophage mask (CD169 / yellow channel, thresholded, 3D) rather than relying on spillover objects.
- Options: threshold + 3D morphology; build from existing CD169 per-object intensity; separate channel/model.
- Decide what is simplest and robust. Use it for distance-to-macrophage and containment features instead of macrophage object tracking.
- Report which of these already exist in the codebase.

### 2. Candidate features (per object, per frame, no tracking)
- distance to nearest macrophage mask (3D), fraction of surface in contact or inside
- parent `big` object (containment/contact in 3D) and its ID
- `nuc` without `mem` (naked nuclear fragment); `mem` without `nuc`
- local fragment density and its change between neighbouring frames
- morphology: `volume_mesh`, `solidity`, `ellipticity_interm_oblate`

### 3. ROI discovery
- Cluster high-scoring objects in (x, y, z, t), e.g. DBSCAN or a coarse 4D grid. Output box + time window.
- Hotspot types: **birth** (frags appear next to a big cell), **contact** (frag against macrophage mask), **loss** (frag disappears next to macrophage mask), **turnover** (high local change, no obvious partner).
- Rank ROIs. Make scoring weights user-adjustable ("feedback knobs").

### 4. ROI package sent to Claude
Per ROI:
- **Image (PNG, exported directly, no UI chrome):** rows = 5-6 timepoints with time printed, fixed contrast across rows. Columns = composite raw, green, magenta, CD169/yellow, composite with thin class-coded contours and label IDs. Nearest-neighbour upscaling, 200-400 px crops, scale bar, coordinate grid/ticks, legend. For contact/engulfment ROIs add a z-slice or orthogonal (xz/yz) view at the object centroid.
- **JSON:** ROI id; box in px and µm; frames; pixel size; frame interval; object table (label ID, population, centroid, `volume_mesh`, solidity, ellipticity, per-channel means, CD169, distance to macrophage mask, parent `big` ID, triggering scores); minimap position.

### 5. Claude output (structured, per ROI)
```json
{
  "roi_id": "",
  "type": "engulfment | free_debris | bleb_fragmentation | dividing | artifact",
  "confidence": 0.0,
  "object_ids": [],
  "frames": [],
  "reason": "one line"
}
```

### 6. GUI integration: ROI list panel (co-driving surface)

Add an **ROI list panel** to the app, modelled on the existing task runner list and movie list (reuse their components, state patterns and styling; audit them first and name the files).

- **List:** one row per ROI with thumbnail, type, rank/score, time window, status (`pending | claude-labelled | accepted | rejected | flagged`), Claude's label + confidence + one-line reason, and the user's own label/notes.
- **Controls:**
  - Generate / regenerate ROIs (run discovery for the current set/population, with progress like the task runner).
  - Prev / next ROI, jump to ROI (viewer seeks box, z and time window via `seek_viewer`; marks objects via `viewer:mark`).
  - Run Claude triage on one ROI, on the selection, or on all pending.
  - Accept / reject / relabel Claude's label; add a note; flag for review.
  - Filter and sort by type, status, score, confidence, disagreement between user and Claude.
  - Scoring-weight knobs and population selector (`/qc/frags`, `nuc`/`mem`, `small`/`big`) that trigger regeneration.
- **Shared state:** the ROI list is the single source of truth for both user and Claude. Claude reads and writes it through MCP tools (e.g. `roi_list`, `roi_get`, `roi_goto`, `roi_label`, `roi_generate`), not by scraping the screen. Define the tool schemas.
- **Co-driving flow:** Claude proposes labels; the user confirms or overrides; Claude can navigate the user to the next unreviewed ROI and explain why it matters. Guardrails: Claude cannot mark an ROI `accepted` itself, cannot delete ROIs, and every change is attributed (user vs Claude) and reversible.
- **Persistence:** ROI sets saved with the image/set so reviews survive sessions. Store ROI definitions, scores, labels (with author + timestamp) and the generation parameters, so results are reproducible.
- **Export:** CSV/JSON of accepted ROIs, and feed accepted/rejected labels back into the validation protocol.
- Check how this fits the Julia + Vue rewrite (pineapple) versus the current R/Shiny app, and say where to build it first.

## Deliverables (write to `docs/plans/ROI_TRIAGE_PLAN.md`)

1. **Audit:** what already exists (landscape, viewer MCP, gating, mesh features, macrophage handling, montage/export code), with file paths and PR numbers. What is missing.
2. **Realism check:** which parts are feasible now vs risky. Be specific about 3D handling cost, mask quality, and viewer export constraints.
3. **Prior art:** short review of existing approaches to phagocytosis / apoptotic body / efferocytosis detection in intravital data and of LLM- or VLM-assisted image triage. Cite sources; mark anything unverified.
4. **Error modes** the triage loop must detect and self-correct, excluding segmentation (e.g. spillover-driven false macrophage calls, projection ambiguity, contrast drift between frames, hallucinated counts). For each: how it is detected, what the agent does.
5. **Co-driving model:** how this fits semi-autonomous Claude working alongside the user (Claude proposes ROIs and labels, user approves; use `viewer:mark` / `seek_viewer` to jump to ROIs; guardrails and feedback knobs). Not fully autonomous runs.
6. **Validation protocol:** hand-label ~30 ROIs, run Claude triage, report agreement per type and per confidence bin. Define the acceptance bar before scale-up.
7. **GUI + MCP design:** ROI list panel layout, reused components, state model, MCP tool schemas, persistence format.
8. **Staged implementation plan:** smallest first slice that gives the user useful ROIs (suggest: generate + list + prev/next + seek, then Claude labelling, then knobs), with PR-sized steps, tests, and recital checks per step.

## Rules

- Do not assume `nuc` = green-only or `mem` = magenta-only. Check the gating definitions in code and report them.
- Do not invent column names. Read the actual object table schema and list it.
- Mark every claim as **verified in code**, **inferred**, or **unknown**.
- Flag where the design above is wrong or over-engineered, and propose the simpler alternative.
- No implementation, no refactors, until the plan is approved.
