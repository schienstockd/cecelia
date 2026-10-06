# Task discovery — what each step is for, and when a problem points back a step

**Status:** in progress — P3 built (2026-10-06); P1, P2, P4, P5 not built. Prompted by the 2026-10-05/06 guide runs
([`GUIDE_RUNS_PLAN.md`](GUIDE_RUNS_PLAN.md)). Builds on the task specs (`app/src/tasks/*/*.json`), the
QC catalogue (`app/src/qc/text.jl`) and the correction-plan engine
([`CORRECTION_QC_PLAN.md`](CORRECTION_QC_PLAN.md), `app/src/correction_plan.jl`).

## Goal

A person, or Claude, with a problem ("the cells come out fragmented", "the background is noisy")
finds which step deals with it, and whether to change parameters at this step or go back and fix an
earlier one.

## Why (from the runs, verified in the transcripts)

All four guide runs struggled with segmentation. They tuned cellpose on noisy images rather than
spending time on cleanup. Run 2 looked at denoising, which makes these images worse because they are
not photon-limited. Nothing told the agents this, and nothing would tell a user either:

- **Every function is listed, none says what it is for.** `get_module_params` and the module
  dropdowns show all 55 tasks with 323 parameter tips. No task spec says what the task is for, when to
  use it, or when not to. "Denoise: photon-limited data only" is written nowhere.
- **The cleanup recommender sees only metadata.** Every run called `recommend_correction_plan`, the
  Cleanup page's engine. It recommended drift correction and nothing else. It already computes a
  photon-limited score (`smooth.photon_limited_frac`, from the import's zero-voxel measure), but no
  rule uses it.
- **Checks speak only on failure.** `import.photon_limited` fires on photon-limited data and says
  nothing otherwise, so "denoising won't help here" is never said.
- **Segmentation has no QC findings.** There are findings for metadata, drift, AF, tracking, HMM and
  clustering, but no `seg.*`. Nothing says "these objects look fragmented — check cleanup before
  tuning cellpose".

## Locked decisions

1. **The task spec is the one source.** Each task's JSON gains three fields:
   - `purpose`: one line saying what the task is for;
   - `useWhen`: 1–3 short lines;
   - `notWhen`: 0–3 short lines.

   The GUI, the MCP and any problem index read these fields. There is no separate hand-written
   troubleshooting document, because that would drift from the code.
2. **The text is for any user, never for a dataset.** Each line states a property of the method
   ("needs photon-sparse signal", "blurs cell edges"). It never names a channel, a percentile or a
   threshold for some data. This is the no-breadcrumbs rule: the reader still reasons. Each line must
   be traceable to the task's code, its docs or a plan's measurement. The PR cites the source for each
   one, and Dominik redlines the copy.
3. **The copy rules are the existing ones** ([`docs/ui/COPY.md`](../ui/COPY.md), `docs/UI.md`): brief,
   one line, the problem rather than an essay.
4. **The recommender says why, both ways.** The photon-limited score drives denoise and smooth:
   - **include** when channels are photon-limited;
   - **exclude** with the reason "not photon-limited — denoising would remove signal" when they are
     not.

   The excluded list already carries reasons, so this is the "check passed" answer, delivered where
   the cleanup decision is made. It adds no new QC finding.
5. **AF stays the user's pick.** AF correction is not added to the recommender. The bleedthrough
   estimators disagree 5× on real data (CORRECTION_QC_PLAN §Q-C2). The channel-name heuristic was
   withdrawn on 2026-10-03, because which channels share signal is the user's judgement. The `afCorrect`
   `useWhen` text and the intravital recipe's optional AF step (#1466) carry it.
6. **Segmentation findings point back a step.**
   - New `seg.*` findings are info level and are computed by the segment task from what it already
     measures (object sizes against the diameter it was given, cells per frame across frames).
   - Each finding's `long` text names the EARLIER step to check before tuning this one.
   - Thresholds start as placeholders and say so. They must be checked visually on real data before
     anyone treats them as calibrated (CLAUDE.md, *Real-data visual validation*).
7. **Claude reads a catalogue rather than searching.** A new observer tool,
   `get_task_catalogue(stage?)`, returns every visible task's `purpose`, `useWhen` and `notWhen`,
   grouped by stage (cleanup → segment → measure → gate → track → behaviour → cluster). That is small
   enough to read whole (55 × ~3 lines), so it needs no search engine. `get_module_params` carries
   the same three fields for each task.
8. **For people, the same text appears where they choose.**
   - In the module page's task dropdown: the purpose shown under the task name, and use/not-when on
     hover.
   - A "Which step?" view in the Guides panel lists every `useWhen` and `notWhen` line by stage, each
     linking to its module page. It is generated from the specs, never written by hand.
9. **A toggle, so the change can be measured.** `guide-run --discovery off` runs the agent without
   the new information. The MCP then strips the three fields, hides `get_task_catalogue`, drops `seg.*`
   findings from what it returns, and asks the recommender for its metadata-only answer
   (`evidence=metadata`). The setting is recorded in the run record. The default is on. Runs with it
   off are a separate arm and are never pooled.

## Phases

### P1 — spec fields + ratchet
- Add `purpose`, `useWhen` and `notWhen` to every visible task spec (`hidden` ones may skip).
- Read the fields in the Julia spec index. Add a ratchet test that every visible spec has a
  `purpose` and that every line fits the length limit.
- The PR lists every line with its source, for Dominik to redline.

### P2 — surfaces
- MCP: `get_module_params` carries the fields, and `get_task_catalogue` is added. Update server.py and
  guidance.py together; guidance gets one line saying when to read the catalogue.
- GUI: the task dropdown, and the "Which step?" view in the Guides panel, generated from the specs.

### P3 — recommender evidence
**Built (2026-10-06).** `_apply_photon_rules!` in `app/src/correction_plan.jl`; `evidence` on
`/api/correction-plan/recommend`; the autonomous MCP sends `metadata` when `CECELIA_MCP_DISCOVERY=off`.
Bands are placeholders: `≥ 0.90` photon-limited, `< 0.50` not, between = no opinion. Smooth is never
excluded (its `gated` statistic was built for non-photon-limited movies). The include fires only on the
no-preset card (§3: a card outranks a score). Images imported before `meta.saturation` existed get it
on their first recommend (the import's probe, persisted fill-only). Rule summary: CORRECTION_QC_PLAN.md → *Notes on the table*.
- In `apply_rules`, add the photon-limited include/exclude for denoise and smooth, with reasons.
  Test it on the engine's pure function, and cover the `evidence=metadata` switch.

### P4 — segmentation findings
- Add `seg.*` codes to `qc/text.jl`. They are emitted by the segment/measure tasks from values those
  tasks already compute, and appear in `get_qc_metrics`, the task log and the image's QC dot.
- **Checkpoint:** Dominik looks at the findings on the M2 crops before the thresholds count as
  calibrated.

### P5 — toggle + test
- Add `guide-run --discovery on|off` (Decision 9).
- **Checkpoint:** two intravital runs, one on and one off, started at midnight and supervised. The
  question is whether the agent goes back to cleanup instead of tuning segmentation.

## Out of scope

- Searching by free text (the catalogue is small enough to read).
- AF in the recommender (Decision 5).
- Rewriting the guides; they cover a workflow, and this covers single steps.
- Running new analysis at plan time beyond the existing import measures.
