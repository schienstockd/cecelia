# Cecelia structure — what it is, one clear schematic, and where it has holes

**Status:** in progress (2026-11-07), branch `structure-plan`. P1 done (draft, [`CECELIA_STRUCTURE_AUDIT.md`](CECELIA_STRUCTURE_AUDIT.md)); P2 overview v1 in the README; record fixes #2011, #2016, #2020 merged; follow-ups #2023, #2024, btrack #2027 merged. P3 ranked (2026-11-08); waves A–C and follow-ups merged 2026-11-09 (#2036–#2055). Open: maintainer calls listed under *Open calls*. The brief this came
from, [`docs/archive/CECELIA_STRUCTURE_AUDIT_PROMPT.md`](../archive/CECELIA_STRUCTURE_AUDIT_PROMPT.md),
was an elaboration of the ask below. This plan narrows it back to the ask.

## Goal

The maintainer's ask (2026-11-07):

> If we argue that Cecelia is a structured platform for image analysis then we should audit what that
> structure is and come up with a clean and clear schematic. Because if that's the punchline then that
> has to be clear. And the audit might flag where we have gaps in the current setup or flaws or aspects
> that we should be expanding. Then the AI part can stay minimal and experimental and a bit more
> freeform because of that structure.

There are three products:
1. **What the structure is**, described from the code.
2. **One clear schematic** of it.
3. **Gaps, flaws and things worth expanding.**

AI is drawn as a thin layer that reads from the structure. This plan proposes no AI features.

## Decisions (2026-11-07)

1. **Read the code; don't run new walkthroughs.** Many walks have already been done, by hand and by
   autonomous agents: claim recital P0/P2, the agent-overnight app runs, judge sweeps. Their findings
   are cited as evidence for gaps. They are not re-run.
2. **"Structure" means the layers that make work checkable, not the feature list.** That is the
   objects and their record, the contracts, the QC and checks, and chains/replay.
   `HARDENING_SURFACES.md` already lists the features (175 surfaces). P1 uses it to find code and does
   not rebuild it.
3. **The AI layer comes from existing inventories.** These are `HARDENING_SURFACES.md` → *Claude+MCP*
   and *Kiwi*, `frontend/src/lib/claudeTools.ts`, and `AI_TOGGLE_PLAN.md` (#2008, which owns the
   "AI off" switch). For the schematic, this plan records only which structural part each AI surface
   reads.
4. **Maturity marks come only from shipped records** (`frontend/src/lib/guides/maturityRecords.ts`).
   Anything without a record is drawn untiered, not guessed. Today no feature is Tested (14 Beta,
   9 Experimental, 1 exempt). Planned parts are drawn as gaps, never as if they exist.
5. **Where it lives.** The P1 description is
   [`CECELIA_STRUCTURE_AUDIT.md`](CECELIA_STRUCTURE_AUDIT.md), next to this plan. The schematic source
   (Mermaid + SVG) goes in `docs/todo/structure_figures/`. Once the maintainer has reviewed them, the
   description and schematic are promoted to a permanent `docs/STRUCTURE.md`, and the gap list stays
   here until it is worked off.
6. **The schematic needs the maintainer's eyes before any paper use.**

## Phases

**P1. What the structure is.** This is code reading, and it is in progress. It covers:
- the objects (project, set, image, output, population, gate, run, chain, blackboard entry), where
  each is stored and in what format, and the file that owns it;
- what each pipeline stage records (inputs, params, outputs, versions);
- which contracts are enforced in code and which only by convention (units, 2D/3D, time interval,
  coordinates, naming, population definitions);
- where the QC and checks sit along the pipeline;
- what each AI surface reads.

Each claim cites a file. *Checkpoint:* `CECELIA_STRUCTURE_AUDIT.md`.

**P2. The schematic.** The pipeline runs left to right: acquire → import → process (segment, track,
clean) → populations and gating → analyse and plot → export. Underneath it are the layers: the
record, QC and checks, chains (replay), and AI on top. There are two versions: a full one for the docs
and a ten-box one for the paper. Each gets a two-sentence caption and a legend that maps every box to
a file. *Checkpoint:* render reviewed by the maintainer.

**v1 (2026-11-07):** one compact overview (1100 px wide), in the style of the README's earlier
framework diagram: a band per layer with a short headline, a one- or two-word chip per stage, and the
tagline "Analysis you can hand off and check". `structure_figures/structure_overview.svg` is the plan
copy, with gaps in magenta italics. `make_structure_overview.py --readme` writes the README copy
(`frontend/public/readme-structure.svg`), which shows only what exists, because it is the first
diagram a new reader sees. Maturity tiers are not shown yet. A ten-box paper version is still to do.

**P3. Gaps, flaws, worth expanding, worth cutting.** These mostly come from P1 and P2: anything that
can't be drawn cleanly is a finding. Each item records what it is, the evidence (code ref or an
existing walk/run), its size, and whether to fix it before the protocol or state it as a limitation.
The checklist for spotting them: traceability, consistency, gaps shown as gaps (not silent defaults),
QC per stage, replay, mechanical review, hand-off from the record alone, works with AI off.
*Checkpoint:* the ranked table below, plus an order of work.

## P3 ranking and order of work (2026-11-08)

Ranked by what a reader of the record or a reviewer of a claim would trip on first, then by size.
Fix all of them except the two stated limitations at the end. Waves group the work by code area, so
parallel PRs don't conflict.

| # | Gap (rows below) | Why this rank | Wave |
|---|---|---|---|
| 1 | Missing calibration silently becomes 1.0 µm/px and 1 min/frame; z never required | A wrong unit is a wrong number with no error, which is the worst kind of hole | A |
| 2 | Run log doesn't name the input version read, or that a run replaced an earlier output | The record can't trace a number back without it | A |
| 3 | Run log cap drops old entries silently | A record that forgets is not a record. Archive, don't drop | A |
| 4 | Chain run stores only a params hash; no start from an old frozen template | Replay is one of the four layers in the schematic | A |
| 5 | Claim checks miss preprocessing params and parent gates; no 2D/3D consistency check | The mechanical review is the punchline's evidence | A |
| 6 | Raw source has no fingerprint | The trace ends at a path that can change underneath | A |
| 7 | Gates overwritten in place; GUI gate edits leave no run-log entry | The trace breaks at the gate (audit, trace walk) | B |
| 8 | Gating and statistics have no QC; the QC-required rule isn't enforced; cohort QC never runs in chains | The two stages with no checks | B |
| 9 | Board PDF/SVG carry no provenance; Parameters CSV rebuilt at export | The exported figure is what leaves the lab | B |
| 10 | Population types have no registry | Internal consistency, refactor-sized; last so it doesn't collide | C |

**Stated limitations, not fixed:** runs from before `runlog.json` existed have no params; nothing
blocks (warn, never block) stays a choice for the maintainer to state in the paper.

## Open calls (maintainer, from the P3 fixes)

- **Warn, never block** — state it in the paper as a choice, or treat it as a gap.
- **Resume keeps recorded overrides** (#2043) — changed from dropping them; replay ↻ button unseen in the app.
- **Gate-edit guard is wider** (#2052) — a track run now counts as reading the cell gate above it.
- **`authored_labels_version`** stays `v1` (#2052) — keep as documented, retire the drift banner, or stamp the labels' run.
- **Board export** (#2050) — plots stay live, only the Parameters table is frozen; old boards snapshot on first plot change. Export a PDF/SVG once to check.
- **Staleness noise** (#2051) — outputs downstream of pre-`inputs` runs show `staleUnknown` until re-run.
- **Label pixels vs measures** (#2055) — the run log can't tell which a reader used, so a re-measure flags pixel readers (branching) and misses population readers (clustering).
- **Source check** (#2055) — runs on metadata resync only; a missing raw file is info, not a warning.
- **Pop-type registry JSON direction** (#2054) — Julia writes it (figureStyle goes the other way).
- **Needs eyes:** stats warning line + Check cohort on Gating (#2053), keep-previous Settings tooltip (#2051), replay ↻ (#2043), PDF/SVG export (#2050).

## Gap list (from P1; ranked and sized in P3)

Evidence for each row is in [`CECELIA_STRUCTURE_AUDIT.md`](CECELIA_STRUCTURE_AUDIT.md). Sizes are
first guesses: S = under a day, M = a few days, L = a design first.

**Record (provenance)**

| Item | Kind | Size | Status |
|---|---|---|---|
| Run log stores no Cecelia/engine version, device, seed or author stamp | flaw | S | done, #2011; remaining tasks #2024 |
| Run log doesn't record which input version a run read (unless pinned) | flaw | S | done, #2039 |
| Re-runs overwrite labels, measures and branching. **By design** (maintainer, 2026-11-07): keeping every segmentation by default would pile up stores nobody needs, and `keep_previous_version()` is the opt-in for reprocessing. What the structure needs is that the record names the run that made the current output and says it replaced one | choice, record part open | S | done, #2039 (`replaces`); re-measure #2055; downstream staleness #2051 |
| Gates are overwritten in place, so a plotted population can't be tied to the gate it was drawn with. Unlike stores, gate files are tiny, so history is cheap. `authored_labels_version` is always `v1` | flaw | M | done, #2052 (history + hashes; `v1` is by design, documented) |
| Board PDF/SVG carry no provenance; the Parameters CSV is rebuilt at export, not frozen with the plot | gap | M | done, #2050 |
| Remembered params are written at dispatch, even for failed or cancelled runs. Fine as a form default (retry what you tried); wrong where it's read as the record (the claim check's flat fallback, and the comment at `sockets.jl:799`) | flaw | S | done, #2020 |
| Run log is capped at 200 entries per image, and old entries drop silently | flaw | S | done, #2039 (archive) |
| Excluded or skipped images, and why, are not saved anywhere | gap | S | done, #2016 |
| Raw source is a path only, with no checksum | gap | S | done, #2040 (fingerprint); compared on resync #2055 |
| GUI gate edits are not tasks, so there's no run-log entry for them | gap | M | done, #2052 (gate history is their record) |
| Runs from before `runlog.json` existed have no recorded params | flaw (history) | — | stated limitation |

**Contracts**

| Item | Kind | Size | Status |
|---|---|---|---|
| Missing calibration silently becomes 1.0 µm/px and 1 min/frame outside `requires.scale` tasks; z is never required | flaw | S–M | done, #2040 (recorded + QC warning) |
| No check that two populations in one image were measured the same way (2D/3D) | gap | S | done, #2036 |
| Population types have no registry; hard-coded lists already disagree | flaw | M | done, #2054 |
| QC is "required" for new tasks, but no test enforces it | gap | S | done, #2053 |

**QC and checks**

| Item | Kind | Size | Status |
|---|---|---|---|
| Gating and statistics have no QC; the claim checks are their only check | gap | M | done, #2053 |
| Cohort QC runs only from a button; chains never run it | gap | S | done, #2053 |
| Claim checks compare only segmentation/tracking params (not preprocessing) and only the leaf gate | gap | S | done, #2036 |
| Nothing blocks. Is that a choice to state in the paper, or a gap? | question | — | maintainer call |

**Replay**

| Item | Kind | Size | Status |
|---|---|---|---|
| A new chain run uses the current template; there's no way to start one from an old run's frozen template | gap | S–M | done, #2043 |
| A chain run stores only a params hash, not the effective params or REPL overrides | flaw | S | done, #2043 |
| btrack's optimiser time limit. The 10-min limit never applied (btrack nested it), so tracking always ran unlimited; now explicit, because a timeout returns unmerged tracks | flaw | S | done, #2027 |

**Worth expanding (strong and underused)**
- **The scheduler gives every task a run log, validation and an applicability check for free.** This
  is the backbone of the structure and should be the centre of the schematic.
- **The claim checks (S0–S2) need no model.** They are the mechanical review the punchline needs.
- **AI reads only through shared readers, and its writes are stamped.** "Thin layer on top" holds in
  the code.
- **Author stamps exist everywhere except task runs.** Putting them on run logs is a small change that
  closes the AI provenance story.

## Related

`HARDENING_AUDIT_PLAN.md` + `HARDENING_SURFACES.md` (feature inventory) · `AI_TOGGLE_PLAN.md` (#2008) ·
`IMAGE_ANALYSIS_RECITAL_PLAN.md` (claim checks, P0/P2) · `EVIDENCE_PROVENANCE_PLAN.md` (run stamps,
parked) · `FEATURE_MATURITY_PLAN.md` · `docs/OBJECTMODEL.md` · `docs/SCHEDULER.md` · `docs/POPULATION.md`.
