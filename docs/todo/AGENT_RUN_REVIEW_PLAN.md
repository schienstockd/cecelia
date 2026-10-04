# Agent run review — the run's decisions on the blackboard, a verdict on each

**Status:** P1 + P2 section verdicts built (2026-10-04) — run records via `run_record.py` with the `agentRun` marker; Good / Bad / Unsure per decision, misses, the "Agent runs" filter, the MCP proposal tool. Not built: P2's refs into the copy (KiwiRef `projectUid` + `chain`), P1b, P3, P4. Follows [`AGENT_OVERNIGHT_PLAN.md`](AGENT_OVERNIGHT_PLAN.md)
P4b (the app-tier runs). Builds on the blackboard ([`BIDIR_CONTEXT_PLAN.md`](BIDIR_CONTEXT_PLAN.md)
Part 4, [`PROJECT_MEMORY_PLAN.md`](PROJECT_MEMORY_PLAN.md) Decision 11 outcomes), `KiwiRef`
([`KIWI_ASSISTANT_PLAN.md`](KIWI_ASSISTANT_PLAN.md) Decision 4) and the frozen-ref sidecar
([`KIWI_CAPTURE_AND_BLACKBOARD_PLAN.md`](KIWI_CAPTURE_AND_BLACKBOARD_PLAN.md) Decisions 9–10).

## Goal

An unattended run leaves a record you **correct in place**: in your own project, read what the agent
decided on each image and on what evidence, mark each decision good / bad / unsure with a note,
discuss it with Kiwi or a chat session that sees the same pictures and refs, and get a **score**
that compares runs. The record outlives the run's copy, which is disposable. Today the record is
`trace.jsonl` + `record.json` in `/tmp`, readable only by replaying the run.

Corrections become lab knowledge for later runs only after a person rewrites them in general form
(Decision 9).

## What exists (verified 2026-10-04)

- Blackboard entries: `<proj>/blackboard/<entryId>/{entry.md, meta.json}`; Markdown + Mermaid;
  `attachments` (capture ids, frames shown in the entry); the `kiwiRefs` sidecar of ref chips with a
  frozen label + snapshot per ref, and `KiwiRefChip` already falls back to `was: <label>` when a ref
  no longer resolves; "Add to Kiwi" on an entry; ONE entry-level `outcome {verdict, note}`.
- `KiwiRef` kinds (`frontend/src/utils/kiwiRef.ts`): project, set, image, population, cells, tracks,
  viewer, plot, tile, capture, task, ui, blackboard, proposedPlot. A ref has **no project id** — it
  resolves in the project the entry lives in. No `chain` kind.
- Captures: `POST /api/viewer/capture` writes `<proj>/captures/<id>/{meta.json, frame.png}` with a
  `surface` from a closed set (`viewer_frame`, `viewer_slab`, `ui`, `plot`); the route is deliberately
  off the MCP allow-lists (a capture is what the USER shared).
- The run trace records every tool call with its arguments, in order (`trace.jsonl`, stream-json);
  `record.json` holds the copy's resulting chains / gates / label sets next to the reference's.
  **It says little about why:** run 1 of 2026-10-04 has 66 tool calls, 7 text blocks, and 28 thinking
  blocks that arrive **empty** in the trace.
- `gate_plot` / `gate_cells_view` (#1407) render the pictures a gating decision rests on.
- `get_cohort_qc` — per-set outliers for a task's banked metric (nCells, nTracks, meanSpeed, HMM state
  mix…). Unusable in one-image copies: runs 1 and 2 called it and failed on the missing set.
- The blackboard is **per project** (PROJECT_MEMORY_PLAN D8). A run copy is built raw by
  `app_project.py`, so it has no blackboard — last night's "empty-blackboard baseline" could not have
  been anything else.

## Locked decisions

1. **A run covers the whole test set.** The copy holds every run image (raw only), in one set, so the
   agent works through them as a user would and cohort QC has a cohort. Brief: *"Hey. can you track
   the cells in these images and analyse their behaviour?"* The source is tSJpBI's "Crop" set (7 crops,
   all with a reference analysis), split by mouse: the **run images are M1's four** (yDfwP7, UJS0Hz,
   dvFmih, 3vBHp8); **M2's three are held out** (jV6p8M, 8F20qd, QWhG6x, Decision 9), so the P4
   comparison tests whether a lesson from one mouse carries to another.
2. **The unit is a decision, not the reasoning.** A section per decision, tagged with its image and
   one fixed step — `cleanup · segment · measure · gate · track · behaviour · report` — holding what
   was done (chain node + params, gate geometry, task run), the evidence looked at since the previous
   decision, and what came out. Prose reasoning is not graded on its own: it is often a post-hoc
   story and cannot be scored consistently.
3. **The harness writes the record from the trace — the agent is not asked to.** Actions are extracted
   mechanically (`create_chain` nodes, `run_task`, `add_gate` / `set_gate` / `delete_gate`), so a run
   cannot leave out the decision it got wrong, and the agent's instructions do not change (the
   no-breadcrumbs rule holds). Its own words appear verbatim as *said before acting*.
4. **The why is asked after the run, and labelled so.** The harness resumes the finished session once
   (`claude -p --resume`, budget-capped) with the extracted decision list and asks for one line per
   decision. The run is over and its record frozen first, so this cannot change what it did; the
   answer shows as *explained after the run*.
5. **The record lives in the SOURCE project and is self-contained.** One blackboard entry per run, in
   your project, so marking happens in one place and survives deleting the copy:
   - the decision text, params and counts are in the entry body;
   - the pictures the agent looked at (gate plots, cells views) are **captures in the source project**,
     attached to the entry, so the evidence outlives the copy;
   - refs into the copy (population, cells, tracks, chain) carry the copy's project id and a frozen
     label: live they open the copy, after deletion the chip shows `was: <label>`.
   Copies are built raw, without the source's blackboard, so a later run never reads earlier records.
6. **Run evidence is marked as such.** The harness's captures use a new surface `agent_run`, so the app,
   Kiwi and `get_recent_captures` can tell a run's evidence from what you shared; the "look at this"
   list shows only yours. The capture route stays off the MCP allow-lists — the HARNESS writes these,
   never an agent.
7. **Verdicts are per section, authored by a person.** `good | bad | unsure`, note required for `bad`.
   A chat session may *propose* a verdict (Kiwi turns stay read-only) (stamped `by: claude`); the score counts only
   `by: user`. The entry-level outcome stays.
8. **Misses are sections too.** What the agent should have done and did not (no QC gate, no AF
   correction) is added by the reviewer as a `missed` section — image + step — with a `bad` verdict.
   Without this the score rewards doing less.
9. **Corrections do not flow to the next run automatically.** A `bad` note on a given image
   ("OTI: CTV < 240") read back by a run on that image is the answer key, not reasoning. Promoting a note
   to lab knowledge is a person rewriting it in general form ("T-cell dyes bleed into each other's
   channels — check each pair"). The with/without-knowledge comparison scores on **held-out images**
   (M2's crops, Decision 1), which no run or correction has touched.
10. **Platform findings are recorded apart from the agent's score.** A tool error or a backend error
   during the run is the app's fault, not the agent's: the record lists them in their own section,
   unscored, and the harness logs each as an `agent_run_finding` event to the effectiveness log so the
   weekly judge sweeps and verifies them like any other bug (Decision 10 detail below, agreed with the
   judge session 2026-10-04). Not `fanout_audit_finding`: those feed the commit hook's must-tag gate,
   reviewer precision and the rule mapping, where a tool error would count as a CLAUDE.md violation.
11. **Lab knowledge lives in the project's own blackboard.** An entry marked as knowledge stays in the
   project it was written in, and the harness carries that project's knowledge entries — and only
   those — into a run copy. No machine-level store (as the model vault is): it would drift from the
   project, miss from an export, and mix in other users' knowledge from the same machine.

## Shape

Entry `Agent run <stamp> — <set>` (filterable as an agent run in the Blackboard list), one section per
decision in run order:

```markdown
### d07 · gate · yDfwP7 · /OTI_CTVneg on OTI
**Did:** add_gate rectangle mean_intensity_2 0–5000 × mean_intensity_3 0–240 (linear) → 106 / 341 cells
**Looked at:** gate_histogram(OTI, mean_intensity_3) · gate_plot(OTI, 2 × 3) [capture]
**Said before acting:** "most objects in the OTI segmentation had gBT-level CTV …"
**Explained after the run:** …
```

Section verdicts in `meta.json`: `sectionOutcomes: {"d07": {verdict, note, by, at}}`. Ids are the
heading's `dNN` and never renumber; misses are `mNN`.

## Phases

### P1 — multi-image copies + the run record writer (harness)
- `app_project.py`: copy the run images (M1's four, raw) into one set; `run_app.py` takes the set,
  the plural brief, and records per image + the cohort QC numbers on both sides in `record.json`.
- `scripts/agent_eval/run_record.py` (on `trace_view.py`'s existing stream-json parser, not a second one): `trace.jsonl` → decisions (actions, image, step, the reads since
  the previous action, the preceding text block) → the post-run "why" turn → one entry in the source
  project via the existing blackboard create route; evidence images as `agent_run` captures (the PNG
  from the trace's tool-result image block when present, else re-rendered through the same route).
- Captures: `agent_run` added to `_CAPTURE_SURFACES`; `get_recent_captures` and the share-in list
  skip it.
- Canary: the record is written after the canary is read, so the harness's writes into the source
  never meet it (built that way instead of an exemption).
- Kiwi's "Clear all captures" keeps `agent_run` captures — they are a record's pictures, not shares.
- **Checkpoint:** back-fill last night's three single-image runs (no agent cost) → three entries you
  read for completeness and grain.

### P1b — platform findings → the weekly judge (after the judge refactor merges)
- `agent_run_finding` added to the closed event taxonomy (`python/cecelia/effectiveness/log.py`).
  Payload `{key, tool, error, file, line, desc}`; row `commit` = the SHA the run checked out, `branch`
  = null, so the judge's sweep treats it as landed and reads the function at that commit.
- Sources: the run's tool errors (name + the API's error text) and backend errors logged in the run
  window (`file:line` from the stacktrace when it has one). Dropped only when the error is empty or is
  plainly the agent's own misuse; everything else goes to verify.
- `key` = sha1(tool + error text with uuids, paths, numbers and timestamps stripped), so the same
  error does not open a new bug every night.
- Judge side (the judge session's follow-up): `bugs.candidates()` reads the new event; a finding
  without `file:line` skips the excerpt step — `open`, "no file:line — for the verify agent",
  grouped by `key` — instead of merging every location-less finding into one bug.
- **Checkpoint:** a back-filled run's set_gate errors (fixed in #1405) appear in a judge dry run as
  one bug, then `gone` at a SHA after the fix.

### P2 — refs into the copy + section verdicts
*Section verdicts built (`api/src/blackboard_run_review.jl`, `components/blackboard/`); `by` is the
caller's `author_stamp()`, so a Claude verdict is a proposal by construction, not by a body field.
Refs into the copy are next — the record already reads completely without them (text + captures).*
- `KiwiRef` gains an optional `projectUid` (resolver + chip open the copy; absent = the entry's own
  project, so every existing ref is unchanged); a `chain` kind `{name, node}`. Frozen labels written
  by the harness, so the existing `was: <label>` fallback covers a deleted copy.
- `POST /api/blackboard/section-outcome` (`{entryId, sectionId, verdict, note, by}`) + the
  `sectionOutcomes` field; add-a-miss writes an `mNN` section.
- `BlackboardModule.vue`: good / bad / unsure on each section heading, note inline, "add missed
  decision" (image + step); an "agent runs" filter on the list. Copy per `docs/ui/COPY.md`.
- Observer MCP `set_blackboard_section_outcome` (stamps `by: claude`) — server.py + guidance.py.
- **Checkpoint:** you review one back-filled run in the GUI; delete its copy; the entry still reads
  completely (pictures, `was:` chips). A Kiwi/chat session proposes verdicts that show as proposals.

### P3 — the score
- `scripts/agent_eval/score_review.py`: per run, per image and step — good / bad / unsure / missed;
  across runs the same table plus run-to-run spread; next to `record.json`'s comparison with the
  reference and the cohort QC. Lists runs with unmarked sections and their copies' names.
- **Checkpoint:** the back-filled runs scored after your P2 review.

### P4 — the knowledge loop
- "Promote to lab knowledge" on a section: a new entry pre-filled with the note, marked as knowledge,
  for you to rewrite in general form.
- The harness copies the source project's knowledge entries into each run's copy before the agent
  starts (Decision 11).
- **Checkpoint:** the same brief on the held-out images (M2's three), three runs with and three without the knowledge
  entries, scored with P3.

## Touchpoints

P1: `app_project.py` + `run_app.py` + 1 new script + `_CAPTURE_SURFACES` and two capture readers +
the canary. P1b: 1 event name in `log.py` + the harness emitter; judge side `bugs.candidates()` + the
location-less branch. P2: `kiwiRef.ts` + resolver + `KiwiRefChip` (project id, `chain`), 1 route + 1 meta field,
`BlackboardModule.vue`, 1 MCP tool (server.py + guidance.py), tests. P3: 1 script + test. P4: 1 GUI
action + 1 copy step in `app_project.py`.

## Out of scope

Automatic claim checking (Kiwi's support checker, parked); grading the agent's prose; any change to
what the agent is told.
