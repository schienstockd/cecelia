# Guide runs — an agent follows one in-app guide, you review what it hands back

**Status:** in progress (2026-10-06) — P1 built (`cause` on a run record's `bad` section verdict: API, GUI, MCP);
P2 built (run reviews to the weekly judge; `agent` causes listed, not matched); P3 `pixi run guide-run`
built. Checkpoints open: P1's marks, P2 in `judge-review`, one reviewed intravital run.
Reframes the app-tier runs of
[`AGENT_OVERNIGHT_PLAN.md`](AGENT_OVERNIGHT_PLAN.md) P4b. Builds on the run record and section
verdicts of [`AGENT_RUN_REVIEW_PLAN.md`](AGENT_RUN_REVIEW_PLAN.md) (P1, P1b, P2, P4), whose P3 score
this replaces. The brief that asked the question:
[`docs/archive/agent-runs-scope-prompt.md`](../archive/agent-runs-scope-prompt.md).

## Goal

The question each run answers is: **does this guide lead a new user to a correct analysis?** An agent
stands in for that user. It gets one guide and a project, and it hands back an analysis. You review
it the way you would review a rotation student's work done from the lab's protocol: the protocol is
what's under test, not the student.

These are not smoke tests. Most of what the 2026-10-05/06 runs found did not stop the agent. It
finished, and you found the problems by reading the results:
- squashed gate views;
- clusters with no description;
- records without plots;
- AF correction skipped before merged-channel segmentation.

The repeated tool errors (the mechanical part) already reach the judge.

## What exists (verified 2026-10-06)

- **The runner.** `scripts/agent_eval/run_app.py` takes a fresh copy of the source project, a canary
  on the source, a budget cap, and `--knowledge` (off by default). It writes the run record into the
  source project as a Blackboard entry (`run_record.py`), with stage boards attached
  (`stage_boards.py`).
- **Section verdicts.** `api/src/blackboard_run_review.jl` stores `sectionOutcomes
  {sectionId: {verdict, note, by, at}}`: `good | bad | unsure`, a note required for `bad`. Misses are
  `mNN` sections.
- **Errors to the judge.** `run_findings.py` logs `agent_run_finding`. A 4xx that a single run fixed
  on its next call stays unscored; the same error in ≥2 runs is logged as `kind: "repeat"` (#1447).
- **Guides to the agent.** The `get_guide` MCP tool reads `mcp/cecelia_mcp/guides.json` (#1435). A
  vitest checks that every task a guide runs has a task spec (#1463).
- **Traces.** Copied out of `/tmp` to `~/.cecelia-effectiveness/app-runs/` (2026-10-06). The harness
  still writes to `--root`.

## Locked decisions

1. **The unit is one guide on one project.** The brief is *"Use the <guide> guide to process the
   images in this project."* Each guide has its own test project, starting with the easiest, cleanest
   data, so that a failure points at the guide or the platform rather than at the data.
2. **A `bad` verdict carries a cause.** Each one names:
   - `guide`: the guide didn't say it;
   - `platform`: the information existed but the agent couldn't see it;
   - `agent`: the guide and the tools were enough.

   When guide and platform both fit, pick the cause with the cheaper fix; the label routes the fix,
   it is not a finding in itself. Expect `agent` to be a large bucket — the agent follows the text
   literally and never asks; the repeat rule (Decision 3) turns a recurring one into a guide gap.
   A cause is one field on the existing section verdict, not a new scoring system. A run also gets an
   overall verdict, using the entry-level outcome: accept, accept with fixes, or reject.
3. **What reaches the judge.**

   | Goes | Stays on the record |
   |---|---|
   | the same tool error in ≥2 runs (built) | an error the error message fixed on the next call |
   | every `bad` with cause `guide` or `platform` | analysis choices you would make differently |
   | the same `agent` cause in ≥2 runs of a guide, proposed as a guide gap | a single `agent` cause |

   A person sets the cause, so the judge's verify step checks the fix, not whether the finding is real.
4. **`--knowledge` stays minimal and is never pooled with guide runs.** A lesson that would help any
   user becomes guide text instead (the AF step, 2026-10-06). Lessons hold only what is specific to
   the lab. Runs with knowledge test the Blackboard itself and are reported separately. Every record
   names the guide, the commit and whether knowledge was on.
5. **One run by default.** Three runs only to check that a fix changed the agent's behaviour. The cost
   cap is $5 per run (observed $2.33–2.94).
6. **Started by hand, no timer.** `pixi run guide-run <guide> [--runs N] [--at HH:MM]` (Linux, like the
   judge timer). A run follows a change to a guide or the tasks it runs, or a batch of fixes. The
   wrapper stays thin: fresh copy, cap, teardown, cost recorded even when a run aborts. No LLM
   supervisor and no nudging mid-run.
7. **Stop rule, per guide.** Three reviewed runs with no fix pause that guide. If every guide is
   paused, the runs stop.
8. **Your review is the budget.** The cap on runs is what you will review in a week, not what the
   machine can run. Review only runs that finished; one that aborted or hit the cap is read from its
   errors, which already reach the judge. As guides multiply, the review budget decides which guides
   run, not the cost of running them.
9. **A short checklist per guide**, kept with its test project: the few things you look at (e.g. for
   intravital: cleanup before segmenting, QC gate on the image, cells per frame, speeds, what the
   clusters are made of). It holds one reviewer's judgement steady across weeks. What a reference
   can answer (detections, track speeds against your own analysis of the same crops) comes from
   `record.json`'s comparison, so your reading goes to the judgement calls.

## Phases

### P1 — the cause on a `bad` verdict
- `sectionOutcomes[id].cause ∈ guide | platform | agent`, required with `bad`, in
  `blackboard_run_review.jl` + `POST /api/blackboard/section-outcome`.
- The GUI offers three choices beside the note (`BlackboardModule.vue`; copy per `docs/ui/COPY.md`).
  The MCP `set_blackboard_section_outcome` passes it through, still as a proposal.
- **Checkpoint:** you mark the three 2026-10-06 records.
- *Built:* `_BB_SECTION_CAUSES` in `blackboard_run_review.jl` — required with `bad` on a run record
  (meta `agentRun`), refused with `good`/`unsure` and on any other entry (an ordinary note's `sNN`
  claims have no guide). A `bad` stored before causes has none: read as not set, never rewritten; the
  GUI shows the three choices unselected beside its note, and picking one saves it. The add-a-miss
  form takes a cause too (a miss is a `bad`). MCP `set_blackboard_section_outcome(…, cause)`.

### P2 — causes to the judge
- The weekly pass reads run records (meta `agentRun`) and logs each new `guide` or `platform` verdict
  as an `agent_run_finding` with `kind: "review"`, carrying the run, section and note. It is keyed so a
  verdict is logged once.
- `agent` causes are listed per guide. Two that match across runs are proposed as one guide gap.
- **Checkpoint:** P1's marks appear in `pixi run judge-review`.
- *Built:* `scripts/judge/run_reviews.py`, run by `weekly.py` before the bug sweep (and
  `pixi run judge-run-reviews` to print). It scans `*/blackboard/*/meta.json` under the
  projects dir (`--projects-dir`, else `CECELIA_AGENT_APP_PROJECTS`, else `~/cecelia-feijoa/projects`). A person's `bad` with
  `guide` / `platform` → one row, key `rev-` + sha1(project, entry, section), carrying run, section,
  heading, cause, note and `agentRun.guide`. The sweep opens it without the judge; verify checks
  whether the fix is in. The rows are stamped a second before the pass started, so the next pass
  never reads them as new. **Matching is not built:** no judge step compares free-text notes, and one
  would be a new LLM layer. The record lists `agent` causes per guide and flags a guide whose notes
  span 2+ runs as a possible gap, for a person to compare. Design: `docs/ai-assist/WEEKLY_JUDGE.md`.

### P3 — `guide-run` — **built, checkpoint open**
- `scripts/agent_eval/guide_run.py` wraps `run_app.py`: guide id → test project + brief, `--runs`,
  `--at`, `--knowledge`, `--budget-usd`. Runs are sequential, one at a time. The record title names
  the guide.
- Traces are written to `~/.cecelia-effectiveness/app-runs/<stamp>/`, not `/tmp`.
- Built as specified, plus: the guide → test project map (with the reviewer checklist, shown atop
  the record) is `scripts/agent_eval/guide_projects.json`; the projects dir comes from the running
  app's `/api/diagnostics`; each run (or a run skipped because the app is down or the lock is held)
  appends one line to `~/.cecelia-effectiveness/guide-runs.jsonl`; `--runs N` stops at the first run
  that fails. The record's `agentRun` meta carries `guide`, `codeSha` and `knowledgeOn`.
- **Checkpoint:** one intravital run started with the command, then reviewed with P1.

## What would change this plan

- **Add a timer** (a weekly one-guide rotation) once reviews keep producing fixes without you
  starting the runs yourself.
- **Add harder data** (a noisier or dimmer project per guide) only after the easy tier passes. Add one
  variable at a time and report each tier separately, so that hard biology is not logged as a bug.
- **A monitor** (`smoke-console` in the brief): not until runs are frequent enough to lose track of.

## Out of scope

Grading the agent's prose. Automatic scoring against a reference. Changing what the agent is told (no
breadcrumbs). GUI-only friction, which the agent never sees.
