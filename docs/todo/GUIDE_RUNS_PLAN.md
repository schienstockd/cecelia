# Guide runs — an agent follows one in-app guide, you review what it hands back

**Status:** in progress — P1 built (2026-10-06) — `cause` on a run record's `bad` section verdict (API, GUI, MCP); P2, P3 not built. Reframes the app-tier runs of
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

### P3 — `guide-run`
- `scripts/agent_eval/guide_run.py` wraps `run_app.py`: guide id → test project + brief, `--runs`,
  `--at`, `--knowledge`, `--budget-usd`. Runs are sequential, one at a time. The record title names
  the guide.
- Traces are written to `~/.cecelia-effectiveness/app-runs/<stamp>/`, not `/tmp`.
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
