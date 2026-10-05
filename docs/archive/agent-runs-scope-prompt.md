> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in `docs/todo/GUIDE_RUNS_PLAN.md`.
>
> **Outcome:** not run as a brief; its questions were answered in a chat with Dominik on 2026-10-06,
> which reframed the runs as guide runs reviewed by a person rather than smoke tests (a clean finish
> hid most of what the runs found). Read [`docs/todo/GUIDE_RUNS_PLAN.md`](../todo/GUIDE_RUNS_PLAN.md).

# Define what the autonomous runs are for: guide smoke tests, not unattended analysis

**Model:** Opus.
**Type:** Evidence review + scope recommendation. Analyze existing runs; do not
launch new ones and do not build anything. Every recommendation must cite what
the runs actually show.

## Why

The autonomous runs started as "process my images overnight and it will be
perfect, with the app optimized for agent use." We want to reframe them, and
this task decides whether the evidence supports the reframing.

**Proposed reframing (a hypothesis, not a conclusion):** each run is a
**smoke test of one in-app guide**. The agent gets a guide (via the MCP
`get_guide` tool), processes images, and reports where it got stuck or worked
around something. It stands in for the user who silently clicks around a bug
instead of reporting it. Pass means "finished the guide with no detours," not
"the biology is right." We will add more guides for other image types, so the
design must stay cheap as guides multiply.

**Evidence so far (verify each, they may be out of date):**
- The first guide run led to real fixes: the `set_gate` error (#1405), gate-plot
  axes zoomed out by open-ended thresholds (commit 9516aeb), and popSelection
  `accepts` mismatches where a track gate returned empty or threw (#1453, which
  also made `get_module_params` keep `accepts`).
- PR #1437 gave the agent the guides; the intravital recipe now follows the
  paper's workflow with cellpose as the default.
- PRs #1420 and #1422 added the judge intake for run errors
  (`agent_run_finding`).
- Dress-rehearsal baseline: with tables only, no pictures, the agent caught the
  CTV→GFP bleedthrough in 1 of 3 runs.
- **A trial run with the blackboard showed the agents listened to the
  blackboard entries** (the maintainer's observation; verify it in the
  transcripts). The setup for a run should therefore let the maintainer choose
  whether the agent uses blackboard scoring.
- **The datasets so far were chosen deliberately as the easiest and cleanest,
  with the most straightforward analysis.** A failure on them is therefore
  more likely a platform or guide problem than data difficulty. More complex
  images and more guides are easy to add later.
- **Three new runs were set up after those fixes merged.** They are the main
  new evidence here.

## Principles to hold

- **Build nothing until a real run shows it is needed.** Treat any proposal,
  including this prompt's, as a hypothesis.
- **Cheap code, expensive verification.** A layer earns its place by catching
  what would cost more downstream than the layer costs to run and maintain.
- **Do not merge** friction findings (where the agent got stuck) with
  ground-truth scores (is the biology right). Different questions, owners and
  urgency.
- **A clean run proves the guide is executable, not that the analysis is
  correct.** The agent never touches the GUI, so GUI-only friction is out of
  scope for these runs.

## The work — do this first

Find the run records and transcripts yourself (starting points: 
`scripts/agent_eval/*`, `docs/todo/AGENT_OVERNIGHT_PLAN.md`,
`docs/todo/AGENT_RUN_REVIEW_PLAN.md`, `docs/ai-assist/judge-runs/`, the
effectiveness log's `agent_run_finding` rows, `mcp/cecelia_mcp/guides.json`).
Read the transcripts, not only the agents' own summaries.

1. **Per run (the three new ones):** which guide, outcome, wall time, tokens
   and cost, whether it finished, every place it got stuck, every workaround
   or detour even when it finished, every REPL fallback, and every error.
2. **Classify each friction point:** guide gap / tool or API bug / missing
   capability (e.g. no plots for a QC step) / agent error / fixture or data
   issue / GUI-only (out of scope). Note which ones reached the judge as
   findings and which would have been invisible without reading the transcript.
3. **Compare with the first run.** Which earlier failures disappeared after the
   fixes, which recurred, and what is new? With three runs, say plainly what is
   stable and what could be variance.
4. **Self-report reliability.** Did the agents disclose their own workarounds?
   Compare each report with its transcript for undisclosed detours. This
   decides whether "tell us where you got stuck" is a usable signal.
5. **Yield and noise.** Real bugs or guide fixes per run and per dollar, false
   "stuck" reports, and how long triage took.
6. **The plots question.** Did any run stall or guess at a step (QC gating in
   particular) for lack of the pictures a user sees? State what the evidence
   says about whether the plot-rendering work
   (`AGENT_PLOTS_SINGLE_ENGINE_PROMPT.md`) is on the critical path or can
   wait. Do not design it here.
7. **The blackboard.** Find out what blackboard scoring is in the code (what
   writes entries, what scores them, how the agent reads them, and whether
   entries persist between runs). From the trial run and the new runs, show
   which entries the agents followed, which they ignored, and whether any
   entry changed what the agent did at a guide step. Say what the evidence
   shows about whether the blackboard helps, hides guide problems, or both.

## Design question: supervision, launch and scheduling

**Supervision (the maintainer's lean, to confirm or challenge):** the run
itself is unsupervised, and the maintainer reads the report afterwards. Do not
add an LLM supervisor, and do not nudge the agent past a wall mid-run, because
the point is to see where it gets stuck. The wrapper around it stays thin and
mechanical: fresh copy of the fixture (real data never touched), cost cap and
timeout, teardown of worktrees and processes on failure, transcript plus the
"where I got stuck" report saved, cost recorded even when a run aborts, and a
flag when no run has happened for about two weeks so silence is not mistaken
for "no bugs." The weekly judge already runs as a timer through `cron_pass.sh`;
reuse that shape where it fits. Note that the old eval supervisor cost about
$11 a week and 6.4k lines, and was dropped; this must not grow into that.

**Launch and scheduling:** decide how a run is started. Options:

- **A. On demand, with an optional start time.** One command, for example
  `pixi run guide-smoke` (run now) or `pixi run guide-smoke --at "23:00 2026-10-06"`
  (run once at that time, e.g. overnight so it does not compete with the
  maintainer's own GPU use). If guides, run count or cost cap are not given as
  flags, it asks interactively which guide workflows to test and how many runs.
  Flags make it non-interactive, so a timer or script can call it.
- **B. Regular timers or cron jobs only.** A config lists guides and cadence;
  a timer runs them like the weekly judge.
- **C. Hybrid.** The same command serves both: manual runs with flags or
  prompts, and a timer that calls it non-interactively with a default set
  (for example guides changed since the last run, plus a slow rotation).

The maintainer's lean: start with A and run by hand while the first few runs
show what actually happens, add the timer from C only once the reports look
usable. Confirm or challenge that, and answer:

1. How should the guide set be chosen when not specified (all, named, "changed
   since last run" derived from git changes to the guide or the code it
   touches, plus a rotation)? What does that cost per run and per week as
   guides multiply?
2. How are run count and a cost cap set, and what are sensible defaults from
   the data?
3. How is a one-off start time scheduled given the repo's rule that code runs
   on Linux, macOS and Windows? Is a cross-platform scheduler needed, or is
   this a developer tool where one platform's mechanism (and "run now" plus
   an external timer) is enough? Check what the judge timer does today.
4. What must be true for an unattended time-based run: machine awake, the
   backend and preview worker available, no clash with the maintainer's GPU
   work, one run at a time or a concurrency limit?
5. Where do results go, and how does the maintainer find out a run finished or
   failed?
6. **Blackboard on or off.** The setup dialogue (and a flag for non-interactive
   calls) should ask whether the agent uses blackboard scoring. Think through
   what each choice measures:
   - With the blackboard off, the run tests the guide, MCP and API path alone.
   - With it on, the agent gets extra help, so a blackboard entry may paper
     over a guide gap and hide the very friction a guide smoke test exists to
     find. It may still be the right arm for testing the blackboard itself.
   - If entries persist between runs or are written by earlier agents, runs
     are no longer independent, which matters for any N-run comparison.
   Recommend whether the two arms should be run separately, never pooled in
   one result, how the flag is recorded in every run record, and a default.
   Check in the code and the trial run what the blackboard actually does
   before answering; do not assume.

## What to produce

1. **Run findings** (items 1-5 above), per run and in aggregate, with costs.
2. **Scope statement.** What these runs are for and not for. At minimum:
   - the job (smoke test of the guide, MCP and API path) and what it cannot
     see (GUI friction, biological correctness)
   - what pass and fail mean
   - where they sit relative to recital, the weekly judge, the mechanical
     checks (ratchets, a check that every guide's `fun_name` exists in the REPL
     API), and the ground-truth scoring on seeded fixtures
   - cadence and cost as guides multiply (for example run a guide when it or
     the code it touches changes, plus a slow rotation) with a per-guide cost
     estimate from the data
   - **a difficulty ladder.** Easy, clean datasets isolate platform and guide
     bugs; harder data (noisier, dimmer, drift, other image types) will mix
     in genuine data difficulty and agent mistakes, which makes attribution
     harder. Recommend how to add complexity (one variable at a time, each
     tier's pass/fail kept separate, easy tier kept as the cheap baseline
     that reruns on every change) and how to stop biological hardness from
     being logged as a bug.
   - how output reaches the maintainer: free-text "stuck" list read by a
     person, versus a finding type feeding the judge. Recommend the minimum
     that the evidence justifies and say what repeated pattern would justify
     more structure.
3. **Launch and scheduling design** (see the design question above): a
   recommendation among A / B / C, the command shape and flags, how the guide
   set, run count and cost cap are chosen, how a start time is scheduled, and
   the thin wrapper's responsibilities, and the blackboard on/off option
   (default, separate arms, flag recorded per run). Keep it as small as the evidence
   allows, and say what would justify adding the timer.
4. **What to drop or keep from the old overnight ambition.** List the parts of
   `AGENT_OVERNIGHT_PLAN.md` and `scripts/agent_eval/*` that only serve
   "perfect unattended analysis" or agent-only optimization, and recommend
   keep / drop / not-yet for each, naming the evidence. Do not recommend
   deleting anything the runs did not show to be unneeded.
5. **Plots verdict** from item 6: critical path, nice to have, or not needed
   yet, with the evidence.
6. **Worth-it verdict:** continue as guide smoke tests / continue in reduced
   form / pause / stop. The maintainer has said that if this approach does not
   work, the autonomous runs should be given up. So define success and failure
   now, from the data: a budget (number of runs, cost cap, time window), what
   counts as success (completes the guide with few detours, or surfaces real
   bugs not otherwise found), and what counts as failure (nothing real found,
   or triage costs more than the fixes are worth). State where the current
   evidence puts it.
7. **What remains unknown.** Three runs is not statistics. Say what would need
   to happen before the scope is treated as settled.
8. **Final step, only if the system is kept: a monitor.** A
   `pixi run smoke-console` to see what is scheduled, what ran, and how each
   run went (guide, blackboard on/off, outcome, cost, the stuck list). Give
   only the rough direction: what it needs to show, and whether it should
   extend the existing recital console instead of being a new tool. Its shape
   depends on what the smoke tests turn out to be, so do not specify it in
   detail, and skip it entirely if the verdict is pause or stop.

## What NOT to do

- Do not launch new agent runs, build new infrastructure, or add schemas
  without repeated evidence that they are needed.
- Do not propose optimizing the app for agent use beyond what the guides and
  tools need for people too.
- Do not score biology or merge friction findings with ground-truth results.
- Do not treat three runs as proof of a rate. Report what happened.
- Do not assume the agents' reports are accurate; check against transcripts.
- Do not default to justifying the runs because they were proposed. "Reduce"
  and "stop" are valid answers.

## Output format

- Run findings first (per run, then aggregate), with costs.
- Scope statement.
- Launch and scheduling design (A / B / C, command shape, wrapper duties).
- Keep / drop / not-yet list for the old overnight ambition.
- Plots verdict.
- Worth-it verdict with the explicit success and failure definition.
- What remains unknown.
- Monitor (smoke-console): rough direction only, and only if the system is kept.
