> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in `docs/todo/TASK_DISCOVERY_PLAN.md`.
>
> **Outcome:** not run as a brief. On 2026-10-07 Claude checked its concerns against the code with
> Dominik in chat. Several facts here were already stale: #1483, #1476, #1485 and #1486 had merged, the
> GUI reads the fields in #1492, and `seg.*` findings (#1479) are part of the `--discovery` bundle. The
> on/off measurement is the intravital guide-run pair of the 2026-10-07 night. Read
> [`docs/todo/TASK_DISCOVERY_PLAN.md`](../todo/TASK_DISCOVERY_PLAN.md).

# Review the task discovery fields (PR #1483) and decide their user-facing shape

**Model:** Opus.
**Type:** Review + evidence check + recommendations. Read the code, the PR and
the plan; analyze existing runs. Do not implement, and do not launch new agent
runs unless you can justify the cost. Every recommendation must cite what you
found.

## Context

PR #1483 (open) implements P1 of `docs/todo/TASK_DISCOVERY_PLAN.md`: all 50
visible task specs gain `purpose` (one line), `useWhen` (1-3 lines) and
`notWhen` (0-3 lines), plus a ratchet in `app/test/suite/task_spec_ratchets.jl`
that enforces form (80 characters, sentence case, one sentence, no trailing
period). The goal is to give the agent a better chance to reason about which
function (task) to use. The PR lists every line with its source for redlining
and asks the maintainer to rule on six items:

1. Smoothing vs AF correction order (sources disagree; no order line written).
2. Stack alignment vs drift correction order (open in CORRECTION_QC_PLAN
   §Q-C1; no order line written).
3. Cellpose's `notWhen` pointer ("Noisy or drifting images: fix them in
   Cleanup before tuning this"). Sourced from guide copy and the plan; nothing
   measures it. The midnight run is meant to test it.
4. Flow-register "can mistake cell motion for distortion" (unmeasured plan
   motivation).
5. Denoise "Static structure channels" (an untested risk in the plan).
6. Whether `carry_over_snapshot`, `carry_over_restore` and the two staleness
   reports should be `hidden`, since they only make sense inside composites.

Related: #1476 (MCP `get_task_catalogue`, discovery fields, a
`CECELIA_MCP_DISCOVERY` reader), #1437 (guides given to the agent), PR #1485
(`pixi run guide-run <guide> --discovery on|off`, default on: writes
`CECELIA_MCP_DISCOVERY` into both MCP servers' env; the observer then strips
the spec fields and leaves `get_task_catalogue` unregistered, and the
autonomous server switches the recommender's `evidence=metadata`; the arm is
recorded in `record.json`, the run's Blackboard title and every
`guide-runs.jsonl` line), and the autonomous guide runs, which are being
reframed as proxy-user/smoke tests (see `AGENT_RUNS_SCOPE_PROMPT.md` if
present). #1485 depends on #1476 being merged.

## Principles to hold

- **Build nothing until a real run shows it is needed**, and do not optimize
  the app for agents beyond what helps people too.
- **Cheap code, expensive verification.** Copy is cheap to write and costly to
  keep correct. Judge the fields on what verifies them, not on how easy they
  were to add.
- **Keep experimental arms separate.** Never pool runs with and without a
  feature in one result.
- **A clean outcome is not a correct one.** The agent choosing the intended
  task does not prove the analysis is right.

## The concerns to check (the maintainer's assistant raised these; verify each)

1. **Who reads the fields?** The PR says `/api/tasks/definitions` passes the
   whole spec through and that the MCP and GUI read the new fields without
   further plumbing. The PR's own convention check found nothing in `app/src`,
   `api/src`, `python/cecelia` or `frontend/src/tasks` that reads these keys.
   Establish exactly what consumes them today (MCP via #1476? the GUI task
   picker? anything else). If only the agent consumes them, say so: it is
   agent-only optimization, which the principles above discourage, and the
   remedy may be surfacing the same text to people in the GUI.
2. **A wrong `notWhen` costs more than a missing line.** A missing line leaves
   the agent where it was; a wrong `notWhen` steers it away from a correct
   task. For each of the six rulings, and for any other line that rests on
   unmeasured claims or on guide copy, judge the risk. Recommend keep, rewrite
   neutrally, or drop, and say what measurement would settle it. Treat the
   maintainer as the domain authority on biology: state your view and the
   evidence, do not overrule the maintainer's call.
3. **The two order rulings.** Writing no line is right when sources disagree.
   Find where the agent would get the order instead (recipes in
   `frontend/src/lib/guides/recipes.ts`, guide text, `correction_plan.jl`), and
   whether it is stated clearly there.
4. **Hiding composite internals (ruling 6).** Check whether `hidden` specs
   still run inside composites and chains (`copyImage` and `remove` are
   existing precedent), what else `hidden` affects (GUI chain editor,
   catalogue, ratchets, tests), and whether hiding is safe.
5. **Staleness.** About 50 tasks times roughly 5 lines of copy is a lot of text
   that can go out of date when methods change, and the ratchet checks form,
   not truth. Is there any mechanical way to catch drift (for example a check
   that task labels or tasks named in a `notWhen` still exist), or is the
   guide-run feedback the only check? Do not propose heavy machinery; say what
   is cheap and whether it is worth it.
6. **Does it work?** The discovery fields are only worth keeping if the agent
   picks better tasks with them. Check the existing run records and
   transcripts: did agents choose wrong or unnecessary tasks that these lines
   would have prevented, or choose well without them? Did any run show a
   misleading line causing a detour (the cellpose cleanup pointer is the one
   being tested)? The on/off switch exists (#1485), so compare arms run at
   the same code version on the same day; runs from before the fields existed
   are not a clean "off" baseline because other fixes landed in between.
   Note that "off" toggles a bundle: the spec fields, the
   `get_task_catalogue` tool, and the recommender's evidence. A win for "on"
   therefore cannot say which part helped, and cannot settle the copy rulings
   line by line. Say what the arm can and cannot show, and whether a finer arm
   is justified. Keep arms separate from the blackboard arm and from each
   other, and compare which tasks the agent chose and where it got stuck, not
   only completion.
   One caveat for the arm: PR #1475's own reservation says the intravital
   guide-run crops carry no `meta.saturation`, so the photon-limited score is
   absent and the recommender gives the same answer on both arms until
   those images are re-imported (backfill is #1486, open). Until then the
   recommender part of the discovery bundle is a no-op on those runs, and
   the on/off comparison measures only the spec fields and the catalogue
   tool. Check the run records for whether that was the case.
7. **The ratchet and the PR's own reservations.** The convention check flagged
   a potential duplicate: the new copy-rule regexes differ from the existing
   tip checks (`multi_sentence`, `ABBREV`, the house-style trailing-period
   check), so the same rule would accept a tip and reject a discovery line.
   Judge whether sharing the helpers is worth doing now or can wait.

If the evidence supports it, classify what you find with the proxy-user labels:
guide gap / platform hid it / agent's mistake, and say which kind of failure
these fields address.

## User focus: a task dependency schematic

The maintainer's idea: users also need to see how tasks relate (order,
prerequisites, what each task produces and consumes) as a schematic they can
follow, built from the same task data, so the discovery work serves people and
not only the agent. The unresolved order rulings (smoothing vs AF correction,
stack alignment vs drift correction) point at the same gap: order is where the
sources disagree and no line was written. Evaluate this as a question, not a
build. Gate any build on evidence of user need.

0. **Prior art: the Cleanup module page.** The maintainer already tried
   something like this for the Cleanup module page. Start there. Find it (the
   Cleanup page in `frontend/src`, `docs/todo/CORRECTION_QC_PLAN.md` and any
   related plan or PR) and establish: what was built (ordering, prerequisites,
   a diagram, guidance text?), what data it is driven by (derived from specs or
   hand-written), how it behaves today, and what it cost to build and keep
   correct. Did it help users or the maintainer? What went wrong or was left
   undone? Treat it as the real evidence for or against the schematic idea, and
   as the template for what a wider version would involve. If it is not
   documented how well it worked, say so and say what would show it.
   Likely relevant pieces (verify which one the maintainer means): the
   correction-plan recommender and its panel; PR #1475 (merged: a
   photon-limited score from the import histogram now includes smooth and
   denoise on photon-limited data and excludes denoise below a lower band,
   with a stated reason, and a card seeded step always outranks the score;
   `evidence=metadata|all` switch); PR #1486 (open: backfill
   `meta.saturation` on demand); `TASK_DISCOVERY_PLAN.md` (P3); and
   `CORRECTION_QC_PLAN.md` (open order questions Q-C6/Q-C7: should a blind-spot
   denoiser see smoothed input?). The plan's own motivation is that in the
   2026-10-05/06 guide runs every agent tuned segmentation on uncleaned images,
   which is direct evidence of a "what step comes first" failure by a proxy
   new user. Read those runs to see exactly what happened.
1. **Is there a user gap?** What evidence exists that users pick the wrong task
   or the wrong order (the maintainer's experience, guide copy, the agents'
   stuck lists as a proxy for a new user, the CORRECTION_QC_PLAN)? Do not
   assume it.
2. **Derive before hand-writing.** What do task specs already declare (inputs,
   outputs, accepted population types, value names, `carry_over` steps, the
   chain and composite structure, recipes, guide panels)? Could a prerequisite
   graph (task A writes X, task B reads X) be derived mechanically? Which
   ordering is soft ("usually before") and cannot be derived, how many such
   edges are there, and which are disputed? Hand-written edges need a source
   and a ratchet; derived edges stay correct by construction.
3. **Relationship to what exists.** Recipes and guides already encode typical
   order. Would the schematic duplicate them, or be a view over recipes plus
   derived edges? Prefer a view over a new artifact.
4. **Smallest useful form.** For example "needs / produces / usually before"
   on each task in the picker, or a per-guide overview, before any full graph.
   Say where users would see it (task picker, guide, chain editor) and what the
   existing GUI components could already host.
5. **The agent side.** Would the same data feed the MCP catalogue? If so,
   treat it as a possible future arm of the discovery experiment, kept
   separate, only if the first discovery result justifies it. Do not design it
   now.

State a verdict: build the smallest form / derive first and revisit / not yet
(no evidence of user need) / not worth it.

### Author's prior on the schematic (a hypothesis to challenge, not a conclusion)

Written by the assistant that helped draft this prompt, without having read
the task specs. The schematic was only an idea from the maintainer, so state
plainly where you agree or disagree and why.

**Lean: not yet, or derive first and revisit.**
- **There is no user evidence yet.** The only pointers are the two disputed
  order rulings and the guide runs, which test an agent, not a new user. A
  schematic built before a user is shown to get lost is the same pattern as
  the eval suite that was dropped.
- **The valuable edges are the ones that cannot be derived.** What a task
  reads and writes is probably derivable and probably thin. The ordering that
  actually trips people ("smooth before or after AF correction") is soft and is
  exactly where the sources disagree. A drawn schematic must either pick a
  side or show false certainty. Writing it down before the question is settled
  would encode a guess.
- **Recipes and guides may already carry order**, in the form a user follows.
  A second artifact beside them risks drifting from them. If anything is
  built, a view over recipes plus derived "needs / produces" is safer than a
  new graph.
- **Cost lands later.** Hand-written edges need sources, a ratchet and upkeep
  as methods change, which is the staleness problem already raised for the
  discovery copy.

**What would change my mind:** the maintainer or users getting the order or
the task choice wrong in practice; the derived graph turning out richer than I
expect; the guide runs' stuck lists, or the discovery on/off arm, showing that
task choice and ordering are where agents (as proxy new users) lose time; or
the chain editor already having structure that a view could reuse cheaply.

**Update:** the maintainer says something like this was already tried on the
Cleanup module page, and PR #1475 and the plan show that every agent in the
2026-10-05/06 guide runs tuned segmentation on uncleaned images. So my "no user
evidence yet" point is weaker than I wrote: a proxy new user did skip the
earlier step. Two things still hold. First, the fixes already chosen
(a `notWhen` line pointing to Cleanup, segmentation QC findings naming the
earlier step, and the recommender) target that failure directly, so measure
whether they suffice before building a graph. Second, the Cleanup recommender
suggests the right form for "what comes first": it says what to do given the
data and gives a reason ("not photon-limited, so denoising would remove
signal"), which a static dependency graph cannot do because correct order
depends on the data. My lean is therefore a data-driven, self-explaining
recommendation per module where a measurable score exists, and recipes or
guides elsewhere, not a general schematic. The Cleanup page's outcome should
outweigh this prior; say whether the recommender pattern generalizes to other
modules and where it cannot (no measurable score).

**What I am least sure of:** how much the specs declare today, how many soft
ordering edges there really are, and whether users get lost at task choice at
all. The investigation exists to settle these, so do not accept this prior
without checking it.

## What to produce

1. **Evidence found**, per concern above, with file paths and run references.
2. **Rulings view.** For each of the six rulings: your recommendation, the
   risk it carries, and what would settle it. Flag clearly which depend on
   biology only the maintainer can judge.
3. **Consumer finding.** What reads the fields today, and whether the GUI
   should show them too.
4. **Measurement plan, minimal.** Whether and how to compare discovery on and
   off, and what a result would have to look like to keep or drop the fields.
   Reuse the guide-smoke setup; add nothing new unless the evidence demands it.
5. **Worth-it verdict:** keep as is / keep with changes (list them) / keep a
   reduced set (name it) / revert. State the evidence behind it.
6. **User focus verdict** (the dependency schematic section above): the
   evidence of user need, what can be derived versus hand-written, the
   smallest useful form, and the verdict.
7. **What remains unknown.**

## What NOT to do

- Do not rewrite the copy lines or redo the PR; recommend changes.
- Do not overrule the maintainer on domain facts; give a view and the evidence.
- Do not hand-author a dependency graph that can be derived from specs, and do
  not design the schematic UI; the schematic question is the one exception
  to the next rule, to be evaluated and gated on evidence.
- Do not propose new infrastructure (schemas, monitors, judges) without
  repeated evidence that it is needed.
- Do not merge runs with and without discovery, or with and without the
  blackboard, into one result.
- Do not treat a few runs as a rate, and do not assume the agent's report of
  its own reasoning is accurate; check transcripts.
- Do not default to defending the PR because it exists. "Reduce" and "revert"
  are valid answers.

## Output format

- Evidence per concern.
- Rulings view (six items).
- Consumer finding.
- Minimal measurement plan.
- Worth-it verdict.
- User focus verdict (dependency schematic).
- What remains unknown.
