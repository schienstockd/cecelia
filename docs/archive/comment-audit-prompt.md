# Maintainability Audit — Cecelia

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

## Why this audit exists

This codebase has so far been written and maintained primarily by an AI agent
working with one person. That's been necessary, but it's a liability long-term:
the goal is for Cecelia to eventually be maintainable by a human community, not
permanently dependent on an agent holding context that lives only in prompt
history. This audit is a step toward that — finding the places where the
codebase currently assumes an agent (or the one person who's been prompting it)
is always in the loop.

Two different risks, both in scope here:

1. **Comments/docs that don't actually transfer knowledge** — verbose but hollow,
   or verbose and useful but not indexed for anyone (agent or human) to find.
2. **Implicit contracts** — behavior that's correct today only because an agent
   remembered an unwritten rule, not because it's enforced by a type, a test, or
   a written invariant. These are the dangerous ones: a human contributor (or a
   future agent without the same context) can violate them silently, and the
   failure won't show up until much later, somewhere else.

## Task

Audit code comments, docstrings, and cross-module assumptions across this
codebase for human-maintainability. Do **not** change logic, behavior, or naming
— flag and propose fixes for comments/docs, and separately flag (don't fix)
implicit contracts that need a test or a written invariant. Where a comment turns
out to be load-bearing (documents a correctness invariant, a lock ordering, a
race-condition guard), leave it alone or tighten it, but never delete it for
brevity.

Work file by file. For each file, produce:
- A short list of flagged comments/docstrings, each tagged with one of patterns
  1–3 below.
- A short list of flagged implicit contracts (pattern 4), each describing what's
  assumed, where it's assumed, and what would enforce it instead (a type, an
  assertion, a test, a schema).
- A proposed rewrite for each comment/doc flag.
- If a file is mostly fine, say so briefly and move on — don't manufacture
  busywork.

## Three failure patterns to check every file against

These are drawn from a review of three real files in this repo (`hmm.jl`,
`af_correct.jl`, `scheduler.jl`).

### 1. Reference to an absent artifact
A comment justifies behavior by citing a source that isn't in this repo — e.g.
"mirrors the R `hmmTransitions` truth table," "ports `caTools::runmean`," "matches
the R `drop_na`/`filter` behaviour including its quirk of X." Nobody reading this
repo can verify these claims, including a future AI agent, because the reference
doesn't exist here.

**Fix:** either inline the actual invariant in terms of this codebase's own
behavior (what does the function guarantee, independent of any R code?), or — if
the cross-reference is genuinely useful — keep the original source file/snippet
somewhere in the repo (e.g. `docs/reference/`) and link to it by path.

**Example (bad):** "Ties → smallest state. Ports the R postFiltering/postIterations
step (DescTools::Mode over a frollapply window, take-first on ties)."
**Example (good):** "Ties broken by smallest state ID, for reproducibility."

### 2. Incident report disguised as a docstring
A docstring narrates the debugging history of one specific run instead of
specifying current behavior: "This task used to be QC-exempt...", "The first run
reported 0.0248...", "Measured on `WIaUjL/p6t4mC`: 0.113 from CH3 into CH2...",
"It appeared — and it was a missing mechanism, not a missing warning." This mixes
three different things that belong in three different places:
  - **What the function does** → stays in the docstring (2–4 lines).
  - **Why a past design was rejected** → the codebase already has a convention
    for this: `docs/todo/*_PLAN.md` files include a "Locked decisions" section
    with the decision, the reasoning, and rejected alternatives (see
    `MULTI_POP_TRACKING_PLAN.md` for a good example). Move it there — into the
    plan for the feature this decision belongs to — rather than inventing a new
    `docs/decisions/` ADR folder. Don't propose a new convention for something
    this codebase already does.
  - **A specific debugging incident, dataset ID, or one-off measurement** → move to
    a changelog or commit message, not source.

**Fix:** trim the docstring to current behavior and its real invariants. Move
narrative/historical content into the relevant `docs/todo/*_PLAN.md`'s "Locked
decisions" section (or `CHANGELOG.md` for pure incident history) if it's worth
keeping at all.

### 3. Correctness-critical comment (leave alone / protect)
Not a failure pattern — the opposite. Comments explaining lock ordering,
cancellation races, silent-failure contracts across thread/process boundaries
("never nest `_TASKS_LOCK` inside `_POOLS_LOCK`", "posting happens in a `finally`
because a throw here is otherwise silent and strands the submitter"). These are
often verbose but each sentence is doing real work protecting a human (or an
agent) from reintroducing a bug.

**Do not trim these for brevity.** If a file is dense with this kind of comment
(e.g. concurrency code, anything with locks/races/atomics), flag it explicitly as
**protected** — add a one-line header note like:

```
# concurrency-critical — comments here document lock ordering and failure-mode
# contracts; do not trim for brevity without re-reading them.
```

and skip further edits to that file's comments unless something is genuinely
redundant (see below).

### 4. Implicit cross-module contract (flag, don't fix)
Behavior that's correct today only because someone (an agent, or you) remembers
an unwritten rule — not because it's type-enforced, asserted, or tested. Look
for:
- Module A assuming a specific shape/units/range from module B's output with no
  type, schema, or assertion backing it (e.g. "channel names arrive as a list of
  strings, already deduplicated" — is that guaranteed anywhere, or just true so
  far?).
- A comment that explains a caller-side workaround for a callee's quirk instead
  of the callee enforcing its own contract (e.g. "the target is dropped from its
  own competitor list" done defensively at the call site rather than validated at
  the source).
- Config or param dicts passed as loosely-typed `Dict{String,Any}` where a
  human — or a future agent — has no way to discover the expected keys except by
  reading every call site.
- Anything whose correctness depends on call order that isn't structurally
  enforced (function B must be called after function A, with nothing preventing
  the reverse).

**Do not fix these in this pass.** Flag them as a list — this is a risk register,
not a refactor. For each: what's assumed, where, and what would make it
self-enforcing (a stricter type, an assertion at the boundary, a test that would
fail if the assumption broke).

### 5. Monolith pressure (flag, don't fix)
The codebase is going to keep growing. Flag places where new functionality would
naturally get bolted onto an existing file/module rather than landing in a
clear seam, because that's how a monolith accretes — not from one bad decision,
but from many reasonable-looking additions each one file over. Look for:
- Files already doing more than one job (a task's run logic + its QC logic + its
  Python-param translation all in one file, e.g. `af_correct.jl`) — growth here
  usually means "add another finding function" to an already-mixed file rather
  than a new module.
- A module boundary that exists in name only — e.g. two files that both reach
  into each other's internals (non-exported state, `_`-prefixed helpers) rather
  than talking through a small public interface. New functionality tends to
  deepen this coupling instead of respecting it.
- A single generic container type (`Dict{String,Any}` params, a catch-all
  registry) that many unrelated features are likely to keep extending, instead of
  each feature owning its own typed shape.
- Any place a comment says or implies "for now this just handles X" — a likely
  landing spot for the next feature to be wedged in rather than given its own
  home.

**Do not fix these in this pass.** Flag them as a list, each with: the file/
seam in question, why it's likely to attract undifferentiated growth, and what a
cleaner seam would look like (a new module, a typed interface, splitting run/QC/
param-translation responsibilities apart) — so this becomes an input to planning
future features, not a refactor to do right now.

### 6. Navigability (the most immediate problem — flag, and produce the map)
Right now, finding where anything lives requires asking an agent first. That's
the most immediate, personally-felt problem this audit should fix — more urgent
day-to-day than 1–5 above. While reviewing each file, note:
- What would a person naturally search for to land here? ("where's the AF
  correction QC logic," "where do resource pools get configured")
- Is the file/function name and location predictable from that instinct, or
  would a newcomer have no reason to guess this is where it lives?

Produce `docs/MAP.md`: a plain-language index organized by *task a person wants
to do*, not by module name — e.g. "Want to change how a task reports QC
findings? → `tasks/<name>/<name>.jl`, the `*_qc_findings` function" — covering
the major things someone would want to change (add a task, change scheduling
behavior, adjust a segmentation pipeline, etc.), not an exhaustive file listing.
This is the single deliverable most likely to reduce how often you need to ask
an agent just to locate something.

## What's still fair game even in a "protected" file

- Comments that repeat the *same* invariant more than once in slightly different
  words (three restatements of "why not a `try/catch` probe" when one paragraph
  would do). Consolidate, don't delete the invariant.
- A docstring that bundles several independent field explanations into one
  undifferentiated block — reformatting into a short per-field list (one line
  each, justification only where non-obvious) is fine and doesn't lose content.

## Output format

For each file touched:

```
### path/to/file.jl
- [pattern 1|2|3] <location> — <what's wrong> → <proposed fix>
- [contract] <location> — <what's assumed> — <what would enforce it>
- [monolith] <seam> — <why growth would land here> — <cleaner seam instead>
- [nav] <what someone would search for> — <where it actually is / should be>
...
Verdict: <normal pass | protected — concurrency-critical>
```

Then a final summary:
- Total files reviewed, count by comment pattern (1–3), and a short list of any
  files flagged as fully protected.
- A separate **risk register** of all pattern-4 implicit contracts found,
  ranked informally by how silently they'd fail (a wrong-shape dict passed
  straight to Python with no validation ranks higher than a redundant but
  harmless assumption). This list is the actual "what breaks first if I'm not
  in the loop" answer — treat it as the priority list for follow-up work
  (tests, assertions, type tightening), not something to act on in this pass.
- A separate **structure register** of all pattern-5 monolith-pressure points
  found, so future feature planning can route new work to a clean seam instead
  of the path of least resistance.

## Standard to carry forward

A one-time cleanup doesn't help if the next feature (agent-written or
human-written) drifts back into these same patterns. So the audit's real
deliverable is not just fixed files — it's a short, durable standard that future
work gets checked against.

Produce `docs/MAINTAINABILITY.md` (or fold into `docs/MODULES.md` if that's
already the convention) containing:
- **Comment/docstring rules**, distilled from patterns 1–3: what a docstring
  should and shouldn't contain, where historical/debugging narrative goes
  instead (a `*_PLAN.md`'s "Locked decisions" section, or `CHANGELOG.md`), and
  how a correctness-critical comment should be marked so it isn't mistaken for
  clutter later.
- **Contract rules**, from pattern 4: when a new cross-module assumption needs a
  type, an assertion, or a test instead of just a comment — with the audit's own
  findings as worked examples of what "just a comment" looks like when it fails.
- **Structure rules**, from pattern 5: a short guide to where new functionality
  should go — e.g. "a task's run logic, its QC logic, and its param translation
  are three responsibilities; a new one belongs in its own function/file, not
  appended to an existing one" — using this repo's actual seams as the
  reference, not generic advice.

Keep it short enough to actually get followed — a checklist, not an essay. Its
job is to be the thing checked *before* new code is written (by you, an agent,
or a future contributor), not something referenced only after a problem is
found. Treat it as living: findings from future audits should update this doc
rather than spawning a new one each time.

## Your own verdict

After completing the audit, give your own honest assessment — not just the
structured findings above. Specifically:

- Is this codebase actually on a path to being maintainable by a person who
  isn't fluent in the project's history, or is the gap bigger than a comment/
  contract/structure pass can close?
- Of everything you found, what are the 3–5 highest-priority fixes if the goal
  is "a competent Julia developer, unfamiliar with this project, can make a
  small change safely within a day of onboarding"? Rank by actual impact, not
  by how many instances of a pattern you found.
- Say plainly if you disagree with any of the five patterns above, or think
  something more important is missing from this audit's scope.

Don't hedge this section to match the tone of the rest of the report — this is
the part meant to be read first.

## README needs to say this out loud

Right now the README (implicitly or explicitly) reads like the process is "Dominik
and Claude, indefinitely." That has to change in the README itself, not just in
internal docs nobody outside the project would think to look for.

Add a section to `README.md` — visible near the top, not buried — that states:
- This project is being actively made maintainable by people other than its
  original author, not just by an AI agent. Say this plainly; don't imply it.
- Where to actually start: link `docs/MAP.md` (pattern 6) for "where things
  live," and `docs/MAINTAINABILITY.md` (the standard above) for "what's expected
  of a change."
- Any concrete guidelines a human contributor needs that don't currently exist
  anywhere: how to run tests, what a PR is expected to include, who/where to ask
  questions, what parts of the codebase are safe to touch vs. flagged
  concurrency-critical (pattern 3) or mid-refactor (pattern 5's structure
  register).
- If some of this doesn't exist yet (a contributing guide, a test-running
  command, an issue template), say so directly in your final report rather than
  inventing content for the README — flag it as a gap for a follow-up pass, the
  same way pattern-4/5 findings are flagged rather than fixed.

This section of the README is the actual test of whether this audit worked: a
stranger should be able to read it and believe a human community is genuinely
invited in, not tolerated as an afterthought to an AI workflow.
