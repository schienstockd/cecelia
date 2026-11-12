> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. The
> current design lives in [`docs/MODULES.md`](../MODULES.md) (`purpose` / `useWhen` / `notWhen`) and
> [`docs/todo/TASK_DISCOVERY_PLAN.md`](../todo/TASK_DISCOVERY_PLAN.md).
>
> **Outcome:** not run as of archiving (2026-11-12, `origin/main` a481c737a). Read against main, most
> of what it proposes is already built, and four premises had moved:
>
> 1. **The block is not always open.** It has been closed by default since #2142 (GUI layout M4,
>    merged 2026-11-12), under a fresh key (`cc-task-about-open-v2`) so the default reaches everyone.
> 2. **Steps 3.1, 3.2 and 3.5 are built.** The one-line `purpose` sits under the picker and in the
>    picker's tooltip (2026-10-07). A line can be `{text, check}`, and the check runs advisory against
>    the selected images (`TASK_CHECKS` in `frontend/src/utils/taskDiscovery.ts`, 8 checks, never
>    blocks Run). The specs are the single source for the page and for the MCP `task_catalogue`.
> 3. **The size is smaller than stated.** 58 tasks carry lines: 99 use-when and 101 not-when, median
>    3 per task. Only Denoising has 6, and `docs/MODULES.md` caps the lists at 1–3 and 0–3. 14 of the
>    200 lines already carry a check.
> 4. **The data is task-spec JSON (`app/src/tasks/*/*.json`), not a Params struct**, so "versioned like
>    other Params structs" does not apply. Maturity tiers belong to pages (`lib/guides/tiers.ts`), not
>    functions. "Experimental" in the example is the page's tier.
>
> Still open from the brief: classifying the unchecked lines, and checks with no `TASK_CHECKS` entry.
> In the brief's own example, "photon-limited" has a QC signal (`photon_limited` in
> `app/src/qc/text.jl`) but no check, and "movie shorter than the model window" has neither.

# WHEN_TO_USE_TIGHTEN_PROMPT

**Model:** Opus. **Mode:** audit and proposal. No code changes in this pass.
**Output:** `docs/todo/WHEN_TO_USE_TIGHTEN.md` (about 2 pages, with a before/after sample).

## Problem

Each task function shows an always-open **When to use** block: a "Use when" list and a "Not when" list, often 6 or 7 bullets
(example: *Denoising (SUPPORT), Experimental*). It reads as rules for an agent, not guidance for a person.
It adds density above the controls, most people will not read it, and some of its rules are things the software could check and show itself.

The data is valuable. It feeds agents, recipe and correction logic, and docs. The **presentation to a person** is what needs tightening.

## Principles

Same as the GUI plan: light and loose surface, firm skeleton, nothing important buried, prose a person must read before acting is cut or converted to a check.
Cecelia works fully without Claude, so the person-facing version must stand on its own.

## Method

### 1. Inventory

List every function that has a When to use block (task, function, maturity tier, number of Use-when and Not-when bullets, whether it is
shown open, collapsed or not at all). Find where the text lives (a single source or scattered), who consumes it (agents, recipes,
the correction decision tree, guides, docs) and what format it has. One table.

### 2. Classify every bullet

For each bullet, mark exactly one:

- **Checkable:** the software can evaluate it from image metadata, task history or project state (channel saturated, movie shorter than the model window, no trained model yet, drift correction not run).
- **One-line summary material:** states what the function is for.
- **Warning worth reading:** a "Not when" that stops a wrong or wasteful run and cannot be checked.
- **Reasoning:** explains why; useful but not needed before acting.
- **Agent-only:** useful as a rule for a decision-maker, not for a person.

Report counts per class across all functions. This sizes the change.

### 3. Propose the person-facing form

Evaluate and recommend one design. Start from this and challenge it:

1. **One line** under the function title: what it does and for what ("Removes shot noise from sparse, low-count movies").
2. **Checkable rules become live status** in the checks cluster, at the moment they matter, each with one action
   ("No model yet. Train one", "Channel 2 is saturated. Skip denoise", "Run drift correction first"). Advisory, never blocking.
3. **Unchecked warnings** (at most two per function) stay as short text near Run, or in the checkpoint.
4. **Reasoning and the full lists** sit behind a collapsed "More about this function", also reachable from the guide step.
5. **Agent-facing data is unchanged** and remains the single source; the person-facing form is generated from it, not a second copy.

Consider alternatives: tooltip on the function picker, a "Is this right for my data?" check that evaluates the rules and answers in one line,
and function-picker hints. State when each wins.

### 4. Specify the data

Propose the smallest data change that supports this: a one-line summary field, a `check` reference on rules that can be evaluated
(which existing QC measure or metadata field backs it, or "needs new"), a severity, and a `warning` flag for unchecked "Not when".
Show the schema change and how existing entries migrate. Keep it versioned like other Params structs.

### 5. Before and after

Rewrite three functions as samples: the Denoising (SUPPORT) block shown above, one segmentation function, and one that is mostly agent-only.
Show the old block, the new person-facing view (one line, flags, collapsed more) and which bullets moved where.
Count visible lines before and after.

## Output format

1. **Inventory and classification table** with counts.
2. **Recommended design** in a short paragraph, with the strongest reason against it.
3. **Data change** and migration path.
4. **Three before/after samples.**
5. **Checks to add:** each checkable rule, the signal behind it, existing vs. needs new, and where its flag appears.
6. **Sliced PRs:** small, ordered, each reviewable by eye; mark which need eyes on the real panel.
7. **Decisions needed from D**, each with your recommended answer.

## Rules

- Evidence from the repo: file paths and counts; label anything inferred.
- Do not remove information that agents or recipe logic rely on; change presentation first.
- Checks are advisory and never block a run.
- Reuse the shared check component and panel sections; no new primitive unless none fits.
- Wording for people: plain, short, no jargon where a simpler word works; same term as elsewhere in the app.
- If a rule cannot be verified from the code or data, do not turn it into a check; keep it as text and say so.
