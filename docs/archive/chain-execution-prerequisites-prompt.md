> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

# Prerequisites for Chain Execution Rights — A Punch List, Not a Proposal to Grant Them — Cecelia

## Background

Per `mcp/README.md`, Claude currently **designs chains but never runs them** —
there is no `run_chain`/`submit_task` MCP tool, and launching a chain is a
WebSocket message with no HTTP route at all, so the MCP client (HTTP-only)
cannot reach it. The README states this plainly: a chain run's nodes replace
the store/h5ad for their `value_name`, so it would be the first MCP action
that can destroy results. That boundary is currently enforced at the
transport level — the strongest kind of enforcement available, because it
isn't a convention that can be silently bypassed (see: the zarr-writer bug,
the CSS-variant drift, `#915`'s batching — every prose-only "don't do X" in
this codebase's history has eventually been violated by exactly this kind of
gap).

Anthropic's own Model Hardware Standard (MHS) work is a useful comparison
point, not because Cecelia should copy it, but because it makes the actual
bar explicit: MHS's defensibility for autonomous *physical* hardware control
rests on two things, not one — (1) safety limits enforced at the **driver
level**, below and independent of the model, and (2) **active human
supervision** in real time, with Anthropic's own reporting stating plainly
that Claude's physical/spatial reasoning "still requires expert oversight"
(the Genentech bubble-misdiagnosis case is the concrete example of a human
catching a wrong call in time). Neither of those two things — a driver-level
hard limit, or a supervised/interruptible execution mode — currently exists
for Cecelia chain runs. The absence of an execution route is standing in for
both right now, by simply not existing.

**This document is not a proposal to grant execution rights.** It's a
concrete, checkable list of what would have to exist first for that decision
to be made responsibly later — separate from this task, by a human, with
these prerequisites actually in place rather than assumed. Building this list
does not commit to using it.

## Task

### Part 1 — Audit what exists today

- Read `chain.jl`, the scheduler (`scheduler.jl`), and whatever handles a
  chain-run's actual store/h5ad write to understand exactly what "replaces
  the store/h5ad for their `value_name`" means mechanically — is it a
  wholesale overwrite, an in-place mutation, something else?
- Check whether anything already resembling dry-run validation exists (params
  validated without executing — the typed-params arc's `parse_<task>_params`
  functions are relevant groundwork here, since they validate without
  running).
- Check whether real-time progress visibility already exists for a running
  chain (the WebSocket stream the MCP monitor reads from — does it carry
  enough detail for a human to actually watch and understand what's
  happening, or just coarse status?).
- Check whether any interrupt/abort mechanism exists for an in-progress
  chain run today (for a human using the Vue app directly, not for Claude).
- Check whether resource pools/quotas (mentioned in `mcp/README.md`'s
  scheduler context) already bound how much a single run can consume, or
  whether a run's resource cost is currently unbounded.

### Part 2 — The punch list

For each item, Opus should mark it **exists / partially exists / missing** based
on Part 1, and for anything missing or partial, propose the concrete
mechanical fix — not a convention, not a docstring, an actual enforced
mechanism, per the lesson of every prose-only rule that's failed in this
codebase so far.

1. **Non-destructive by default.** A run writes to a new `value_name`
   (or an equivalent versioned target) rather than overwriting the existing
   one — the same content-hashed-history pattern chain templates already use.
   This should be a structural default, not an opt-in flag someone has to
   remember to set.
2. **Dry-run validation.** A chain can be fully validated — params, resource
   availability, node graph — without executing, and this validation is
   mandatory before any execution path, not optional.
3. **A mechanically enforced "cannot silently destroy existing results"
   invariant** — not a comment, an actual check (e.g. a write path refuses to
   proceed if it would overwrite a `value_name` with existing data, unless
   explicitly and separately confirmed). This is the zarr-writer lesson
   applied here directly: state the rule in code, not just in a docstring.
4. **Bounded blast radius per run** — a hard resource/compute quota per
   execution, so a single run (autonomous or not) cannot silently consume
   unbounded compute or storage.
5. **Real-time supervised visibility** — a human can watch a run's progress
   as it happens, not just see a final status, with enough detail to
   recognize something going wrong before it finishes.
6. **An interrupt/abort control reachable mid-run**, not just an initial
   approval gate — supervision means being able to intervene during
   execution, the same way a human intervened in the Genentech case, not
   only before it starts.
7. **A complete audit trail per run** — what was approved, by whom, with what
   params, producing what output — tied into the lab-log (or an equivalent),
   so every execution is reconstructable after the fact.
8. **A clear, explicit classification of reversible vs. irreversible actions**
   within a chain, so both the system and a human supervisor know which
   steps warrant extra caution rather than treating every node the same.

### Part 3 — Report format

For each of the 8 items: status (exists/partial/missing), evidence from Part
1, and — for anything not fully done — a concrete proposal for closing the
gap, sized the way every other prompt in this project has been: additive,
reviewable in its own PR, not a single mega-change.

## Your own verdict

- Once this punch list is fully closed, would execution rights actually be
  defensible — i.e. does closing all 8 items genuinely get Cecelia to the
  same standard MHS claims for hardware, or is something still missing that
  this list doesn't capture?
- Is this whole effort premature? Given there's no established use case yet
  demanding autonomous chain execution (the honest answer from the ROI/
  autonomous-microscopy discussion was "no proven need yet, and CSS-drift is
  the correct prior"), is it worth building any of items 1–8 now, or should
  this punch list exist purely as documentation of the bar — something to
  return to only if and when a real need for execution rights actually
  materializes? Give a clear recommendation, not just the tradeoff.
- Which of the 8 items, if any, are worth doing **regardless** of whether
  execution rights are ever granted — i.e. which improve safety/robustness
  for human-triggered runs too, and so are good to build on their own merit
  rather than only as a prerequisite for something hypothetical?

Don't hedge this section to match the tone of the rest of the report — this is
the part meant to be read first.
