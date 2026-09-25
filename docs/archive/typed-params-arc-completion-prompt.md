# Completing the ChainNode.params Typing Arc — Cecelia

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked and
> investigated at the time. It is not a description of how the code works now, and not instructions
> to re-run. Current design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

## Background

An arc of PRs (#906 `cleanupImages`, #909 `editImages`, #910 `tracking`, #913
`segment`, #914 `opticalFlow`) has been converting each task family's params
from untyped `Dict` access (`get(params, "field", default)` scattered through
`_run_task`) into a typed `Base.@kwdef struct <Task>Params` + a
`parse_<task>_params(::AbstractDict)` function that validates at run start and
throws `ParamValidationError` on malformed input. This is a direct, structural
fix for the implicit-contract risk the maintainability audit's pattern 4
flagged: a param dict with no compile-time guarantee of shape is exactly the
kind of thing that fails silently deep in a pipeline instead of loudly at the
door.

Each of those five PRs did **one task family at a time** — bounded, reviewable,
with family-specific quirks called out explicitly (e.g. #914's `trainChannels`
staying a resolved-at-runtime bag, `flowMetrics`'s `nothing`-means-"no picker"
semantics preserved deliberately rather than typed away).

**#915 broke that cadence** — it attempts to close the *entire remaining arc*
in one PR: `importImages` (15 tasks), `spatialAnalysis` (15), `behaviour` (14),
`clustTracks` (14), `clustPops` (13), `clustRegions` (8), `exportImages` (5),
`testTasks` (5) — roughly 89 tasks across 8 families, all at once. That's a
scope jump from what made the first five PRs trustworthy: small enough to
actually review, with room to catch a per-family quirk before it ships. A
passing test suite on an 89-task batch tells you nothing broke loudly; it
doesn't tell you every family's specific default/bag/nullable semantics were
preserved correctly the way a human reviewer catching `#914`'s `flowMetrics`
note would.

## Task

Two things. Do them in this order — the second is more important long-term
than the first.

### Part 1 — Finish the arc, but restore the one-family-per-PR cadence

For each of the 8 remaining families, do what #906–#914 already did:

1. Convert the family's task(s) from `get(params, "field", default)` calls
   scattered through `_run_task` into a `Base.@kwdef struct <Task>Params` and
   a `parse_<task>_params(::AbstractDict)` validator.
2. **Explicitly identify and justify any field that should stay untyped** (a
   "bag") — the way #914 did for `trainChannels` (resolved at runtime via
   `channel_indices`) and `temporalScales` (validated later, at run start, not
   at parse time). Don't silently type everything; some fields have real
   reasons to stay loose, and those reasons need to be stated in the code, not
   just known by whoever wrote the PR.
3. **Preserve nullable/sentinel semantics deliberately**, not accidentally —
   `flowMetrics`'s `nothing` meaning "no picker in this call" is exactly the
   kind of thing a mechanical Dict→struct conversion could silently break by
   defaulting it to something typed. Check every family for an equivalent
   sentinel value before converting it.
4. Add unit tests covering full-dict and empty-dict paths, matching the
   existing `app/test/suite.jl` → *typed params — `<family>`* convention.
5. **One PR per family**, same as #906–#914 — not one PR for all 8. If #915
   already exists as a single combined PR, close it or split it into 8 PRs
   along the same family boundaries listed above, rather than reviewing it as
   one unit.

For each family, in your PR description, call out explicitly (matching #914's
"Notable" section style): what stayed a bag and why, what sentinel/nullable
semantics were preserved, and what the test coverage actually checks — not
just "N new tests, 0 failures."

### Part 2 — Make sure this doesn't have to be remembered next time

This is the part actually worth this document existing. The five-then-one
pattern break in #915 is itself evidence that "follow the established
convention" doesn't hold on its own — it depends on whoever's driving the next
PR remembering to look at how the last one was done. That's exactly the kind
of dependency on memory (yours, or an agent's) that the maintainability audit
was trying to get the codebase away from. Fix it mechanically, not by hoping
it's remembered:

1. **Add a CI-enforceable check** that a task family's `_run_task` doesn't call
   `get(params, "...", ...)` directly outside its own `parse_<task>_params`
   function — a grep-based test is enough; it doesn't need to be clever. This
   should fail loudly if a future task (or a future AI-driven PR) reverts to
   untyped dict access, instead of relying on a reviewer noticing.
2. **Make that same check double as a progress punch list, not just a
   pass/fail gate.** The codebase already has a punch-list mechanism for
   CSS/UI conformance (find it — likely tied to `docs/ui/PRIMITIVES.md` or
   `docs/ui/COPY.md`'s "canonical catalog" framing) that presumably enumerates
   what's compliant vs. not, so progress can be tracked as a checklist rather
   than inferred from memory or from reading PR history. Mirror that mechanism
   for this arc rather than inventing a second tracking convention: the same
   scan that fails CI on a *new* violation should also be runnable standalone
   to print every task still on raw `Dict` access, so "how many tasks are
   left" is always a command away, not something to reconstruct from PR
   numbers. If the CSS/UI punch list has a specific format (a generated
   markdown checklist, a dashboard, a test that lists failures) reuse that
   exact format for consistency, and say so if it doesn't translate cleanly.

   **This isn't unique to typed-params — extend it to the `@enum` arc too.**
   The state-machine work from #900 flagged 6 instances of the same
   `Symbol`/`String`-as-closed-enumeration problem, split across planned PRs
   the same way typed-params was. It's the same mechanical shape: one repeated
   pattern, greppable (fields typed `::Symbol`/`::String` that are actually a
   closed set of states), countable, CI-checkable. Build the punch-list scan
   generically enough to cover both arcs — or as two instances of one
   mechanism — rather than a typed-params-only tool.

   **Be clear about where this mechanism does and doesn't apply**, since not
   every kind of finding fits it the same way:
   - *Fits directly* (mechanical, countable, CI-checkable): typed-params,
     `@enum`/state-machine — both are "one repeated pattern across N
     instances," the same shape as CSS/UI conformance.
   - *Fits partially* (can be *found* mechanically, but "fixed" needs a human
     read, not just a passing grep): the comment/docstring patterns from the
     maintainability audit. A scan can list which files still contain a
     flagged phrase, but marking one "done" because the phrase is gone doesn't
     verify the replacement is actually good — it could be gamed by deleting
     content rather than improving it. If you build this list, it must not be
     presented as "green = compliant" the way the CI-checkable ones can be —
     "flagged" and "reviewed-and-fixed" are different columns.
   - *Doesn't fit* (one-off, invariant-sensitive, no natural "done"): the
     structure-register splits (`task.jl`, `population_manager.jl`,
     `chain.jl`, `af_correct.jl`) and `docs/MAP.md`'s ongoing coverage. These
     are individually-reviewed projects, not repeated-pattern instances — a
     binary "split: yes/no" would hide the actual risk (a split that passes
     tests but breaks the `uid_index` invariant would show as "done" while
     being worse than not started). Don't force these into the punch-list
     format; track them as their own tickets instead.

3. **Check whether the existing 27+N unit tests per family would actually
   catch a regression** — i.e., would a test fail if someone reintroduced raw
   `get(params, ...)` access, or does it only test that the typed path works
   correctly without testing that the untyped path is gone? If the latter, add
   what's missing.
4. **Update `docs/MAINTAINABILITY.md`** with this as a named, standing rule —
   "every task's params are a typed struct + parser, not a raw Dict; bags are
   allowed but must be justified inline" — the same way the doc already states
   the `@enum`-over-`Symbol` rule from the state-machine findings. This is
   what makes the convention discoverable by the *next* person or agent
   without them having to reconstruct it from reading five old PRs.
5. **Update `docs/MAP.md`** with an entry under "Tasks & the scheduler" or
   "Analysis families": "Add a new task with typed params" → point at the
   convention and a canonical example family to copy from.
6. Note explicitly in your final report which of items 1–5 you actually
   completed versus which need a human decision first (e.g. if CI
   infrastructure for a grep-based check doesn't exist yet and needs to be
   set up as its own small piece of work).

## Your own verdict

- Is 8 families / ~89 tasks actually as uniform as the arc's framing suggests,
  or did you find families where the "bag" and "sentinel" exceptions are
  common enough that the typed-struct approach is fighting the domain rather
  than fitting it? Say so plainly if some family doesn't actually benefit from
  this pattern.
- Of Part 2's items, which is actually load-bearing versus nice-to-have? If
  you had to ship only one, which one, and why.
- Does the fits/partially-fits/doesn't-fit split above actually hold once you
  look at the real CSS/UI mechanism, or does reality split differently? Say so
  plainly rather than forcing the categories above if they don't match what
  you find.
- Is there a risk that "type everything" becomes its own accumulating
  monolith-adjacent problem — e.g. a `Params` struct per task times ~90 tasks,
  each slightly different, with no shared structure — the same way pattern 5
  (monolith pressure) flagged undifferentiated growth elsewhere? If so, is
  there a smaller number of shared param-shape abstractions that would reduce
  duplication without giving up the type safety.

Don't hedge this section to match the tone of the rest of the report — this is
the part meant to be read first.
