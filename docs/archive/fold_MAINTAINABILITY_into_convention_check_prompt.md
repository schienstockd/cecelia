> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in `docs/MAINTAINABILITY.md`, `docs/ai-assist/CONVENTION_CHECK.md`, and
> `docs/todo/*_PLAN.md`.
>
> **Scoped 2026-10-01** into [`docs/todo/MAINTAINABILITY_ENFORCEMENT_PLAN.md`](../todo/MAINTAINABILITY_ENFORCEMENT_PLAN.md).
> Several code pointers below were stale on arrival (scheduler split, enums already built, marker
> enumerations misnamed) — the plan's *Corrections* section lists them.

# Build: fold MAINTAINABILITY.md into convention-check + a new mechanical lint

**Model:** Opus for the design/prompt-editing work; the mechanical lint
itself should need no model call, following `inventory_coverage.py`'s
pattern (see below).
**Type:** Implementation, not audit. `docs/MAINTAINABILITY.md` and
`docs/ai-assist/CONVENTION_CHECK.md` are both attached as read-only context
— read both in full before editing either. Also read `#1297`
(`python/cecelia/effectiveness/inventory_coverage.py` and its PR
description) before building item 3 — it is the template, not just prior
art, for how the new mechanical check should be structured and validated.

## Why

`MAINTAINABILITY.md` states it is "the check that runs *before* new code
lands," but nothing currently enforces most of it — it's a reference
document, not a gate. A prior review sorted its contents into three buckets
by enforcement mechanism, not by content type. This task implements that
split. Do not re-litigate the split — it's locked in below. Implement it.

## Ground truth: the citation-currency retirement (#1297)

Before this task, `citation_currency.py` — an earlier mechanical recital
step — was replayed against 150 merged PRs and found to fire only 8 times,
catching 0 stale docs; half its intended job was already covered by CI's
`test_doc_pointer_convention.py`. It was retired outright, not kept as
dead weight. Its replacement, `inventory_coverage.py`, follows a specific,
now-established pattern:

- Advisory only, never blocks.
- Warns on two independently-replayed conditions (new undocumented shared
  file; new undocumented API route), each validated against real PR
  history before shipping (19/150 PRs and 30 files for the first; 12/14
  route-adding PRs and 24 routes for the second).
- A named opt-out marker (`INVENTORY-EXEMPT: <reason>`) explicitly modeled
  on the existing `COHORT-EXEMPT`/`DASK-OK` marker family — not a new
  exemption mechanism invented from scratch.
- Logs the specific flagged files/routes, not just a count.
- Emits a `_<Title>: <verdict>_` tail line matching the format the
  reviewer/hook chain already expects.

**Item 3 below (the new `maintainability_lint_run`) must follow this same
pattern and this same replay-before-trust discipline** — not the citation-
currency shape it replaced. Specifically: before wiring anything into the
commit hook, replay each of the four sub-checks in item 3 against merged
PR history (the same ~150-PR window #1297 used, or as much of it as is
practical) and report a hit rate per sub-check. Any sub-check that fires
near-never or never catches something real should be cut before it ships,
the same way citation-currency was — do not ship a check on the strength
of "it seems like it should catch things" without replay evidence.

## The split (locked in)

### 1. Into convention-check's existing `[should reuse]` marker — type-pattern reuse

These are genuinely the same question convention-check already asks
("does a canonical equivalent exist that this addition should have used
instead"), just applied to type shapes instead of functions/components.
No new marker needed — extend the existing reviewer's definition of
"canonical equivalent" and "addition-shaped hunk" to cover:

- **Cross-module contracts** (`MAINTAINABILITY.md` → *Cross-module
  contracts* → *The three specific triggers*): a new `Dict{String,Any}` (or
  `AbstractDict`) crossing a boundary (frontend↔Julia, Julia↔Python,
  API↔handler) without a typed constructor. Canonical equivalent to check
  for: an existing typed constructor pattern in the same boundary family
  (e.g. `AfCombinationSpec`, `AfChannelStats` are named examples in the
  doc).
- **Enums for state machines** (`MAINTAINABILITY.md` → same section): a new
  `Symbol`/`String` field with a known, small set of legal values, added
  without `@enum` + a typed setter. The doc names six existing instances of
  this exact anti-pattern (`TaskRecord.status`, `ChainNode.scope`,
  `ChainNode.barrier_policy`, `ImageNodeState.status`, `CciaImage.status`,
  `Population.pop_type`) as the fix template — use `TaskStatus` as the
  canonical pattern to point to.
- **Sum types over `Union{Nothing,T}` discriminants**: a new field using
  `nothing`-vs-value to discriminate a variant, where a sum type or a
  narrow accessor helper would remove the branch-everywhere burden.

Edit `docs/ai-assist/CONVENTION_CHECK.md`'s step 4 (grep instructions) and
the "Addition-shaped" definition to explicitly include: a new stringly/
symbol-typed field with an enumerable value set; a new untyped dict/struct
crossing a named boundary; a new `Union{Nothing,T}`-discriminated field.
Keep the existing marker vocabulary (`should reuse` / `potential
duplicate`) — do not invent a new marker for this bucket.

### 2. New marker in convention-check — narrative/reference placement

This is judgment-dependent (is this cross-reference genuinely load-bearing
per the doc's own bridging-code exception; does this comment restate
something that belongs elsewhere) but isn't a "reuse a canonical symbol"
finding — there's no symbol to point at, it's a placement violation. Add a
**third marker, `[wrong home]`**, to `CONVENTION_CHECK.md`, covering:

- **No cross-references to code outside this repo**
  (`MAINTAINABILITY.md` → same-named section): a comment citing
  `depmixS4`, `DescTools::`, "the R port," or any file/module not in this
  checkout. Check the doc's own exception list first (legacy migrators,
  published-algorithm ports like the celltrackR files, reference
  implementations kept alongside production code) before flagging —
  bridging-code references are load-bearing, not a violation.
- **Narrative content in the wrong place**
  (`MAINTAINABILITY.md` → *Where narrative content goes* table): a comment
  containing a rejected alternative + rationale (belongs in the area's
  `docs/todo/<AREA>_PLAN.md` *Locked decisions* section), a specific
  debugging incident/dataset/one-off measurement (belongs in `CHANGELOG.md`
  or the PR description, if anywhere), while load-bearing rationale for
  *this* choice over the alternative stays in source.

Output format for the new marker, matching the existing bullet shape:
file:line — comment recites , belongs in <PLAN doc section /
CHANGELOG / PR>, not source [wrong home]

Do not flag correctness-critical protected comments (lock ordering,
cancellation races, silent-failure contracts, cross-thread invariants,
state-machine terminality — `MAINTAINABILITY.md` → *Correctness-critical
comments — protected*) under this marker even if they read as narrative;
those are explicitly protected content, not narrative-placement violations.
If a diff trims comment lines inside a file carrying the protected-file
header note, that's item 3 below, not this marker.

Update `CONVENTION_CHECK.md`'s "Don't" list and marker-definition section
to describe `[wrong home]` alongside the existing two markers, and update
the effectiveness-log schema/outcome-vocabulary wiring wherever the marker
set is enumerated — check `python/cecelia/effectiveness/log.py`'s
`OUTCOME_VOCABULARY`, `console.py`'s `_MECHANICAL_RUN_COUNT` table (named
explicitly as "out of scope" by convention-check in #1297 — meaning it's a
known, standing private enumeration worth re-checking here), and any other
place that lists convention-check's marker types. This is exactly the
enumerable-set-in-two-places shape fanout has caught repeatedly (the
`OUTCOME_VOCABULARY` copy in `#1249`, the outcome-order tuple in `#recital-
console`) — do not let this addition become another instance of the same
bug.

### 3. New standalone mechanical check — no model call, built like inventory_coverage.py

Follow `inventory_coverage.py`'s established shape exactly: advisory only,
never blocks; a named `<PREFIX>-EXEMPT:` opt-out marker; logs specific
flagged locations, not counts; a `_<Title>: <verdict>_` tail line; and —
critically — every sub-check below gets replayed against merged PR history
before it ships, with a reported hit rate, the same way both of
`inventory_coverage.py`'s conditions were validated (19/150 and 12/14)
before merge. A sub-check with a near-zero replay hit rate gets cut, per
the citation-currency lesson, not shipped on faith.

Sub-checks to build and replay:

- **No incident history in source**
  (`MAINTAINABILITY.md` → same-named section): regex for dataset-ID-shaped
  strings (the doc gives examples like `WIaUjL`, `p6t4mC`, `fXgbTl` — six-
  character alphanumeric slugs), dated-authorship patterns
  (`Name, YYYY-MM-DD`), quoted user-report strings, internal phase-code
  patterns (`P9 slice 4`, `Phase H, H3`), and commit-SHA cross-references
  (`commit [0-9a-f]{7,}`). **Exception to encode, as an
  `INCIDENT-OK: <reason>`-style marker matching the established family:**
  a measurement comment that is the stated provenance for an adjacent
  hardcoded constant is protected, not incident history — the doc names
  `GATED_SEC_PER_PLANE`/`GAP_WORTH_PAYING_FOR` in
  `frontend/src/tasks/smoothVis.ts` as the example. A cheap heuristic
  (comment sits immediately above/beside a `const`/assignment with a
  numeric literal) can supplement the marker; false negatives should fail
  safe (don't flag) rather than false-flag protected content.
- **"For now this just handles X"**: literal phrase grep
  (`"for now"` near `"handles"`, case-insensitive, plus reasonable
  variants — check real usage in the repo for the actual phrasing pattern
  before finalizing the regex).
- **File-responsibility line count**: a task file under
  `app/src/tasks/**` exceeding ~200 lines, per the *Split rule*. Exclude
  `app/src/tasks/scheduler.jl` by name (per *Splitting a lock-owned
  monolith needs invariant proof* — this file's size is a known, accepted
  exception, not a finding).
- **Protected-comment-file trim detection**: for any file carrying the
  concurrency-critical header note (currently
  `app/src/tasks/scheduler.jl`, per *Currently flagged as protected* — grep
  for the header comment itself to find any others added later), warn (not
  block) if a diff net-decreases comment line count in that file, so a
  human notices and re-checks before assuming it's just formatting.

Reuse `inventory_coverage.py`'s diff-parsing helpers
(`new_files_from_diff` and siblings) rather than writing a second diff
parser — `#1297`'s own convention-check pass explicitly confirmed there
was previously only one diff parser in the package (the now-deleted
`citation_currency.touched_files_from_diff`); check whether
`inventory_coverage.py`'s replacement is the right one to extend before
adding a third.

## Explicitly deferred — do not build this pass

- **Docstring shape (2–4 lines, no narrative)** — real, but the softest
  judgment call of everything in the doc ("is this too long," "does this
  contain WHY-narrative" both resist crisp grading). Ship items 1–3 first,
  observe a few weeks of real findings, then decide whether this earns a
  fourth mechanism or stays a human PR-review concern. Do not build a
  check for this in this pass.
- **File-responsibility "acquires a third responsibility"** (as opposed to
  the line-count trigger, which item 3 covers): judging *which*
  responsibility a file already carries needs holistic understanding of
  the whole file's purpose, not a single addition's shape. Leave this as
  an accepted gap for now — the line-count half of the split rule is
  mechanically enforced; the responsibility-counting half isn't yet.

## What to verify before considering this done

- **Replay every item-3 sub-check against merged PR history and report a
  hit rate per sub-check**, matching #1297's own validation discipline —
  this is not optional and is the main gate on whether item 3 ships as
  specified or gets trimmed.
- The new `[wrong home]` marker is correctly excluded from `[should
  reuse]`/`[potential duplicate]` counting anywhere the effectiveness log
  or rollup aggregates convention-check outcomes by marker type — check
  `rollup.py`, `console.py`'s `_MECHANICAL_RUN_COUNT`, and `log.py` for any
  hardcoded two-marker assumption.
- Run the expanded `convention-check` prompt against at least one real
  historical diff that should trigger each of the three new type-reuse
  categories (a `Dict{String,Any}` at a boundary, a new stringly-typed
  status field, a new `Union{Nothing,T}` discriminant) and confirm it
  fires — don't assume the prompt edit works from reading it alone.
- Confirm every place that named `citation_currency` and now needs to name
  the new check instead is actually updated — `#1297`'s own fanout finding
  (`fanout-de217f12`) caught `GOVERNANCE_INDEX.md` still crediting the
  retired citation-currency check after retirement; do not repeat that
  exact miss for whatever this task's own retirements/renamings touch.

## Output format

- Diff/PR for `CONVENTION_CHECK.md` (items 1 and 2).
- New script + hook wiring for `maintainability_lint_run` (item 3),
  built on `inventory_coverage.py`'s pattern.
- The replay hit-rate results per sub-check, reported plainly, with any
  sub-check that failed to earn its place explicitly cut and said so —
  not silently dropped, not shipped anyway.
- The verification results from "What to verify," reported plainly.
- One paragraph confirming `MAINTAINABILITY.md` itself is updated to
  reflect what's now actually enforced vs. still a plain reference doc —
  the intro line ("the check that runs before new code lands") should stop
  being aspirational for the parts this task mechanizes.