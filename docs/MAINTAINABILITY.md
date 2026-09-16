# Maintainability standard

The check that runs *before* new code lands — comment shape, cross-module contracts, file
responsibility. Applies equally to human- and agent-written code. Findings from every future audit
update this file rather than spawning a new one.

Companion to the "where things live" index in [`MAP.md`](MAP.md) and the discovery step in the root
[`CLAUDE.md`](../CLAUDE.md). Frontend-specific UI-copy rules stay in [`ui/COPY.md`](ui/COPY.md);
this doc is source comments and structure.

---

## Comment / docstring rules

### Docstring shape

**2–4 lines**: what it does, what it returns, its load-bearing invariants. Nothing else. If the
paragraph explaining WHY is longer than the docstring, that paragraph belongs somewhere else — see
[Where narrative content goes](#where-narrative-content-goes).

Signature + one line is fine for a small helper. A pattern-match `_state_str(v)` needs no docstring
at all; the name is the doc.

### No cross-references to code outside this repo

A comment that cites `depmixS4`, `caTools::runmean`, `DescTools::Mode`, the "R port", a superseded
R function name, or any file/module not in this checkout is a **liability, not a reference** —
nobody reading this repo can open it, including a future contributor and a future AI agent.

**Fix, in order of preference:**
1. Restate the invariant in this codebase's own terms — what does the function guarantee,
   independent of the reference?
2. If the cross-reference is genuinely load-bearing (a published algorithm, a canonical reference
   implementation), **vendor the source** under `docs/reference/` and link by path.
3. Only as last resort — cite paper + DOI in the code and validate against golden values in the
   test suite. Root [`CLAUDE.md`](../CLAUDE.md) → *Cite sources for non-trivial algorithms*.

**Bad:** `# Ports the R postFiltering/postIterations step (DescTools::Mode over a frollapply window, take-first on ties).`
**Good:** `# Windowed-mode smoothing of a per-track state sequence. Ties broken by smallest state ID for reproducibility.`

**Exception — whole files that exist to bridge a named external system.** A legacy-format migrator
(`migrateLegacy.jl`, `python/cecelia/utils/legacy_migrate.py`, `scan_legacy_run.py`), a canonical
port of a published algorithm (`app/src/tasks/tracking/track_measures.jl` and
`app/src/tracking/track_diagnostics.jl` — the celltrackR ports, validated against celltrackR golden
values in `app/test/suite.jl`), or a reference implementation kept alongside the production path
(`python/cecelia/utils/anisotropy_utils.py:tangent_tensor_field`) IS bridging code — its references
to the named external system are load-bearing. Cite the source; where feasible, validate against
its golden values in the test suite. The rule above applies to comments that *incidentally* cite an
external system, not to files whose purpose is that bridge.

### No incident history in source

None of the following belong in a source comment or docstring:

- Dataset IDs (`WIaUjL/p6t4mC`, `fXgbTl`, six-character project uid slugs)
- Measured percentages tied to one run (`0.113 from CH3 into CH2`, `735-3576 of 65536 levels`)
- Quoted user reports (`reported: "the zslice is still not updating..."`)
- Dated authorship attributions (`Dominik, 2026-08-25`)
- Narrative history (`used to be QC-exempt`, `The first run reported`, `It appeared — X — and it was`)
- Internal phase codes (`P9 slice 4`, `Phase H, H3`) that will rot on the next reorg
- Commit SHA cross-references (`same class as commit 860da24b`) that break on rebase or squash

**One exception:** if a hardcoded constant's provenance IS the argument for the number, the
measurement is load-bearing — that comment is protected (pattern 3 below), not incident history.
Examples: `GATED_SEC_PER_PLANE = 0.12` and `GAP_WORTH_PAYING_FOR = 0.12` in
`frontend/src/tasks/smoothVis.ts`. If you delete the provenance, the constant becomes a magic
number nobody can defend.

### Where narrative content goes

| Content | Home |
|---|---|
| A rejected alternative + why it was rejected | `docs/todo/<AREA>_PLAN.md` **Locked decisions** section (see [`MULTI_POP_TRACKING_PLAN.md`](todo/MULTI_POP_TRACKING_PLAN.md) for the format). Not a new `docs/decisions/` ADR folder — that's a convention this repo doesn't have. |
| A specific debugging incident, dataset, or one-off measurement | `CHANGELOG.md` or the PR description, if it's worth keeping at all. Usually it isn't. |
| Load-bearing rationale (why THIS choice, not that one, would break the invariant) | Stays in source, as short as it can be while remaining a rule not a story. |

### Correctness-critical comments — protected

Comments that document any of:

- **Lock ordering** (`never nest _TASKS_LOCK inside _POOLS_LOCK`)
- **Cancellation races** (the `on_process` race guard in `_execute_job!`)
- **Silent-failure contracts** (`the post lives in a finally because a throw here is otherwise silent`)
- **Cross-thread / cross-process invariants** (`@atomic proc gives cross-thread visibility`)
- **State-machine terminality** (`terminal states are final — don't let :done overwrite :cancelled`)

are **protected**. Never trim them for brevity. Consolidation of *literal* restatements is fine —
three restatements of the same sentence in different words become one paragraph — but the invariant
survives.

If a file is dense with these, add a one-line header:

```
# concurrency-critical — comments here document lock ordering and failure-mode
# contracts; do not trim for brevity without re-reading them.
```

and skip content edits to that file's comments unless something is a literal duplicate.

**Currently flagged as protected:** `app/src/tasks/scheduler.jl` (see file header).

### `# invariant:` prefix

For a load-bearing rule inside a longer function, prefix it with `# invariant:` so a reader (or
agent) can scan for what they mustn't break without reading the surrounding narrative:

```julia
# invariant: post to job.done exactly once, unconditionally — submitter is blocked on take!
post!(result) = (posted[] || (posted[] = true; put!(job.done, result)))
```

---

## Cross-module contracts

If module A depends on module B's output shape, that shape gets a **type / struct / schema** — not
a comment. A comment saying "expects X" is a risk-register entry, not a specification.

### The three specific triggers

1. **A `Dict{String,Any}` (or `AbstractDict`) crossing a boundary** — frontend↔Julia, Julia↔Python,
   API↔handler — needs a typed constructor at the boundary. Naming the shape (e.g.
   `AfCombinationSpec`, `AfChannelStats`) lets a rename fail loudly at compile time instead of
   silently producing zero findings.
2. **A caller-side workaround for a callee's quirk** ("the target is dropped from its own
   competitor list, done defensively at the call site") — enforce it at the source, not defensively
   at every call site.
3. **A "correct today because the call order happens to be right"** — B must be called after A,
   with nothing preventing the reverse — needs either structural enforcement (a builder that only
   produces valid states) or a runtime assertion at B's entry.

### Typed task params — every `_run_task` reads through `parse_<task>_params`

Every task's params bag is a **typed `Base.@kwdef` struct** built by a `parse_<task>_params(::AbstractDict)`
helper — `_run_task` calls it once at the top, then reads `p.field`. Scattered `get(params, "foo",
default)` calls inside `_run_task` are the pattern-4 boundary drift this rule exists to prevent: a
spec rename that isn't mirrored produces a silent default at the read site instead of a struct-field
error at parse. Canonical example: `app/src/tasks/cleanupImages/smooth.jl`.

**Enforced** by `typed params ratchet — _run_task reads params through parse_*_params` in
`app/test/suite.jl`. Same shape as the CSS ratchet in `frontend/src/utils/cssScenarios.ts` — an
exact per-file baseline (`TYPED_PARAMS_MIGRATION_BASELINE`) that MAY SHRINK, MUST NEVER GROW; when
empty, any regression fails immediately.

A field that must stay untyped (a "bag") — a `channelSelection` resolved at runtime via
`channel_indices`, a JSON blob validated later with `ParamValidationError`, a `models` list resolved
per-image — is declared as `::Any` in the struct with an inline comment saying why. `nothing`-vs-value
sentinels (e.g. `flowMetrics = nothing` meaning "no picker in this call") are preserved deliberately,
not typed away.

The rare pre-parse guard — a shape check that only makes sense on the raw bag (see
`editImages/cropImage.jl` distinguishing "missing box" from "defaulted zero-box") — carries
`# ratchet-ok: <reason>` on the exact line, same escape-hatch discipline as the H5AD/zarr readers.
Bare `get(params, …)` inside `_run_task` without that marker fails the ratchet.

### Enums for state machines

A `Symbol` or `String` field with a known set of legal values is a state machine documented only
by convention. Prefer `@enum` + a typed setter (`set_status!(rec, ::TaskStatus)`) — the terminality
check comes with the type; a typo in the value becomes a compile-time error instead of a silent
default.

**This is the single most systemic P4 pattern in this codebase.** The Phase-4 register found six
instances of the same shape (a `String`/`Symbol` field, 4–7 legal values, enum-in-a-comment):
`TaskRecord.status`, `ChainNode.scope`, `ChainNode.barrier_policy`, `ImageNodeState.status`,
`CciaImage.status`, `Population.pop_type` (+ `popType` in `gating_api.jl`). One `@enum` pass
across these resolves six risk-register entries in one PR. Fix template:

```julia
@enum TaskStatus TASK_QUEUED TASK_RUNNING TASK_DONE TASK_FAILED TASK_CANCELLED
mutable struct TaskRecord
    ...
    status::TaskStatus
end
function set_status!(rec::TaskRecord, s::TaskStatus)
    rec.status in (TASK_DONE, TASK_FAILED, TASK_CANCELLED) && return  # terminal
    rec.status = s
end
```

Full list of instances + fix template details: [`docs/archive/comment-audit-findings.md`](archive/comment-audit-findings.md)
→ *Risk register* → *State machines*.

### Sum types over `Union{Nothing, T}` discriminants

`TaskJob.imgs::Union{Nothing, Vector{CciaImage}}` uses `nothing`-vs-vector to discriminate
set-scope vs single-image. Every branch has to remember the check. A sum type (`SingleTarget` /
`SetTarget`) or a `job_target(job)` helper that never leaks the union removes the whole class of
bugs.

---

## File responsibility

A task file that mixes three of `{run orchestration, QC scoring, param translation, cohort metric,
composite dispatch}` is a landing spot for a fourth. That's how a monolith accretes.

### Split rule

Once a task's file **grows past ~200 lines** OR **acquires a third responsibility**, split it along
the responsibility axis:

```
tasks/<name>/
  run.jl        # _run_task, orchestration
  qc.jl         # *_qc_findings, cohort metrics
  translate.jl  # params-for-python, param-shape adapters
```

**Example:** `app/src/tasks/cleanupImages/af_correct.jl` (243 lines) already carries all three.
Cleaner seams: `af/run.jl`, `af/qc.jl`, `af/translate.jl`.

### Splitting a lock-owned monolith needs invariant proof

`app/src/tasks/scheduler.jl` (733 lines) is a monolith by size but the lock-ordering invariant
crosses several of its would-be seams (`never nest _TASKS_LOCK inside _POOLS_LOCK`). A split MUST
preserve the invariant, not just move the code. Flag it as a slow, explicitly-invariant-preserving
refactor with tests that pin the ordering — not a routine cleanup.

### "For now this just handles X" is a red flag

That phrase names a landing spot for the next feature to be wedged in rather than given its own
home. Either commit to X being the whole answer (delete the qualifier) or extract now.

---

## Frontend specifics

- **In-app UI copy** (button labels, tooltips, empty states, QC findings) → [`ui/COPY.md`](ui/COPY.md)
  and [`frontend/CLAUDE.md`](../frontend/CLAUDE.md). Not this doc.
- **Source comments** follow the general rules above.
- **Measurement-provenance comments** on hardcoded constants are protected (pattern 3), not incident
  history. If deleting the comment leaves a magic number, keep the comment. The current example set
  lives in `frontend/src/tasks/smoothVis.ts` (`GATED_SEC_PER_PLANE`, `GAP_WORTH_PAYING_FOR`,
  `SPATIAL_COLOR_TO_SCHEMATIC`).

---

## Checklist before you commit

- [ ] Docstrings are 2–4 lines. Load-bearing invariants only, no narrative.
- [ ] No cross-repo references. Cited external code is vendored under `docs/reference/` or restated.
- [ ] No dataset IDs, quoted user reports, dated authorship notes, measurement history unless
      defending a hardcoded constant.
- [ ] Any `Dict{String,Any}` crossing a boundary has a typed constructor (or a risk-register entry).
- [ ] Not appending to a file that already carries 3+ responsibilities.
- [ ] Concurrency-critical files carry the header note; you didn't trim any invariant comment there.
- [ ] Load-bearing single-line rules use the `# invariant:` prefix.

---

## Living document

Findings from future audits update this file. New patterns get added, not filed in a new doc.
