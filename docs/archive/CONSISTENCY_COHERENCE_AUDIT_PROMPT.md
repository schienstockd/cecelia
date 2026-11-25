> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in [`docs/ai-assist/GOVERNANCE_INDEX.md`](../ai-assist/GOVERNANCE_INDEX.md) and the
> docs it lists.
>
> **Outcome:** a trimmed Run 1 (steps 3–4 only: mechanical sweeps + baseline) is
> [`../todo/CONSISTENCY_COHERENCE_AUDIT.md`](../todo/CONSISTENCY_COHERENCE_AUDIT.md) (2026-11-25,
> `origin/main` 96a2a2fc). Steps 1–2 were already covered; Run 2 is parked. The brief cites none of the earlier work on
> the same question — start from those:
> [`../todo/DRIFT_PREVENTION_ASSESSMENT.md`](../todo/DRIFT_PREVENTION_ASSESSMENT.md) (a general
> drift harness declined 2026-09-26, with revisit triggers),
> [`../ai-assist/DRIFT_DETECTION_PRIOR_ART.md`](../ai-assist/DRIFT_DETECTION_PRIOR_ART.md),
> [`ratchet_inventory_audit.md`](ratchet_inventory_audit.md),
> [`drift_catch_modes_audit.md`](drift_catch_modes_audit.md),
> [`governance_layer_audit.md`](governance_layer_audit.md) and
> [`../todo/CECELIA_STRUCTURE_AUDIT.md`](../todo/CECELIA_STRUCTURE_AUDIT.md). Read against main, these premises had moved:
>
> 1. **"Nothing is scheduled, comparable over time, or turned into rules."** The weekly judge
>    (systemd, Tuesdays) maps 30 days of fanout + convention findings to `CLAUDE.md` sections and
>    proposes `tighten` / `ratchet` for any rule broken in 3+ sessions
>    ([`../ai-assist/WEEKLY_JUDGE.md`](../ai-assist/WEEKLY_JUDGE.md) step 5). That is the
>    finding → rule promotion this brief asks to design.
> 2. **"Recital is diff-scoped, so a duplication reviewer should move into it."** It is already
>    there: recital runs fanout, convention (`should reuse` / `wrong home` vs `INVENTORY.md`) and
>    result reviewers, plus inventory, maintainability-lint, changelog and test-value checks
>    (`python/cecelia/effectiveness/recital.py`, `CLAUDE.md` → *Git & commits*). Convention
>    findings left unfixed are carried by the judge's bug sweep.
> 3. **"Nothing looks at the whole app."** Whole-repo ratchets run in CI: 12
>    `python/cecelia/tests/test_*_convention.py`, `app/test/suite/ratchets.jl`,
>    `task_spec_ratchets.jl`, `cssScenarios.test.ts`, `maintainability_lint.py`, with
>    `_BASELINE_MAX_SIZE` caps. Doc-vs-code paths are checked by `test_doc_pointer_convention.py`.
>
> What stays new: off-the-shelf whole-repo sweeps (JS/TS duplication, dead code, cycles), a
> trend file of counts per check, Julia ↔ Vue ↔ MCP shape agreement, and lineage on the worst areas.

# CONSISTENCY_COHERENCE_AUDIT_PROMPT

Audit and design task. **No new features, no refactors in this run.** Output: `docs/todo/CONSISTENCY_COHERENCE_AUDIT.md` (plus a proposed plan section at the end). Read-only on source; the only file you write is the output doc.

## Why this exists

Most of Cecelia is written by agents under my direction. Each change passes tests and recital at commit, but nothing looks at the **whole app** regularly. The failure mode is *local correctness, global incoherence*: parallel patterns for the same thing, duplicated helpers, boundary leaks, docs that no longer match code, and conventions that were consistent once and have drifted. I only notice when I see the accumulated result of many PRs in the app.

Today's coverage:

- **Recital** cross-checks a diff at commit. It is diff-scoped, so it cannot see cumulative drift.
- **Weekly judge** finds bugs in runs, not structural coherence.
- **Ad hoc sweeps and audits** (hardening, GUI clarity, test value, etc.) happen when I remember or something hurts. Nothing is scheduled, comparable over time, or turned into rules.

Goal: design a **regular, cheap, comparable consistency and coherence check across the app**, and run its first baseline.

## Prior art to take seriously (verify, don't trust)

Read what is available; I only have search-snippet knowledge of these.

- `githubnext/ado-aw` — scheduled "Documentation Freshness Audit" agent workflow that opens PRs fixing AGENTS.md drift (see issue #836 for a run).
- `mindicator/invAIriant` — bounded-scope audits where candidates must survive adversarial evidence checks ("No evidence. No finding.").
- `MattMagg/agent-harness` — invariants, entropy and garbage collection, doc-gardening checklists; references OpenAI's harness-engineering write-up.
- Deterministic tools: Fallow (JS/TS: duplication, circular deps, boundary violations, dead code) and Drift (Python: pattern fragmentation, near-duplicate helpers).
- Practitioner point: agents treat existing code as evidence of correctness, so a flawed pattern replicates. Review **lineage** (what did the agent copy?), not just the diff.

State which ideas transfer to a Julia + Vue (+ whatever else you find) codebase and which don't. Say so if no good tool exists for a language here.

## Run 1 — Inventory and mechanical baseline (do this first)

1. **Map what already guards coherence.** Recital reviewers (list each and what it actually checks), CI, lint, type checks, architecture tests, CLAUDE.md rules, the judge, docs checks. For each: scope (diff vs whole repo), trigger, and what it cannot see. Produce the gap table.
2. **Find the app's actual invariants.** Read CLAUDE.md, architecture docs, and the code. List the invariants that are *documented*, and separately those that are *implied by the code but never written down*. Mark which are mechanically enforced today.
3. **Mechanical sweeps (no LLM judgement).** Run what can run cheaply and deterministically across the whole repo: duplicate or near-duplicate helpers, dead code and unused exports, import/module-boundary violations, circular dependencies, divergent naming or error-handling patterns, inconsistent API/route/MCP-tool shapes between Julia and Vue, duplicated constants or enums across layers, and doc-vs-code mismatches (paths, function names, commands, architecture tree entries that don't exist or are missing). Report tool and command used, runtime, and false-positive rate on a spot check of ten findings.
4. **Baseline numbers.** One small table of counts per check, so later runs are comparable.

Every finding needs: file:line evidence, the invariant or pattern it violates, and a severity. **No evidence, no finding.** Drop anything you cannot cite.

## Run 2 — Judgement and design (separate run, after I review Run 1)

Do not start this until Run 1 is reviewed. When it runs:

1. **Pattern fragmentation (judgement).** For each area where the same job is done several ways (state handling, task/chain execution, plotting, population handling, IPC, error surfacing, settings, guides), identify the canonical way, the variants, and which variant new code is most likely to copy and why.
2. **Lineage check.** For the three worst fragmentation areas, trace via git history which PR introduced each variant and whether an agent copied an existing flawed pattern.
3. **Doc and code agreement.** Sample AGENTS.md/CLAUDE.md and user-docs claims against code. Report drift rate.
4. **Design the recurring check.** See below.

## Design requirements for the recurring check

Propose, don't build. Constraints:

- **Cheap by default.** Deterministic checks run frequently (CI or pre-push, plus weekly full sweep). LLM judgement runs on a bounded scope (one module or area per run, rotating), not "wander the repo".
- **Complements recital, doesn't duplicate it.** Say explicitly what moves into recital (diff-scoped, e.g. a duplication or boundary reviewer) and what stays a whole-repo sweep.
- **Findings feed the existing loop.** Prefer routing into the weekly judge / decide queue rather than a new dashboard. I already have 80+ decide items; the design must not add review burden. Include dedup, a severity gate, and an auto-park rule for low-value findings.
- **Evidence-verified.** Candidates from an LLM lens must be verified against cited code before they count.
- **Ratchet, not report.** Each recurring finding class should end as a lint rule, architecture test, or CLAUDE.md line, so it cannot return. Describe how a finding gets promoted to a rule.
- **Trend over time.** A baseline file with counts per check, diffed each run, so I can see whether coherence is improving.
- **Budget.** Standard seat, credit-metered, so no Fable. State expected cost per weekly sweep and per rotating-area judgement run, and what is dropped if over budget.
- **Maturity tiers.** Say how findings interact with Coming soon / Experimental / Beta / Tested surfaces (e.g. looser bar for Experimental).

## Output format for `docs/todo/CONSISTENCY_COHERENCE_AUDIT.md`

1. Summary (10 lines max): what is covered, what isn't, top 5 incoherence risks.
2. Existing-guard gap table (Run 1 step 1).
3. Invariants list: documented / implied / enforced.
4. Mechanical findings with evidence, baseline counts, false-positive rate.
5. Fragmentation map and lineage notes (Run 2 only; leave a stub in Run 1).
6. Proposed recurring check: components, cadence, routing, ratchet, cost.
7. Decisions I need to make (numbered, each with your recommendation and the cheapest alternative).
8. What you could not verify.

## Rules

- Cite file:line or commit for every claim; mark inference as inference.
- No code changes, no new dependencies installed into the repo. Run tools from a scratch location if needed.
- Tell me if the premise is wrong (e.g. recital already covers something I listed as a gap).
- Keep it short. Tables over prose. If a section has nothing, say "nothing found" rather than padding.
