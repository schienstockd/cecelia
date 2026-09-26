# Drift-Prevention Hooks — Assessment & Decision Record

**Status:** assessed and declined (2026-09-26) — decision record, not a plan. Re-open when a
second case-F-shaped bug lands (see *When to revisit* below).

**Related:**
- Brief: [`docs/archive/drift_prevention_mechanism_prompt.md`](../archive/drift_prevention_mechanism_prompt.md)
- Findings: [`docs/archive/drift_prevention_mechanism_audit.md`](../archive/drift_prevention_mechanism_audit.md)
- Catch-mode audit (follow-up): [`docs/archive/drift_catch_modes_audit.md`](../archive/drift_catch_modes_audit.md)

## Goal

Decide whether to add a `PreToolUse` / `SessionStart` / adversarial-review harness to prevent
sessions from re-implementing helpers that already exist (INVENTORY.md, MAINTAINABILITY.md, CSS/UI
ratchets). Grounded in the audit above, not in vibes.

## Verdict

**No harness. Ship four small targeted fixes instead.** The evidence doesn't support paying the
harness's cost — most drift got caught cheaply by PR review, and only one of seven audited cases
was expensive.

## Reasoning — the honest read

The audit's "no enforcement" root-cause is technically correct but frames PR review as
non-enforcement. PR review IS enforcement, just late. The right question isn't *"add prevention"*
— it's *"where is late catching actually expensive?"*

Answer, from the seven PR-trail cases the audit collected, refined by the follow-up catch-mode
audit that sampled ~45 classifiable drift PRs across the full 1,225-PR window:

- **Frontend drift is caught same-day by user-visual-notice** — the model ships a module page or
  panel, the user opens it, it looks different from everything else, follow-up PR the same day.
  Blackboard is the archetype (#1070 → #1074 same day). Cost: one refactor cycle, no downstream
  code depended on the drifted shape yet.
- **Backend drift is caught by self-audit sweeps** — the model finds N duplicates while doing
  another task, closes them together, often ships a detector in the same change. Sixteen PRs of
  this shape in the sample (#420, #423, #425, #442, #476, #504, #521, #587, #598, #721, …). Cost:
  *(one audit + N fixes) / N* — arguably cheaper than PR-time review per case.
- **One case in the whole sample was expensive** — #1151, a fix that silently left divergent copies
  broken. Rare — the wider sweep didn't surface a second.

None of these are a pattern that pays for infrastructure — the two dominant catch modes have
different economics but both stay cheap, and the one expensive case is rare enough that a scoped
adversarial reviewer would sit idle most of the time.

### The frame underneath — cost-gradient, not rule-following

Why the discovery-first rule in `CLAUDE.md` is skipped even when it's in context: it's a
cost-gradient problem, not a rule-following problem. At the moment of writing, the discovery path
(grep inventory, read the helper, wire it in) costs the agent real tokens/tool-calls/latency
*now*. The bespoke path (write the helper from memory) costs zero *now*, and the duplication cost
lands on some future maintainer. Classic externality: the person who pays isn't the person who
decides.

This is why prose rules lose — they add a moral obligation without changing either cost. It's also
why hooks would work in principle (they reprice the alternatives: not-checking becomes infinitely
expensive), and why we still say no (building a hook costs more than the drift it would prevent,
right now). And it's why `SessionStart` injection is the worst intervention shape — it pays the
discovery cost upfront on *every* session, including the 90% that don't need it. A hook only fires
on the writes that would otherwise drift; that's making the *right* cost cheaper.

The revisit triggers below are proxies for the cost balance flipping: *drift cost per unit time >
amortised hook cost*. Don't build the intervention until the inequality does.

## Why each proposed layer was rejected

### Write-time `PreToolUse` gate — rejected

Would need an exact symbol/component match against `INVENTORY.md` before allowing a `Write`/`Edit`.
Per the audit's per-instance table, catches the fewest cases (only cases B and C outright — the
rest are wrapper-shaped or the canonical name wasn't in inventory yet). False positives on every
new file will train the agent to auto-justify past the block. **Worst layer by
coverage-to-cost.**

### `SessionStart` injection — rejected

Would load `INVENTORY.md` (and/or a MAINTAINABILITY.md digest) into every session's context. The
audit measured `INVENTORY.md` at ~1 KB and the six area files at ~50 KB combined. Loading them
into *every* session — including Julia-only or pure-frontend ones — undoes the nested-CLAUDE.md
discipline that the audit called a real strength (`frontend/CLAUDE.md` loads only under
`frontend/`, saving ~2 KB per unrelated session).

Also: the discovery-first rule in `CLAUDE.md` already tells the agent to grep the inventory. When
sessions skip that step, the problem isn't that the inventory wasn't in context — it's that the
agent didn't consult a rule that WAS in context. Doubling the payload doesn't fix that.

### In-session adversarial-review subagent — rejected, kept as future option

Catches all seven audited cases in a fresh-context review of the diff against the criteria. Real
token cost per change though: ballpark 15-40k tokens per review pass. If invoked always-on that's a
meaningful overhead on every session.

**Not building it yet, but noting the shape for later:** if we do add it, scope it — either by
directory (`frontend/src/components/`, `app/src/gating/`, `app/src/tasks/chain/` — the three
drift-prone areas from the audit) or by diff size (>N lines in a churn-prone dir). Skipping cheap
diffs is what makes it affordable.

### Existing backstops — kept

Two distinct catch mechanisms, not one. Design as if both exist; don't collapse them into "PR
review is the backstop."

- **User-visual-catch (frontend/UX).** The user opens a new module page or panel and notices it
  looks different from everything else. Same-day catch when it works. Not a fallback — an active
  human-in-the-loop enforcement mechanism, and the primary one for whole-page shape (which no
  ratchet in the repo covers today — the `cssScenarios.test.ts` family catches rule-level shape,
  not layout-level shape).
- **Self-audit sweep (backend).** A session working on task A notices duplication in area B,
  closes N offenders together, often ships a detector in the same change. The `_BASELINE`
  ratchets in `app/test/suite/ratchets.jl` and the `_convention.py` family are the durable
  artifact this mode leaves behind.
- **PR-time review.** The general net under both — most useful when the two above missed.

## What we did instead — the four fixes (2026-09-26)

Each fix ships in the same change as this doc.

1. **INVENTORY.md stamp refreshed.** The parent `INVENTORY.md` last-audited stamp was 10 weeks
   stale (2026-07-16) while the per-area `docs/inventory/*.md` are updated in-line. Bumped and
   annotated so a reader doesn't take the stamp as evidence the whole index is stale.
2. **CLAUDE.md redundancy folded.** The audit flagged one duplicate: a blockquote at the top of
   the file restated the divergent-reimplementation rule that appears as an H2 20 lines below.
   Deleted the duplicated paragraph; kept the second paragraph of the blockquote (the
   going-in-circles rule, which was independent).
3. **Meta-ratchet on `_BASELINE` arrays.** Per the eval-gate-ratchet reference in the audit — the
   silent-weakening failure mode is an agent adding a file to `_BASELINE` to skip a new violation
   instead of fixing it. Added `_BASELINE_MAX_SIZE` size caps to each of the four ratchet
   baselines (`test_zarr_access_convention.py`, `test_task_json_convention.py`, and the two Julia
   ratchets in `app/test/suite/ratchets.jl`). Growing the baseline now requires bumping the
   constant in the same PR — visible in diff.
4. **@enum flags — one verified as sugar, one fixed inline.** The audit flagged two post-doc
   `String`-typed fields where the canonical `@enum` sat one dir over:
   - `app/src/tasks/chain/api.jl:205` — `scope::String = ""` in the `chain_node` builder is
     **intentional API sugar**. Traced: the `ChainNode` field is `ChainScope` (enum), and
     `_coerce_scope(::AbstractString, fn)` in `app/src/tasks/chain/types.jl:69` handles
     `String → ChainScope` conversion (documented at 64-66). Not a violation.
   - `app/src/gating/popmanager/pop_df.jl:268` — `pop_type::String` in `DerivedPopSpec` **was**
     inconsistent with `pop_type::PopType` on `Population`/`DerivedPop` one file over
     (`population.jl:20,57`). Fixed in this PR: field flipped to `PopType`, the `_DERIVED_POPS`
     literal now uses `POP_LIVE` instead of `"live"`, the `resolve_pop_type::String` return-path
     (`mixed_resolution.jl:45`) got a `string(...)` wrap, and the four `spec.pop_type == /!=
     string(pop_type)` comparisons across `allow_list.jl` and `pop_df.jl` now compare enum-to-enum
     via `_coerce_pop_type(pop_type)`.

## When to revisit this decision

Open this doc and re-run the audit if any of these fires:

- A second case-F-shaped bug lands — a fix that silently leaves other divergent copies broken.
  One is chance; two is a pattern that pays for a scoped adversarial reviewer. Catch-mode audit
  confirms the wider PR sample surfaces no second case, so this trigger stays well-calibrated.
- The PR-trail drift rate — measured as "cases per month where the reviewer or user had to point
  at a canonical helper the agent missed" — sustains above ~2/month for two consecutive months.
  Cheap to check via `gh pr list` with the same keyword filter the audits used.
- **User-visual-catch on a module page starts arriving days-later rather than same-day.** Today it
  arrives same-day (the blackboard PRs are the archetype), which means drift never lives long
  enough to accumulate downstream. If new module-page drift starts landing before the user opens
  the page — because the user is heads-down elsewhere, because a chained PR ships two together,
  because a page ships in a mode the user doesn't routinely open — then user-visual-catch has
  failed and downstream code will start depending on the drifted shape. That's when a scoped
  adversarial-review subagent on new `frontend/src/modules/*.vue` earns its keep.
- `INVENTORY.md` staleness becomes causal — the audit found it wasn't; that could change.
- A specific area develops a repeated drift pattern that a scoped write-time regex could catch
  cleanly (not the general case, a specific one). E.g. every new Vue component under
  `frontend/src/components/` re-implementing a base primitive would justify a
  primitive-name-collision check scoped to that dir only.

## What NOT to bring back without new evidence

- **Broad `SessionStart` context injection.** The nested-CLAUDE.md pattern is load-bearing; don't
  regress it. And per the cost-gradient frame above, it makes the *wrong* cost cheaper — pays the
  discovery bill on every session including the ones that won't drift.
- **A general-purpose `PreToolUse` regex gate on Write/Edit.** Coverage is too narrow, false
  positives train around it.
- **A longer `CLAUDE.md` or `MAINTAINABILITY.md`.** Per Anthropic's own docs: *"If your CLAUDE.md
  is too long, Claude ignores half of it because important rules get lost in the noise."* Root
  CLAUDE.md is 306 lines and edge-of-comfort; prose additions have a negative expected value
  until enforcement mechanics land.
- **A design that treats user-visual-catch as a fallback rather than a real mechanism.** It is the
  primary catch mode for whole-page UX shape (see catch-mode audit). Any future intervention that
  competes with or short-circuits it should be justified against what it displaces, not against a
  vacuum.

## References

- Anthropic Claude Code best-practices — https://code.claude.com/docs/en/best-practices — the
  advisory-vs-deterministic distinction, the over-specified-CLAUDE.md failure mode, the
  adversarial-review pattern.
- `mehmethk88-dot/eval-gate-ratchet` — the silent-weakening pattern the `_BASELINE_MAX_SIZE`
  meta-ratchet defends against.
- Catch-mode audit ([`docs/archive/drift_catch_modes_audit.md`](../archive/drift_catch_modes_audit.md)) —
  wider 1,225-PR sweep confirming user-visual-catch (frontend) and self-audit-sweep (backend) as
  the two dominant catch modes, and #1151 as the only case-F-shaped bug in the wider sample.
