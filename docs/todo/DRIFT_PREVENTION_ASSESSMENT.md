# Drift-Prevention Hooks — Assessment & Decision Record

**Status:** assessed and declined (2026-09-26) — decision record, not a plan. Re-open when a
second case-F-shaped bug lands (see *When to revisit* below).

**Related:**
- Brief: [`docs/archive/drift_prevention_mechanism_prompt.md`](../archive/drift_prevention_mechanism_prompt.md)
- Findings: [`docs/archive/drift_prevention_mechanism_audit.md`](../archive/drift_prevention_mechanism_audit.md)

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

Answer, from the seven PR-trail cases the audit collected:

- **Six cost one review cycle each.** Cheap.
- **One (#1151) was expensive** — divergence *caused* a bug: a fix landed on 1 of 3 wrapper
  variants and the other two stayed silently broken.

That's not yet a pattern that pays for infrastructure.

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

### PR-time post-hoc net (existing) — kept

The current backstop. Already caught six of the seven audited cases. Keeps working.

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
4. **@enum flag verified.** The audit flagged two post-doc `String`-typed fields where the
   canonical `@enum` sat one dir over:
   - `app/src/tasks/chain/api.jl:205` — `scope::String = ""` in the `chain_node` builder is
     **intentional API sugar**; the `ChainNode` field itself is `ChainScope` (enum), and the
     builder converts internally. Not a violation.
   - `app/src/gating/popmanager/pop_df.jl:268` — `pop_type::String` in `DerivedPopSpec` **is**
     inconsistent with the `pop_type::PopType` fields in the two `Population` structs one file
     over (`app/src/gating/popmanager/population.jl:20,57`). Flagged in
     [`docs/TODO.md`](../TODO.md) as a follow-up; not fixed here because the change touches
     callers.

## When to revisit this decision

Open this doc and re-run the audit if any of these fires:

- A second case-F-shaped bug lands — a fix that silently leaves other divergent copies broken.
  One is chance; two is a pattern that pays for a scoped adversarial reviewer.
- The PR-trail drift rate — measured as "cases per month where the reviewer or user had to point
  at a canonical helper the agent missed" — sustains above ~2/month for two consecutive months.
  Cheap to check via `gh pr list` with the same keyword filter the audit used.
- `INVENTORY.md` staleness becomes causal — the audit found it wasn't; that could change.
- A specific area develops a repeated drift pattern that a scoped write-time regex could catch
  cleanly (not the general case, a specific one). E.g. every new Vue component under
  `frontend/src/components/` re-implementing a base primitive would justify a
  primitive-name-collision check scoped to that dir only.

## What NOT to bring back without new evidence

- **Broad `SessionStart` context injection.** The nested-CLAUDE.md pattern is load-bearing; don't
  regress it.
- **A general-purpose `PreToolUse` regex gate on Write/Edit.** Coverage is too narrow, false
  positives train around it.
- **A longer `CLAUDE.md` or `MAINTAINABILITY.md`.** Per Anthropic's own docs: *"If your CLAUDE.md
  is too long, Claude ignores half of it because important rules get lost in the noise."* Root
  CLAUDE.md is 306 lines and edge-of-comfort; prose additions have a negative expected value
  until enforcement mechanics land.

## References

- Anthropic Claude Code best-practices — https://code.claude.com/docs/en/best-practices — the
  advisory-vs-deterministic distinction, the over-specified-CLAUDE.md failure mode, the
  adversarial-review pattern.
- `mehmethk88-dot/eval-gate-ratchet` — the silent-weakening pattern the `_BASELINE_MAX_SIZE`
  meta-ratchet defends against.
