# Sibling-call audit — the pre-commit reviewer

**What this file is:** the exact prompt the pre-commit sibling-call reviewer subagent is spawned with. Also the mechanism note and the escape valves. Cited from [`CLAUDE.md`](../../CLAUDE.md) → *Git & commits*.

**Why this exists:** catch **case-F-shaped bugs** — *a fix that silently leaves other divergent copies of the same guard, resolver, or contract broken*. Three confirmed pairs in the 2026-Jun–Sep window: #816→#822, #828→#839, #1101→#1151. Design record: [`../todo/SIBLING_CALL_AUDIT_PLAN.md`](../todo/SIBLING_CALL_AUDIT_PLAN.md).

## How it runs

At the pre-commit reservations step, the implementing agent spawns a fresh subagent:

```
Agent(
  subagent_type: "general-purpose",
  model: "sonnet",
  prompt: <contents of the "Reviewer prompt" section below>
          + "\n\n---\n\n" + <git diff --staged>
)
```

The subagent's reply folds into the reservations recital under a `**Sibling-call audit:**` heading — always printed, even when the reply is "no siblings found" or "not applicable", so the record shows the check ran.

**Model = sonnet, not Opus.** The task decomposes into read-diff → name-symbols → grep-callers → per-site shape-match. That's many small independent reads, not one deep reasoning chain. Sonnet handles this shape well; Opus over-reasons at cost that adds up when this runs on every commit. The reviewer surfaces candidates for the user's judgement — confidence markers below do the calibration.

## Escape valves — skip the subagent when

- Diff is docs-only (only `docs/**`, `*.md`, `CLAUDE.md` files touched). Print `**Sibling-call audit:** skipped — docs-only diff` in the recital.
- Diff is a pure new-file addition (no modifications to existing code). Print `**Sibling-call audit:** skipped — no modified code`.

Otherwise spawn the subagent — even for small diffs. The prompt's own short-circuit handles no-fix-hunk cases.

## Reviewer prompt

*(Everything below the horizontal rule is passed verbatim as the subagent's prompt, followed by the staged diff.)*

---

You have full read access to the repo (do not modify). `git diff --staged` follows the `---`.

**Job:** catch **case-F drift** — a fix that silently leaves other divergent copies of the same guard, resolver, helper, or contract broken. You surface candidates; you do not review code.

**Per fix-shaped hunk:**

1. Name the symbol it modifies (`file:line`).
2. `grep` other call sites; read each enough to judge shape.
3. If a call site exhibits the same shape as the bug this hunk fixed, flag it.

**Fix-shaped** = corrects behaviour (guard, missing case, resolver, null-handling, path retarget). Not: pure additions, renames, refactors, formatting, docs.

**Output** — one line per finding, most severe first, ≤300 words total:

```
- **file:line** — symbol, shape, why potentially affected [**confirmed** | **plausible**]
```

- **confirmed** — you read the site; it matches.
- **plausible** — same symbol, fit unverified or ambiguous (say what would confirm).

If >8 findings: top 8 + `N more not listed`.

**Short-circuits (reply verbatim):**
- No fix-shaped hunks → `no sibling-call audit needed`
- Fix modifies a symbol with zero other callers → `- <symbol> — sole caller, no siblings`

**Don't:** suggest fixes; re-review the diff for its own bugs; flag stylistic siblings; fabricate call sites (say so if grep is empty).
