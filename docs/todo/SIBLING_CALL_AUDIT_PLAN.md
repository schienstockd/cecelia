# Sibling-call audit — parked plan

**Status:** **P1 + P2 built** (2026-09-26) on `docs/drift-catch-modes`; P0 skipped in favour of running live — the reviewer is now wired into every reservations recital via [`CLAUDE.md`](../../CLAUDE.md) and its prompt lives at [`docs/ai-assist/SIBLING_CALL_AUDIT.md`](../ai-assist/SIBLING_CALL_AUDIT.md).

**Related:**
- Parent decision: [`DRIFT_PREVENTION_ASSESSMENT.md`](DRIFT_PREVENTION_ASSESSMENT.md) — declined the general harness (2026-09-26). This plan reopens **one** of the three shapes that doc kept as "future option" (the scoped adversarial-review subagent), because the parent doc's own trigger — *"one is chance; two is a pattern that pays for a scoped adversarial reviewer"* — has already fired.
- Catch-mode audit: [`../archive/drift_catch_modes_audit.md`](../archive/drift_catch_modes_audit.md)

## Goal

Catch **case-F-shaped bugs** — *a fix that silently leaves other divergent copies of the same guard, resolver or contract broken* — at the pre-commit reservations step, before the fix lands.

## Why now — the trigger has fired

The parent decision doc says the reviewer earns its keep at a *second* case-F. A cross-check against the wider 1,225-PR window (grep bodies for `"sibling to"`, `"same latent bug"`, `"other callers"`, `silently`) surfaces **three confirmed case-F pairs**, not one:

- **#816 → #822** — viewer image-version pruning. #816 fixed stale-persisted-version on delete; #822 body opens *"Sibling to #816… re-import doesn't go through the delete path, so the same class of bug bit c91ICQ"* (production 404).
- **#828 → #839** — vault model-path resolution. #828 flipped the `denoiseModels` picker to bare stem "mirrors flowModels" but didn't teach the consumer `denoise_model_path` to accept a stem. Denoise segmentation errored in production until #839 collapsed both resolvers into one.
- **#1101 → #1151** — plot-Share stuck-annotator. #1101 landed on `SummaryCanvas` only; `GatingPlots` and `ClusterPlots` hand-rolls stayed broken.

Plus one live open instance the fix PR self-disclosed: **#812** flagged `ViewerPanel.loadObsCols:470` as an unfixed sibling — still sitting in the codebase, not counted above.

## Locked decisions

1. **Model = `claude-sonnet-5`.** Not Opus. The task decomposes into read-diff → name-symbols → grep-callers → per-site shape-match — many small independent reads, not one deep reasoning chain. Sonnet handles this shape well; Opus over-reasons at a cost that accumulates because the reviewer runs on *every* commit. The reviewer surfaces **candidates** for the user's judgement — the `confirmed`/`plausible` confidence marker does the calibration work Opus would otherwise refine. Not decided by measurement — decided by task-shape reasoning + the user's stated frustration with Opus over-reasoning on tight tasks; revisit if live runs show sonnet misses siblings that Opus would have caught.
2. **Trigger = the pre-commit reservations step, in-session.** Not a git hook, not CI. The implementing agent already recites reservations before every commit ([`CLAUDE.md`](../../CLAUDE.md) → *Git & commits*); the reviewer's findings fold into that recital under a "sibling-call audit" heading. Zero-latency; user decides at the same moment they were already deciding.
3. **Isolation = a fresh subagent.** `Agent(subagent_type: "general-purpose", model: "sonnet", prompt: …)`. Empty conversation — the reviewer must not inherit the implementing session's reasoning. That's the whole "adversarial" property.
4. **Scope = by symptom, not by directory.** The reviewer's prompt says: *"For each guard, resolver, or helper this diff modifies, grep for other callers in the repo. Report any caller the diff didn't update but arguably should have."* The three confirmed case-F pairs sit in `frontend/src/utils`, a Julia vault-path helper, and canvas composables respectively — a reviewer scoped to the parent doc's proposed directories (`frontend/src/components/`, `app/src/gating/`, `app/src/tasks/chain/`) would have caught **zero of three**.
5. **Reviewer prompt lives in the repo, versioned.** As [`docs/ai-assist/SIBLING_CALL_AUDIT.md`](../ai-assist/SIBLING_CALL_AUDIT.md), cited from [`CLAUDE.md`](../../CLAUDE.md) → *Git & commits* → *Reservations*. Not baked into a hook, not embedded in a skill file the user can't easily edit — the shape of what "sibling" means will iterate. Placed under `docs/ai-assist/` (not the parent doc's speculative `docs/dev/`) because that directory already houses AI-assist process docs (`OBSERVER.md`, `QC-PROCESS.md`) and doesn't need to be created for one file.
6. **P0 (measure-first) skipped.** Cost gate dropped in favour of running the reviewer live from commit 1. Rationale: manual-invocation gate was recognised as forgettable — by the time the reservations recital fires, the moment to have asked has passed. The prompt's own short-circuit (`no sibling-call audit needed`) plus the docs-only / new-file-only skip valves make the invocation cost near-zero on trivial commits. Signal:noise gets measured in prod use; if the reviewer proves noisy, tighten the prompt via a normal doc edit.
7. **No GitHub CI backstop.** Considered and ruled out: runner minutes are free on the public repo, but the reviewer call itself needs a Claude invocation, and the Enterprise-seat-via-`claude`-CLI setup (memory-noted: no direct Anthropic API access) isn't shaped for headless CI. Copilot is a different vendor and does not substitute. Local-only.
8. **The reviewer is the ONLY hook of the three the parent doc considered that we build.** `PreToolUse` write-gates and `SessionStart` context injection stay declined — the parent doc's cost-gradient reasoning against them still holds; the nested-CLAUDE.md discipline (verified: `./CLAUDE.md`, `./frontend/CLAUDE.md`, `./app/CLAUDE.md` all exist) is load-bearing.

## Phases

### P0 — Measure (SKIPPED per Decision 6)

Original plan called for manual invocation on 3–5 PRs first as a signal:noise gate. Dropped — the manual gate was itself the failure mode (forgotten at the moment of reservations). Measurement now happens in prod use.

### P1 — Encode the reviewer prompt (SHIPPED)

[`docs/ai-assist/SIBLING_CALL_AUDIT.md`](../ai-assist/SIBLING_CALL_AUDIT.md) — the exact prompt the subagent gets, plus the mechanism note (how to spawn) and the escape valves (docs-only, new-file-only). The `## Reviewer prompt` section is what gets passed verbatim; everything above it is context for the human reader.

### P2 — Reservations recital picks it up automatically (SHIPPED)

Added a paragraph to [`CLAUDE.md`](../../CLAUDE.md) → *Git & commits* → immediately after the existing reservations rule. Requires spawning the reviewer + folding findings under a `**Sibling-call audit:**` heading, always printed (so the record shows the check ran even on skips).

## Reservations

- **Untested at N=0.** The whole plan turns on the P0 measurement. If P0 says the reviewer misses real cases or fabricates noise, this plan parks.
- **Sonnet vs Opus not measured for this specific task.** Decision 1 is grounded in general reasoning-overhead feedback, not a head-to-head on sibling-call review. P0's Opus spot-check is where that gets checked.
- **Cost per invocation unknown.** Every reservations recital would spawn one reviewer subagent — even on diffs where no fix is being made (a purely additive PR). P1's prompt should short-circuit on "diff contains no fix-shaped hunks" to avoid the always-on tax.
- **Symptom scope is a heuristic.** "Guards, resolvers, helpers" is loose. A fix to a copy-pasted block that isn't a helper (raw duplicated logic) is still a case-F candidate and the current phrasing may miss it. Iterate the prompt in P0.
- **Reviewer can be talked into "no siblings" by a well-written diff.** Fresh context helps but isn't proof against a fix whose commit message convincingly explains why the sibling shouldn't be updated. Doesn't invalidate the mechanism; means findings are advisory, not blocking.
- **Doesn't retroactively catch #812's still-open sibling.** That's a today-in-the-codebase bug the reviewer would only catch on the next PR that touches `ViewerPanel.loadObsCols` — filing it as a separate `docs/TODO.md` item is out of scope for this plan but worth the two-line note.

## References

- Parent decision: [`DRIFT_PREVENTION_ASSESSMENT.md`](DRIFT_PREVENTION_ASSESSMENT.md)
- Catch-mode audit: [`../archive/drift_catch_modes_audit.md`](../archive/drift_catch_modes_audit.md)
- Anthropic Claude Code best-practices — https://code.claude.com/docs/en/best-practices → adversarial-review pattern
