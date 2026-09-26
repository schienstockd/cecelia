# Sibling-call audit — parked plan

**Status:** **P1 + P2 built** (2026-09-26) on `docs/drift-catch-modes`; P0 skipped in favour of running live — the reviewer is now wired into every reservations recital via [`CLAUDE.md`](../../CLAUDE.md) and its prompt lives at [`docs/ai-assist/SIBLING_CALL_AUDIT.md`](../ai-assist/SIBLING_CALL_AUDIT.md).

**Prior-art scope search:** [`docs/ai-assist/DRIFT_DETECTION_PRIOR_ART.md`](../ai-assist/DRIFT_DETECTION_PRIOR_ART.md) — why we didn't adopt drift-analyzer / Revieko / Conclave / AgentSync and what we borrowed.

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

## What actually justifies this build

Not the parent doc's cost-gradient argument. That argument justifies **not** building `PreToolUse` / `SessionStart` (they'd reprice the wrong cost, and pay discovery upfront on every session including the 90% that don't need it). It does **not** carry over to justify this reviewer — which is not a hook in the repricing sense at all. Findings are advisory, folded into a recital; nothing here blocks a write or makes the cheap path expensive.

The actual argument for this build is smaller and different: **a fresh-context sonnet subagent doing targeted grep is cheap on trivial diffs (short-circuit + escape valves keep the invocation near-zero) and catches more than a human eyeball skim of a fresh diff — because the human has just written the fix and knows what it's supposed to do, so their eye slides past the siblings that don't fit the mental model of "the change I just made"**. That's the case-F failure mode. Fresh context breaks the mental-model grip.

Both arguments can be right in isolation. Keeping straight which claim justifies which build matters — this reviewer is a sharper *catch* tool slotted into the existing catch layer, not a new *prevention* layer.

## Leading indicator — watch this before you watch findings

**"Did the `**Sibling-call audit:**` heading print on every non-trivial commit?"** This is the leading indicator, before "did it catch anything." A silently-skipped recital looks identical to a genuinely-empty audit — zero findings and no output are indistinguishable at the record level. If the heading is missing, the whole mechanism is dark.

Concrete failure the trigger is watching for: on 2026-09-26 (the day this shipped), a session ran reservations on `audit/plot-grid-tile` and skipped the sibling-call audit entirely — satisfied the memory-cached "state reservations" habit without noticing CLAUDE.md had grown a companion sibling-call rule. Structural fix (fold the two rules into one paragraph in CLAUDE.md, plus an auto-memory) shipped in the same commit as this note. If it re-occurs on another agent or another install, the mechanism has a second, deeper, enforcement-tier problem — reach for a `Stop` hook.

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

### P2 — Reservations recital picks it up automatically (SHIPPED, then folded, then reshaped to Kiwi-weave)

Iteration history: first shipped as a separate paragraph in [`CLAUDE.md`](../../CLAUDE.md) → *Git & commits*, adjacent to the existing reservations rule. Same day, a session's memory-cached "state reservations" habit fired the older rule and stopped without emitting the heading. Folded into a single reservations rule where the sibling-call heading was a mandatory `(b)` half. **Reshaped again** to the Kiwi claims+references pattern the user prefers: findings weave INTO the reservations list as prioritized items (a `**confirmed**` sibling becomes a reservation item, not a separate output block), followed by the raw reviewer output verbatim under a `_Sibling-call audit (evidence):_` fold so each woven item cites its source, then a one-line tail `_Sibling-call audit: run_` (or a skip variant) as the leading indicator. Auto-memory `feedback_reservations_include_sibling_call.md` binds the whole thing together for future sessions.

**Why the weave over the fold:** the fold kept sibling findings under their own heading — easy for Opus to paste raw and skate past. The weave forces Opus to *engage with* the findings (rank them, integrate them into the same voice as the other reservations); the evidence fold prevents Opus from filtering a finding out silently; the tail line is the "did it run" indicator. Kiwi-shape (claims + citation) is the user's established pattern.

## Reservations

- **First live run (2026-09-26) on `audit/plot-grid-tile` produced useful output.** Two fix-shaped hunks, classified cleanly (sole-caller, structurally-immune, deliberately-separate-algorithm, latent-recur). The latent-only finding on `CanvasPanel.vue` is the highest-value shape — a forecasted case-F, not a historical one. Single data point.
- **Sonnet vs Opus not measured for this specific task.** Decision 1 is grounded in general reasoning-overhead feedback, not a head-to-head on sibling-call review. Revisit if live runs show sonnet missing what opus would catch.
- **Enforcement-tier collision (structural).** The reviewer only fires if the reservations recital fires, and the recital is itself a CLAUDE.md instruction — the same advisory tier whose reliability was the original problem. The P2 fold + auto-memory close the local case (session's own memory-cached shortcut); real cross-session enforcement would need a `Stop` hook that fails if the last agent output doesn't contain the heading. Not built. Watch the leading indicator (above) — reach for the hook only if it re-fails.
- **Symptom scope is a heuristic.** "Guards, resolvers, helpers" is loose. A fix to a copy-pasted block that isn't a helper (raw duplicated logic) is still a case-F candidate and the current phrasing may miss it. Iterate the prompt as failures surface.
- **Reviewer can be talked into "no siblings" by a well-written diff.** Fresh context helps but isn't proof against a fix whose commit message convincingly explains why the sibling shouldn't be updated. Doesn't invalidate the mechanism; means findings are advisory, not blocking.
- **Doesn't retroactively catch #812's still-open sibling.** That's a today-in-the-codebase bug the reviewer would only catch on the next PR that touches `ViewerPanel.loadObsCols` — filing it as a separate `docs/TODO.md` item is out of scope for this plan but worth the two-line note.

## References

- Parent decision: [`DRIFT_PREVENTION_ASSESSMENT.md`](DRIFT_PREVENTION_ASSESSMENT.md)
- Catch-mode audit: [`../archive/drift_catch_modes_audit.md`](../archive/drift_catch_modes_audit.md)
- Anthropic Claude Code best-practices — https://code.claude.com/docs/en/best-practices → adversarial-review pattern
