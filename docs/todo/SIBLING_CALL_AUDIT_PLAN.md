# Sibling-call audit — parked plan

**Status:** planning (2026-09-26) · no branch yet · unbuilt.

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

1. **Model = `claude-sonnet-5`.** Not Opus. The task is bounded (read the diff, grep the repo, list siblings), doesn't reward long reasoning chains, and Opus's reasoning overhead has been shown to burn tokens without changing the answer on tasks this small.
2. **Trigger = the pre-commit reservations step, in-session.** Not a git hook, not CI. The implementing agent already recites reservations before every commit ([`CLAUDE.md`](../../CLAUDE.md) → *Git & commits*); the reviewer's findings fold into that recital under a "sibling-call audit" heading. Zero-latency; user decides at the same moment they were already deciding.
3. **Isolation = a fresh subagent.** `Agent(subagent_type: "general-purpose", model: "sonnet", prompt: …)`. Empty conversation — the reviewer must not inherit the implementing session's reasoning. That's the whole "adversarial" property.
4. **Scope = by symptom, not by directory.** The reviewer's prompt says: *"For each guard, resolver, or helper this diff modifies, grep for other callers in the repo. Report any caller the diff didn't update but arguably should have."* The three confirmed case-F pairs sit in `frontend/src/utils`, a Julia vault-path helper, and canvas composables respectively — a reviewer scoped to the parent doc's proposed directories (`frontend/src/components/`, `app/src/gating/`, `app/src/tasks/chain/`) would have caught **zero of three**.
5. **Reviewer prompt lives in the repo, versioned.** As `docs/dev/sibling-call-audit-prompt.md`, cited from [`CLAUDE.md`](../../CLAUDE.md) → *Git & commits* → *Reservations*. Not baked into a hook, not embedded in a skill file the user can't easily edit — the shape of what "sibling" means will iterate.
6. **Cost gate before it goes standing — P0 measures first.** Run it manually on the next 3–5 PRs the user would have committed anyway; capture findings, note signal-to-noise. If it surfaces at least one real sibling in that window it's paying; if it's all noise, tighten the prompt before making it default.
7. **No GitHub CI backstop.** Considered and ruled out: runner minutes are free on the public repo, but the reviewer call itself needs a Claude invocation, and the Enterprise-seat-via-`claude`-CLI setup (memory-noted: no direct Anthropic API access) isn't shaped for headless CI. Copilot is a different vendor and does not substitute. Local-only.
8. **The reviewer is the ONLY hook of the three the parent doc considered that we build.** `PreToolUse` write-gates and `SessionStart` context injection stay declined — the parent doc's cost-gradient reasoning against them still holds; the nested-CLAUDE.md discipline (verified: `./CLAUDE.md`, `./frontend/CLAUDE.md`, `./app/CLAUDE.md` all exist) is load-bearing.

## Phases

Each phase is independently useful; stop after any of them if the evidence stops paying.

### P0 — Measure (unbuilt)

Manually invoke a fresh sonnet subagent on the next 3–5 PRs before commit. Prompt draft: *"Here is `git diff --staged`. For each guard, resolver, or helper it modifies, grep the repo for other call sites. Report any caller the diff didn't update but arguably should have. Include file:line for each finding. Under 300 words."* Track:

- How many findings per PR (target ≥1 real sibling across the 3–5 sample).
- Ratio of real to noise findings.
- Wall-clock cost.
- Whether Opus disagrees with Sonnet on any of these (single Opus re-run per PR, spot check).

Decision gate at end of P0: proceed to P1 only if signal-to-noise justifies standing invocation. If it doesn't, park this plan with the P0 numbers written up.

### P1 — Encode the reviewer prompt

Write `docs/dev/sibling-call-audit-prompt.md` from the P0-iterated prompt. Cite it from `CLAUDE.md`'s Reservations section: *"Before commit, spawn a fresh subagent per `docs/dev/sibling-call-audit-prompt.md` and fold its findings into the reservations recital under a 'Sibling-call audit' heading."*

### P2 — Reservations recital picks it up automatically

Update `CLAUDE.md`'s reservations rule so the recital template includes a mandatory *"Sibling-call audit:"* line (either the reviewer's findings or an explicit "no sibling-call audit run" note). No enforcement code — a rule the agent follows the way it follows the reservations rule itself.

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
