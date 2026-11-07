> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in [`docs/todo/AI_TOGGLE_PLAN.md`](../todo/AI_TOGGLE_PLAN.md).
>
> **Outcome:** not run as of archiving (2026-11-07, `origin/main` c7f0af6d); no
> `docs/todo/AI_SURFACES_AUDIT.md` exists. Read against main, four premises had moved:
>
> 1. **"Claim review and translation (S3a, S4)."** After P2 the recital is rules plus one plain
>    review turn (`docs/todo/IMAGE_ANALYSIS_RECITAL_PLAN.md` → *Status*). The Translator (S3a) is
>    parked, and "Check claim" was renamed to **Review** in the v2 mockup. The one model surface is
>    `api/src/claim_review.jl`, which runs as a Kiwi turn (`kiwi_start_turn` → `run_kiwi_turn`). The
>    B6 GUI was not on main.
> 2. **A shared-workstation policy layer already exists.** `[sharedWorkstation]` in `custom.toml`
>    (`app/src/config/shared_workstation.jl`) has `oneClaudeLogin`, `claudeLogoutOnQuit` and
>    `purgeClaudeConversations`, all off by default, under Settings → Shared workstation. Its header
>    says there is **deliberately no master switch** ("a mode that flips several items at once hides
>    which one changed behaviour"), and showing it only to the `default` profile is "a visibility
>    gate, not security". The brief's admin ceiling runs against both of these choices.
> 3. **"Users" are Cecelia profiles, not accounts.** Each profile gets its own Claude login through
>    `claude auth login` (`api/src/kiwi_profile_api.jl`). Item 7 has to start from that.
> 4. **"Most AI features are Experimental"** has no source. `docs/todo/FEATURE_MATURITY_TIERS.md`
>    (a draft) gives a tier only to `assist-with-kiwi` (Experimental) and leaves its claim
>    validation unjudged. No acquisition **advisor** chat exists: `docs/todo/ACQUISITION_ADVISOR_PLAN.md`
>    builds the mechanical Milestone 0 first, and its model-backed core (Milestone 1) is unbuilt.
>
> One premise does hold: "the short existing warning in Kiwi" is `CLAUDE_DATA_NOTICE`
> (`frontend/src/lib/claudeOverview.ts`), shown in `KiwiCockpit.vue`. It is always on and can't be
> dismissed (`docs/ai-assist/OBSERVER.md` → *The data notice*). The opt-in dialog should reuse it
> rather than keep a second copy.

# AI_SURFACES_AUDIT_PROMPT

Audit and plan only. Do not implement. Output: `docs/todo/AI_SURFACES_AUDIT.md`. Self-contained: do not assume other context.

## Goal

Cecelia has AI features spread across the app (Kiwi, Claude co-driving, model-backed review and advice, and others). Some users and labs do not want them. Size and plan a **global AI toggle** that hides and disables everything model-related, so AI is clearly optional, without making the PR too large to review.

This is hardening work (clarity and robustness, no new features). It also supports a position in the paper plans: the platform is useful without AI, and AI is an optional, bounded layer.

## What counts as "AI" (decide and justify in the audit)

Starting position, to be challenged with evidence:

- **In scope (model-backed):** Kiwi, Claude co-driving in the app and viewer, any feature that calls a model, including model-backed claim review and translation (S3a, S4 in `docs/todo/IMAGE_ANALYSIS_RECITAL_PLAN.md`) and any advisor chat.
- **Out of scope (no model involved):** mechanical checks (record gate, cohort consistency, conventions, perturbations), light-path and data-path panels, acquisition settings cards, QC measures, segmentation and tracking, rule-based findings. These must stay visible and work the same with AI off. If a surface mixes a mechanical result with model-written text, say how it degrades.
- **Edge cases to classify explicitly:** the MCP observer server (exposes data to external agents but does not call a model itself), stored AI-generated content (blackboard entries and findings written by Kiwi or an agent), Claude login and credential storage, any local or non-cloud model, dev-only tooling (the dev recital, `claude -p` spawns, worktree consoles). Dev tooling is likely out of scope for an end-user toggle, but record it.

## Audit first (read the repo, report facts with file and PR references)

1. **Inventory of every AI surface:** UI (menus, panels, buttons, plot actions, board features, dialogs), API routes, backend functions, MCP tools, background or scheduled model use, CLI entry points such as `pixi run chat`. For each: what it does, the layer (UI / API / backend / MCP), whether a model is called, what triggers it (automatic or on request), what data is sent to the model (prompts, images, metadata, records), which credentials it uses, and where it appears in the UI.
2. **The seam question.** The recital plan names a single entry point for model turns (the app agent seam: `run_agent_turn` in `app/src/ai/agent_runner.jl`, Kiwi's `run_kiwi_turn`, `api/src/kiwi_turn.jl`). Check whether **every** model call goes through one place. List each path that bypasses it (direct HTTP calls, subprocess spawns, MCP-side calls, frontend calls, scripts). This decides how cheap a strong guarantee is.
3. **Data egress and credentials:** what leaves the machine when a model feature runs, when credentials are read, and what happens at startup with the feature off.
4. **UI coupling:** how AI surfaces are wired into shared components (is there a central place such as a settings store or feature-flag system, or is each surface wired ad hoc?). Note existing flags, maturity badges or feature gates that could be reused (for example the maturity tiers and `lib/severity.ts` conventions).
5. **Stored AI content:** where model-written content is persisted (blackboard entries, findings, notes), how it is marked, and what should happen to it when AI is off (shown, hidden, or shown with a provenance mark).
6. **Tests that exist:** anything that already asserts model paths do not run in some mode.
7. **Users, installs and shared machines:** how Cecelia handles users (accounts or none, multi-user mode), where config and settings live (per install, per OS user, per project), where credentials such as the Claude login are stored and whose they are, whether projects can be shared between users or machines, and whether chats, caches or model-written content persist between sessions or users. Report what exists, not what is assumed.

## Design to plan (propose, do not build)

- **Toggle states:** off (no model use anywhere), on request (models run only when the user clicks, as "Check claim" does), on. Which states are needed?
- **Where the setting lives: layers, where a stricter outer layer always wins.** Starting proposal, to be checked against what the audit finds in item 7:
  1. **System or install (admin policy), the ceiling.** Sets whether AI is available at all: off, on request, or on. A lab or facility that does not allow data to leave the machine sets it here, and users cannot override it.
  2. **User preference, within the ceiling.** The everyday toggle. A user can turn AI off for themselves, or on if the admin allows it, and sees only the options the admin permits.
  3. **Project (optional, likely later).** For datasets with restrictions (unpublished or patient-derived data): AI stays off for that project whatever the user prefers.
  If Cecelia has no real user accounts, say so and propose what the user layer reduces to (for example a per-OS-user or per-session setting).
- **Default state: off, opt-in.** Reasons: data leaves the machine, so consent should be explicit; the features need a login and setup anyway; the platform and its reproducibility claims should hold with no model; shared and managed installs default to off already; most AI features are Experimental. Costs to record: lower discoverability, and opt-in bias in any usage or outcome data (users who enable it are likely enthusiasts, so uptake is not evidence that it helps; include sceptical users in tests). Mitigation: one non-nagging first-run message ("AI features are available and off") plus a settings entry. First enable starts in "on request", not fully on. If the audit finds a reason to prefer default-on for unmanaged single-user installs, say so, but managed and shared installs stay off.
- **Opt-in dialog, shown once at enable time.** The short existing warning in Kiwi stays as a reminder. The dialog states why AI is off by default, what is sent, what you gain, the risks, and the controls. The "what is sent" list is generated from the surface registry so it stays accurate. Do not state anything about retention or training use unless confirmed from Anthropic's current terms for the account types users have; link to those terms instead of paraphrasing. Draft text to refine:

  > **Turn on AI features?**
  > Off by default because these features send data to a third-party service (Anthropic). That choice should be yours, your lab's and your data policy's.
  > - **Sent:** your messages and, per feature, parts of your project [list from the registry].
  > - **Gain:** faster exploration, help checking claims, acquisition advice.
  > - **Risks:** data leaves your machine; the AI can be wrong or vary between runs, and you stay responsible for the analysis; usage counts against your account.
  > - Check your institution's policy before using unpublished, patient-derived or restricted data.
  >
  > You can switch it off at any time, and an administrator can disable it. Everything else in Cecelia works the same with AI off.
  > [Ask before each use] [Turn fully on] [Cancel]
- **Entry point when AI is off (decide, with a recommendation).** With default off, people may never find the setting. Candidate: keep the Kiwi button as one subtle, visible link. When AI is off, it opens a short modal ("AI features are off") with an "Open AI settings" link, which leads to the opt-in dialog; it does not switch AI on directly, so consent stays explicit. Considerations for the decision:
  - It is the single exception to "hide everything AI", so a "Hide AI entry points" option (in the modal and in settings) may be needed for people who don't care.
  - It must respect admin policy: if an administrator has disabled AI, either hide the button or show "AI is disabled by your administrator" with no way to enable.
  - Other surfaces (claim review, advisor, etc.) stay hidden when off, with no prompts, so this does not turn into nagging.
  - Compare with alternatives, such as a settings entry only plus the one-time first-run message, and say which you recommend and why.
- **Shared workstations:**
  - Default to AI off at system level on shared machines, and make enabling it a deliberate admin step.
  - Credentials and settings belong to the user, not the machine, so one person's Claude login is never used in someone else's session.
  - No carry-over of AI state between users: open chats, cached prompts and stored AI findings in a shared project must not leak or be inherited.
  - A visible indicator of the effective state and who set it (for example "AI off: set by admin"), so the user is not left guessing.
  - Model-written content in a shared project carries a provenance mark, so people with AI off can tell which content came from a model.
  Report which of these the current design already satisfies and which it would break.
- **Enforcement below the UI:** with AI off, no model calls, no credentials read, no data sent to a model provider. A single check at the seam, if the audit shows a seam exists, plus a test that proves it. Hiding buttons alone is not enough.
- **A registry of AI surfaces:** one data file that lists every surface (id, layer, what it does, what it sends, where it appears), so the UI reads from it, the toggle stays cheap to maintain, and the list doubles as the inventory for the maturity map and for describing the platform in a paper.
- **No dead ends when off:** no empty panels and no nagging "enable AI" prompts (the single entry point described below is the one deliberate exception). Findings from mechanical checks read the same either way. Say how each mixed surface degrades.
- **Regression test:** with the toggle off, no model path runs. It must keep passing as new surfaces appear (for example by requiring every AI surface to be in the registry).
- **Stored AI content when off:** a recommendation and the reasoning.

## Slice plan

Break the work into small PRs that each stand alone, with an estimate of size, risk and test for each. A suggested order to check against the audit:

1. This audit (docs only).
2. The gate at the seam, with the enforcement test. Likely the smallest and most important.
3. The registry.
4. Default off and the opt-in dialog, with its "what is sent" list read from the registry.
5. Hiding surfaces one area at a time, most visible first.
6. User-facing documentation: what AI does in Cecelia, what it sends, what stays on with AI off.

State what can start now during the hardening phase and what waits. Say which slices depend on the answer to the seam question.

## Output requirements

- Inventory table (surface, layer, model called, trigger, data sent, credentials, UI location, file references).
- Seam analysis: the single entry point if it exists, and every bypass.
- Classification of edge cases (MCP server, stored content, credentials, local models, dev tooling).
- Toggle design with a recommendation, enforcement design, registry format, degradation rules.
- A recommendation on the default state, and the opt-in dialog text with the registry fields it reads. Flag any statement in the dialog that needs verification (data sent per surface, account terms).
- Slice plan with sizes and tests.
- Separate verified facts (from the repo) from assumptions and open questions.
- Open questions for the maintainer, for example: should an "on request" state exist, is a local model planned, should stored AI content be hidden or marked, is an admin-level ceiling needed (and where would it be stored), is a project-level override wanted, and are shared workstations a supported setup.
- No new features and no speculation presented as fact.
