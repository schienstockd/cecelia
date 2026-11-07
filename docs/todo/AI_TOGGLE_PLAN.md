# AI toggle: make every model-backed feature optional

**Status:** **PARKED** (2026-11-07). Nothing built. One decision locked (D1). P0 done as a draft:
[`AI_SURFACES_AUDIT.md`](AI_SURFACES_AUDIT.md), whose §9 has the full slice plan. Next step: P1.
Brief: [`docs/archive/AI_SURFACES_AUDIT_PROMPT.md`](../archive/AI_SURFACES_AUDIT_PROMPT.md). Read its
banner first, because four of the brief's premises had moved by the time it was archived.

## Goal

One setting that hides and disables everything model-related. AI should be clearly optional, and the
platform should be useful without it. This is hardening work: clarity and robustness, not new features.
It also backs the paper's position that AI is an optional, bounded layer. Mechanical surfaces must look
and work the same with AI off: checks, light-path and data-path panels, acquisition cards, QC,
segmentation, tracking, and rule-based findings.

## Verified facts (`origin/main` c7f0af6d)

- **The model entry points named in the brief exist:** `run_agent_turn`
  (`app/src/ai/agent_runner.jl`) and `run_kiwi_turn` (`api/src/kiwi_turn.jl`), with `kiwi_start_turn`
  (`api/src/kiwi_api.jl`) as the background-job wrapper.
- **The claim review goes through the Kiwi path:** `api/src/claim_review.jl` → `kiwi_start_turn` →
  `run_kiwi_turn`. The recital's Translator (S3a) is parked, so the review turn is the recital's only
  model surface (`IMAGE_ANALYSIS_RECITAL_PLAN.md` → *Status*).
- **Credentials belong to Cecelia profiles, not OS accounts.** Each profile logs in with its own
  `claude auth login` (`api/src/kiwi_profile_api.jl`). `default` shares `~/.claude` with the terminal
  `claude` (`agent_runner.jl`, plan D21).
- **An install-wide policy table already exists.** `[sharedWorkstation]` in `custom.toml`
  (`app/src/config/shared_workstation.jl`) has `oneClaudeLogin`, `claudeLogoutOnQuit` and
  `purgeClaudeConversations`, all off by default. Settings → Shared workstation shows it to the
  `default` profile only, which the file calls "a visibility gate, not security". The file also says
  there is **deliberately no master switch**.
- **A data notice already exists.** `CLAUDE_DATA_NOTICE` (`frontend/src/lib/claudeOverview.ts`) reads
  "What Claude reads is sent to Anthropic" and "Check your institute's rules for unpublished or patient
  data". Kiwi shows it as a standing note that can't be dismissed, and `claudeOverview.test.ts` pins it.
- **Maturity:** in the draft `FEATURE_MATURITY_TIERS.md`, only `assist-with-kiwi` has a tier
  (Experimental).
- **Neighbouring hardening work.** `HARDENING_SURFACES.md` → *Claude+MCP (20 rows)* is an existing
  inventory of AI surfaces, with entry points, code paths and tests. `HARDENING_AUDIT_PLAN.md` plans a
  surface manifest with tier badges (step 7, design only) and an opt-in toggle for Experimental surfaces
  (step 9, "don't yet"). This plan has to reuse that manifest or explain why it doesn't.
- **Not checked yet (P0's job):** whether *every* model call goes through these entry points. Likely
  candidates to bypass them are the observer's terminal `claude` launch (`api/src/observer_api.jl`),
  `pixi run chat` (`chat.py`), the login spawns, and dev tooling.

## Decisions

1. **"Off" covers the MCP observer, enforced at the backend** (2026-11-07). Cecelia never runs the
   observer as a service. A `claude` process starts it from a config file, whether that's one of the
   app's own turns or the user's terminal. The observer is a thin HTTP client: every tool call is a
   request to the running backend with the API token (`mcp/cecelia_mcp/client.py`, `auth.py`). Calls
   from Claude already send `X-Cecelia-Client: claude`, which `api/src/server.jl` reads on every
   request. With AI off, the backend refuses those requests and returns "AI is off in Cecelia", and
   the app's own turns never start. One point covers the in-app turns and the terminal `claude`.
   - **The header is not a security boundary.** A token holder could leave it off. That is
     acceptable, because the token holder is the person whose data it is. The switch is a policy for
     that person's own use, not a defence against them.
   - **The user's Claude Code registration stays in place.** With AI off, the tools still appear,
     and each call answers with the message above. That is a clear answer, not a dead end.

2. **The machine-wide switch belongs to peanut** (2026-11-07). It is an item in Settings → *Shared
   workstation* (`[sharedWorkstation]`), which only peanut (`default`) sees. It only matters on
   workstations. Like the rest of that section it is a visibility gate, not security (consistent with
   D1). It is one item, not a switch over the section's other items.
3. **Every other profile sets AI on or off in its own Preferences,** stored per profile on the
   server, within peanut's ceiling.

## Starting positions (from the brief, not locked)

These are the brief's positions. Each needs the audit's evidence and the maintainer's agreement before
it becomes a decision.

- **In scope:** Kiwi, Claude co-driving in the app and viewer, the claim review turn, and anything
  else that calls a model. **Out of scope:** everything mechanical (see *Goal*).
- **Enforcement below the UI.** With AI off: no model calls, no credentials read, nothing sent to a
  provider. One check at the entry point (if there is a single one), plus a test proving it.
  Hiding buttons alone is not enough.
- **A registry of AI surfaces** in one data file. The UI, the opt-in dialog's "what is sent" list,
  the maturity map and the paper's description all read from it. A test requires every model path to
  appear in the registry.
- **Default off, opt-in.** A one-time dialog at enable time. Turning AI on for the first time starts in
  "on request".
- **No dead ends when off.** No empty panels and no nagging. At most one deliberate entry point.

## Open — maintainer calls

1. ~~**Master switch vs the shared-workstation design.** The brief's admin "ceiling" is a master switch
   with enforcement. `shared_workstation.jl` rejects a master switch, and its visibility gate is not
   security. Options: reverse that decision for AI specifically, or make the ceiling a separate kind of
   setting (a policy, not a convenience item) that lives somewhere else.~~ Decided: D2.
2. **Which states to support:** off / on request / on. Is "on request" needed?
3. **The user layer.** ~~Per profile?~~ Decided: D3. Still open: a project-level override?
4. **Default state** for unmanaged single-user installs (the brief says off everywhere).
5. **Entry point when AI is off:** a subtle Kiwi link that opens "AI is off", or the settings entry
   plus a first-run message only.
6. **Stored AI content when off:** hidden, or shown with a provenance mark. Stamps already record model
   and session (#1826).
7. ~~The MCP observer and the terminal `claude`.~~ Decided: D1.
8. **The registry's home.** The closest existing one is `frontend/src/lib/claudeTools.ts`
   (`CAPABILITIES` / `TOOL_GROUPS`, read by `ClaudeOverviewDialog`). It is already a user-worded list
   of what Kiwi and a Claude Code session can use, and its test fails when an MCP tool is missing.
   Extend it, add a column or filter to the hardening manifest (`HARDENING_AUDIT_PLAN.md` step 7), or
   keep a separate file only if the registry needs per-route data that neither of those carries. Related: does step 9's Experimental toggle merge with this one?
9. **Is a local model planned?** If so, the toggle distinguishes "a model runs" from "data leaves
   the machine".

## Phases

Each phase is a standalone PR. Sizes, risks and tests per slice: audit §9.

- **P0 · Audit (docs only).** Run the brief with the banner's corrections. Start from
  `HARDENING_SURFACES.md` → *Claude+MCP* and extend it rather than re-deriving it. Output:
  `docs/todo/AI_SURFACES_AUDIT.md`, containing the inventory table, the entry-point analysis with every
  bypass, data egress, UI coupling, stored content, existing tests, and profiles/installs. Verified facts
  are kept separate from assumptions. **Done (draft):** in-app turns have one seam
  (`kiwi_start_turn` → `run_agent_turn`); every other path is one route, plus two holes (the push writer
  and the autonomous WS header).
- **P1 · Gate at the entry point plus the enforcement test.** This includes D1's backend refusal of
  requests from Claude. Small, per P0 (audit §8.7, slice 2), and it can start now.
- **P2 · Registry** (home per call 8), with the test that every model path is registered.
- **P3 · Setting plus default plus opt-in dialog.** Blocked on calls 1–5. The dialog's "what is sent"
  list is read from P2, and its egress lines reuse `CLAUDE_DATA_NOTICE` rather than repeating them. Link to Anthropic's terms rather than paraphrasing them.
- **P4 · Hide surfaces one area at a time,** most visible first. Each mixed surface follows its
  degradation rule from P0.
- **P5 · User docs:** what AI does in Cecelia, what it sends, and what keeps working with AI off.

P0, P1 and P2 fit the hardening phase. P3 waits on the open calls.
