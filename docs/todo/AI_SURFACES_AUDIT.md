# AI surfaces audit: what a global "AI off" toggle has to cover

**Status:** **draft** (2026-11-07). P0 of [`AI_TOGGLE_PLAN.md`](AI_TOGGLE_PLAN.md). Audit and plan only; nothing built.

- Repo read: `origin/main` c7f0af6d plus the plan branch (docs only). Date 2026-11-07.
- Method: the model-call paths (§2, §3, §7) were traced by hand. Two read-only sweeps covered the
  frontend surfaces (§1b, §4) and stored content and tests (§5, §6), and their load-bearing claims were
  re-checked against the code before use. Nothing was run.
- Extends `HARDENING_SURFACES.md` → *Claude+MCP (20 rows)*, which lists the MCP tool groups and ways
  in. This adds what those rows don't have: model paths, data sent, gates, storage, and tests.
- **Verified** = read in the code at the cited line. **Inferred** and **unverified** are marked where
  they occur, and collected again in §11.
- Brief: [`../archive/AI_SURFACES_AUDIT_PROMPT.md`](../archive/AI_SURFACES_AUDIT_PROMPT.md).

## Summary

1. **In-app model use already has one seam.** The app spawns a model turn at exactly one line
   (`_run_agent_once`, `app/src/ai/agent_runner.jl:889`), reached only through `run_agent_turn` →
   `run_kiwi_turn` → `kiwi_start_turn`. The two routes that reach it are `POST /api/kiwi/turn` and
   `POST /api/claim/review`. Nothing runs a turn automatically: the old "Watch" pass is gone. A gate in
   `kiwi_start_turn` covers all in-app model use.
2. **Everything else that reaches a model is started by the user's own `claude`.** That covers
   `pixi run chat`, the terminal plugin, and Kiwi's "Start in terminal". Each one depends on a backend
   route or on backend requests, so D1 (refuse requests from Claude) plus refusing a short list of
   routes covers them. There are two holes. The push writer sends captures **out** to a paired session
   with no request involved. The autonomous server's WebSocket omits the `X-Cecelia-Client` header.
3. **Pixels leave the machine.** Kiwi's tools include image crops, gate views and capture PNGs, not
   only metadata. The opt-in dialog has to say so.
4. **The frontend has no central AI switch.** The Ask button isn't gated even by `observer.available`.
   The one existing AI toggle, "AI help" (`guidesShowClaude`, default off), only affects the Guides map.
   About 28 surfaces are wired ad hoc.
5. **Provenance is bound centrally but recorded per route.** Most writes from Claude carry
   `via: claude` and the model. The lab log, saved Kiwi turns, the claim-review write-back and anything
   sent over WebSocket do not.
6. **There is no admin layer anywhere.** Config is always per OS user (`~/.cecelia`), and every key in
   `custom.toml` overrides the bundled `config.toml`. The system install shares only the runtime.
   `[sharedWorkstation]` serves one shared OS login, not several accounts on one box. **Decided:** the
   machine-wide switch lives there anyway, held by peanut, because the shared login is the workstation
   case that matters (§8.2).
7. **No test asserts that a model path stays off in some mode,** because no such mode exists. The
   fake engines (`_KIWI_AGENT`, `_KiwiFakeEngine`) make a "turn refused when off" test cheap.

## 0. What counts as AI

| Class | Members | Why |
|---|---|---|
| **In scope: calls a model** | Kiwi Ask and Follow up; claim Review; `pixi run chat`; terminal `claude` sessions using the observer | a model reads project data |
| **In scope: hands out a model path** | Kiwi "Start/Continue in terminal" (the one-liner); terminal plugin setup (`/api/observer/register`); push to a paired session | no model call by the app, but it is the route data takes to one |
| **In scope: credential handling** | Claude login (`claude auth login`), `auth status`, the account chip | reads or writes the profile's Claude credentials |
| **Out of scope: mechanical** | claim checks S0–S2 (`/api/claim/checks`, `check_claim`), QC, cohort and convention checks, light-path and data-path panels, acquisition card, segmentation, tracking, gating, rule findings | no model; must look and work the same with AI off |
| **Edge: MCP observer** | both MCP servers | calls no model itself, but serves data to one. **In scope under D1** |
| **Edge: stored AI content** | §5 | not a call, it is a record. Stays readable; see §8.9 |
| **Edge: cleanup that touches credentials** | shared-workstation logout and conversation purge | removes credentials and transcripts and never sends project data, so it **keeps running with AI off** |
| **Edge: local model** | none exists. `observer_agent_bin()` reads `[ai].agent_bin` (`agent_runner.jl:39`), so the binary is swappable, but no non-Claude engine is implemented | open question 9 |
| **Out of scope: dev tooling** | recital, judge, agent_eval (`claude -p` via `resolve_claude_bin`) | not part of the shipped app; recorded in §2 B5 |

## 1. Inventory

### 1a. Backend: routes and processes

| Surface | Layer | Model called | Trigger | Data sent | Credentials | Where in the UI | Refs |
|---|---|---|---|---|---|---|---|
| Kiwi turn | API → process | **yes** | user (Ask, Follow up) | prompt, refs, Kiwi tool results incl. **images** (§3) | profile `CLAUDE_CONFIG_DIR` | Kiwi panel | `kiwi_api.jl:84,238`; `kiwi_turn.jl:528` |
| Claim review | API → process (as a Kiwi turn) | **yes** | `POST /api/claim/review`; **no caller on main** (no frontend, no MCP tool; the GUI is B6, #1985) | claim + record through Kiwi's tools minus the withheld ones | same | none yet; results land as `### sNN · Review` on the claim entry | `claim_review.jl:12,62,71-96` |
| Chat session | route → `chat.py` spawn | **yes** (the user's session) | `pixi run chat` | whatever the session's tools return | profile env from the route | terminal | `observer_api.jl` `api_observer_chat`; `chat.py:134` |
| Terminal one-liner | route | no (hands out a command) | Start / Continue in terminal | — | profile dir in the command | Kiwi | `kiwi_profile_api.jl:343` |
| Plugin setup | route → CLI | no | Set up / Fix, profile create | — | writes the user's Claude config | Kiwi, Preferences | `observer_api.jl:125`; `observer_plugin.jl` |
| Observer status | route | no | every project change, Kiwi mount | — | none; writes `observer-mcp.json` | drives `observer.available` | `observer_api.jl:106-114` |
| Login, status, logout | route → CLI | no | user; logout also on pick or quit by policy | — | reads or writes the profile credentials | Kiwi account row, ClaudeLoginPanel, sidebar Log out | `claude_auth.jl:73,91,267`; `kiwi_profile_api.jl` |
| Push to paired session | backend → socket | (into a live session) | user shares a capture with "Send to paired session" | capture notification | socket + token in `<proj>/settings/push_target.json` | Share dialog, pairing chip | `push_writer.jl:69`; `captures_api.jl:311` |
| MCP observer tools | MCP → HTTP | (serves a model) | the model's tool calls | everything the tools return | API token | — | `mcp/cecelia_mcp/server.py`; `client.py:242` |
| Autonomous MCP | MCP → HTTP + WS | (serves a model) | the model, in chat or unattended runs | as above; WS `task:run` / `chain:run` | API token | — | `autonomous.py:228,283` |
| Kiwi turn history | route | no | every project change (`stores/kiwi.ts:91`) | — | none | Kiwi feed | `GET /api/kiwi/turns` |

### 1b. Frontend surfaces

From the sweep, spot-checked. "obs" = gated by `observer.available` (`stores/observer.ts:11`).

| Area | Surfaces (file) | Starts a turn | Gate today |
|---|---|---|---|
| Kiwi entry | sidebar launcher (`PanelLaunchers.vue:62`); cockpit (`KiwiCockpit.vue`, mounted `App.vue:226`) | — | always shown; `kiwiOpen` |
| Kiwi Ask | `KiwiAsk.vue:223-245` Ask, Think first, model picker, Stop; feed with Follow up, Save to Blackboard, Continue in terminal | **Ask, Follow up** | **none on Ask** (`canAsk`, `KiwiAsk.vue:118`, checks busy and prompt only); model picker and Continue are obs |
| Data notice | `KiwiCockpit.vue:315` (`CLAUDE_DATA_NOTICE`) | — | always on inside Kiwi |
| Account and login | `KiwiCockpit.vue:319-339`, `ClaudeLoginPanel.vue`, sidebar Log out (`AppSidebar.vue:206`) | — | `accountLabel` |
| Terminal ways in | Start in terminal (`KiwiCockpit.vue:368`), chat starter (`:379`), Terminal row Set up / Fix (`:533`), Preferences → Kiwi plugin row (`PreferencesModal.vue:487-532`) | — | obs, except the chat starter |
| Overview | `?` → `ClaudeOverviewDialog.vue`, `ClaudeWaysIn.vue`, `lib/claudeTools.ts` | — | inside Kiwi |
| Hand-offs into Kiwi | "Add to Kiwi" on plots, tasks, Blackboard, viewer and the image menu (`AddToKiwiButton.vue` + 5 placements, `ImageTable.vue:774`); Share dialog "Attach to Kiwi", "Send to paired session" (`DrawSurface.vue:829-866`) | — | **none** |
| Profiles | Preferences → Profiles, `AppProfilePicker.vue`, `CreateProfileDialog.vue` (create also registers the MCP) | — | none. Profiles are core identity UI and stay; only the "Kiwi's login" wording and the MCP registration are AI |
| Settings | Shared workstation (`SettingsModule.vue:1415`); Other MCP connections (`:1428`) | — | `canManageMachine`; none |
| Guides | "AI help" ✦ on the map (`GuideMap.vue:77-131`); "Assist with Kiwi" guide (`singleTopicGuides.ts:430`); tip "Check a step with Claude" (`tips.ts:183`) | — | **`guidesShowClaude`** for the map only; guide and tip ungated |
| Model-made marks | lab log `[Claude]` entries and launcher badge "Claude noted" (`LabLogPanel.vue`, `PanelLaunchers.vue:55`); Blackboard ✦ agent runs, "by Claude", Claude section verdicts (`BlackboardModule.vue`, `SectionVerdict.vue`); proposed-gate ✦ (`PopulationManager.vue:607`); ROI "Claude" column (`RoiSection.vue:94`); "by Claude" on chains, notebooks and the model vault (`ChainModule.vue:647`, `NotebookTable.vue:68`, `vaultScope.ts:63`); icon legend (`iconLegend.ts:59`) | — | none |
| Live pointing | `viewer:mark` highlights and `PointerBubble` (`stores/ws.ts:272-364`, `PointerBubble.vue`) | — | none (they come from the MCP; D1 stops them at the source) |

**The frontend never calls a provider.** A grep for provider hosts, key headers and absolute-URL
`fetch` / `WebSocket` / `EventSource` across `frontend/src` finds only comments, test fixtures and one
docs link (`utils/mcpConnections.ts:79`). The Vite proxy forwards only `/api` and `/ws` to loopback.

## 2. Seam analysis: every spawn of the `claude` binary

Grep: `_agent_spawn_cmd(` across `app/src api/src` (the one function that turns an argv into an OS
spawn of the agent CLI), `run(pipeline(` in `app/src/ai/`, and `resolve_claude_bin|agent_bin|["claude"`
across all `*.py` outside `.pixi`/tests.

**Julia: three spawn sites, one of them a model turn.**

| # | Site | What it runs | Model turn? | Reached from |
|---|---|---|---|---|
| S1 | `app/src/ai/agent_runner.jl:889` `_run_agent_once` | `claude -p … --mcp-config … --allowedTools …` | **yes** | only `run_agent_turn` (`:955`) ← only `run_kiwi_turn` (`api/src/kiwi_turn.jl:558`) ← `kiwi_start_turn` (`api/src/kiwi_api.jl:134`) ← `POST /api/kiwi/turn` and `POST /api/claim/review` (`api/src/claim_review.jl:62`) |
| S2 | `agent_runner.jl:765` `_run_claude_cli` | short CLI commands: `auth status`, `auth logout` (`claude_auth.jl:73,91`), `plugin marketplace add/remove`, `plugin install/uninstall` (`observer_plugin.jl:125-135,155`), `mcp remove -s local` (`agent_runner.jl:788`) | no | profile status/logout routes, shared-workstation logout on pick/quit (`kiwi_profile_api.jl:142,172`), `POST /api/observer/register` |
| S3 | `app/src/ai/claude_auth.jl:267` | `claude auth login` (browser OAuth, pasted-code fallback) | no | `POST /api/kiwi/profiles/login` |

So **in-app model turns already have a single seam: `run_agent_turn`** — every turn the app starts
passes through it, including the claim review. A gate there (or in `kiwi_start_turn`, one level up,
which also owns the 409/busy reply) covers all in-app model use.

**Paths that start a model outside that seam** (none of them is a spawn by the app's own process):

| # | Path | How it starts a model | Can the app gate it? |
|---|---|---|---|
| B1 | `pixi run chat` (`chat.py:134`) | `subprocess.Popen(argv)` with the argv **the server returns** from `GET /api/observer/chat` | yes — refuse that route; `chat.py` then has no argv to start |
| B2 | Terminal `claude` with the Cecelia plugin / `observer-mcp.json` | the user starts it; the plugin (`observer_plugin.jl`) or `claude --mcp-config` loads the observer | not the model — but every tool call is a request to the backend (D1) |
| B3 | Kiwi "Start / Continue in terminal" | `GET /api/kiwi/terminal/command` hands the user a `claude` one-liner with the profile's `CLAUDE_CONFIG_DIR` (`kiwi_profile_api.jl:343`) | yes — refuse the route (and hide the button) |
| B4 | Push to a paired session | `push_capture_notification` (`api/src/push_writer.jl:69`) writes a capture notice into a paired Claude Code session's socket, from `captures_api.jl:311` | yes — skip the push; **this is app→model egress that carries no `X-Cecelia-Client` header, so D1's request check does not see it** |
| B5 | Dev tooling | `python/cecelia/effectiveness/recital.py:298` (`claude -p`), `scripts/judge/*.py`, `scripts/agent_eval/run_overnight.py:270` via `resolve_claude_bin` | out of scope (developer tools, not the app); recorded |

No frontend code calls a model provider directly (see §1, frontend sweep).

**D1 (backend refusal) — where the check must sit, and one hole.**

- `handle_http` (`api/src/server.jl:222`) is where `X-Cecelia-Client` is read today, but binary routes
  (`try_serve_*`) and the WebSocket upgrade never reach it. The API-token check in the stream handler
  (`server.jl:864`) is the one place every request passes; the D1 refusal belongs beside it.
- **Hole:** the autonomous server's WebSocket (`mcp/cecelia_mcp/autonomous.py:283`, `connect(…,
  additional_headers=auth_headers())`) does **not** send `X-Cecelia-Client: claude`, so a `run_task` /
  `run_chain` over WS would pass a header-based check. Every HTTP call from both MCP servers does send
  it (`mcp/cecelia_mcp/client.py:242`; `autonomous.py` imports `http_json` from there). Fix: add the
  header to that `connect`, plus a test that every MCP-side request path sends it.

## 3. Data egress and credentials

**What leaves the machine on a model turn (S1):** the prompt, the system prompt
(`--append-system-prompt` / `--system-prompt`), and every tool result the model asks for, sent by the
`claude` CLI to the account's provider. Kiwi's tool list is `KIWI_READ_TOOLS`
(`api/src/kiwi_turn.jl:24`): project/image metadata, notes, attributes, populations and measure
summaries, QC, task logs and params, chains, boards, blackboard, lab log, notebooks, guides — and
**pixels**: `gate_cells_view`, `gate_examples` (crops), `get_capture` /
`get_capture_landscape_tiles` return PNG images (`mcp/cecelia_mcp/server.py:2311`). The claim review
uses an allow-list of record tools (`CLAIM_REVIEW_TOOLS`, `api/src/claim_review.jl`); the rest are withheld with a reason (`CLAIM_REVIEW_WITHHELD`).
For B1–B3 the user's own session can call any observer tool, so the egress is "anything the observer
tools return", decided by the model and the user, not by Cecelia.

**Credentials.** The app never holds an API key. Every spawn runs under the active profile's
`CLAUDE_CONFIG_DIR` (`agent_runner.jl:498-530`): `<config_dir>/user-profiles/<name>/` for a named
profile, the CLI's own `~/.claude*` for `default`. Ambient `ANTHROPIC_API_KEY`, `CLAUDE_CODE_OAUTH_TOKEN`,
Bedrock/Vertex/Foundry switches are scrubbed from the child env (`_AMBIENT_CLAUDE_ENV`, `:442`).
Credentials are **read** (by the CLI) on S1, S2 `auth status`, S3; **written/removed** by S3 and
`auth logout`.

**At startup with AI unused:** no spawn found at server start (grep of `server.jl` for
`claude|kiwi|observer|purge|plugin` outside route tables: includes only). `GET /api/observer/status`
writes `observer-mcp.json` into the config dir as a side effect (`observer_api.jl:114`) — a file, no
egress. Shared-workstation logout/purge spawns `claude auth logout` on profile pick and on quit when
those items are on (`kiwi_profile_api.jl:135-215`). Whether `auth logout` contacts the provider is
**unverified**.

## 4. UI coupling

- **No feature-flag system** (a grep for `featureFlag|FEATURES|isEnabled(|VITE_` finds nothing relevant).
- **The closest existing gate is `observer.available`** (`stores/observer.ts`, from
  `GET /api/observer/status`). It means "the Claude CLI is installed", not "AI is allowed", and it
  gates only the terminal controls and the model picker.
- **`guidesShowClaude` ("AI help")** is per profile and off by default, with the comment "everything
  in Cecelia works without Claude" (`stores/settings.ts:388`). It is the precedent for a user-level
  switch, but it lives in `localStorage`, so the backend can't see it. The enforcing switch has to live
  server-side (§8.2), and the frontend reads it.
- **Maturity** (`utils/maturity.ts`, `maturityRecords.ts`) is display only. `assist-with-kiwi` and
  `proposed-gate` are Experimental. `lib/severity.ts` is the QC palette and has no bearing here.
- **The existing per-profile AI settings:** `kiwiOpen`, `kiwiReasoning`, `kiwiModel`,
  `captureAttachToKiwi`, `captureSendToPaired`, `purgeClaudeConversations` (`stores/settings.ts:43-48`,
  `:344-390`).
- **Background traffic with Kiwi closed:** `GET /api/kiwi/turns` and `GET /api/observer/status` on
  every project change (`stores/kiwi.ts:91`, `App.vue:80`). Neither reaches a model.

## 5. Stored AI content

Binding is central (`handle_http` binds `REQUEST_VIA` and `REQUEST_CALLER`), but recording is per
route: only handlers that call `author_stamp()` (`app/src/config/profile_settings.jl:268`) store
`{profile, via, model?, session?}`.

| Content | On disk | Mark | Gap |
|---|---|---|---|
| Blackboard entries, sections, verdicts | `<proj>/blackboard/<id>/{entry.md,meta.json}` | `createdBy` / `updatedBy` / `sectionOutcomes[].by` = stamp (`blackboard_api.jl:366`, `blackboard_run_review.jl:175`) | legacy entries unstamped |
| Claim Review sections | inside the claim entry | `meta.claim.review.{model,turnId}` (`claim_review.jl:81`) | **inferred:** the entry's `updatedBy` carries the via of whoever POSTed the review, not the model (the ScopedValue is inherited by the spawned turn) |
| Lab log | `<proj>/lab-log.md` | free-text `[<author>]` tag from the request body (`routes/lab_log.jl:64`); model appended only for `Claude…` tags (`lab_log.jl:110`) | **no via, no session; tag not checked against the caller.** The MCP server also writes `[Claude]` blocks with no model involved (`server.py` `_flush_to_lab_log`) |
| Kiwi turns | `<proj>/kiwi/turns.json`, last 50 (`kiwi_api.jl:21`) | model output by definition; `model`, `profile`, `reply.sessionId` | — |
| Kiwi turn saved to Blackboard | Blackboard entry | heading `# Kiwi turn — …` (`kiwiTurnSave.ts:146`) | **stamped `via: app`, with no model or turnId** |
| Figures, notebooks, chains, boards | `<proj>/figures`, `notebooks`, `settings/chains`, `settings/<boards>` | stamp (`figures_api.jl:61`, `notebooks_api.jl:529`, `routes/chain.jl:205`, `routes/project.jl:229`) | — |
| Gates | image `gating/{vn}.json` | `createdBy` / `updatedBy`; `claude_made` (`gating_api.jl:1351`) | — |
| ROI labels | `<img>/roiReviews/{suffix}.json` | `claudeLabel.by` = stamp (`rois.jl:196`) | the field isn't restricted to `via: claude` |
| LabArchives context | `<proj>/settings/labarchives.json` | `updatedBy` + free-text `syncedBy` | — |
| Projects, sets and images created by the autonomous server; WS task and chain runs | various | profile only (`sockets.jl:691,812`: "No `via`") | **unstamped** |
| Claude transcripts | profile `CLAUDE_CONFIG_DIR` (`~/.claude` for `default`) | — | **inferred** kept (no persistence-off flag in `_build_claude_cmd`); purged only by policy or opt-in, never for `default` |
| Pairing record | `<proj>/settings/push_target.json`, **including the socket token** | — | ~~travels in `.ccbundle`~~ skipped by the export since 2026-11-08 (`_SKIP_FILES`, `project_io.jl`) |

## 6. Tests that exist

- **Fake engines, no spawn:** `api/test/suite/kiwi_turn.jl` (`_KiwiFakeEngine`, `_KIWI_AGENT` swap)
  pins persistence, follow-up resume, the ownership 409, one turn per project, cancel, model coercion,
  **missing CLI → 503 before anything runs** (`:338`), and that every observer tool is in exactly one of
  `KIWI_READ_TOOLS` / `KIWI_EXCLUDED_TOOLS` (`:55`). `api/test/suite/claim_review.jl` pins that the
  withheld tools never reach the engine, and the write-back.
- **Pairing off:** `app/test/suite/observer.jl:105-123` (`CECELIA_OBSERVER_NO_PAIR` only on the headless
  and chat configs); `mcp/tests/test_client.py:864-887` (no auto-pair under NO_PAIR or no-GUI).
- **Write sets pinned:** `test_client.py:66`, `test_autonomous.py:104,271`.
- **Provenance:** `api/test/suite/authorship.jl` (stamps through `handle_http`, with and without the header).
- **Registry-shaped test:** `frontend/src/lib/claudeTools.test.ts` parses `server.py`,
  `gating_views.py` and `KIWI_READ_TOOLS`, and fails when a tool is not described.
- **None asserts that a model path stays off,** because no such mode exists. The 503 test is the
  template: same place, same fake, a different reason.

## 7. Users, installs, shared machines

- **No accounts.** "Users" are Cecelia profiles: `[ai].profile` in `custom.toml`
  (`agent_runner.jl:459`), project ownership in `project.json` owners. Anyone can pick any profile;
  `shared_workstation.jl:4` calls the `default`-only Settings section "a visibility gate, not security".
- **Config is per OS user, always.** `config_dir()` → `~/.cecelia` (or `CECELIA_DEV_DIR`). A system
  install (`CECELIA_INSTALL_SCOPE=system`, `docs/INSTALL.md` → *Shared / lab machines*) shares only the
  runtime in `/opt/cecelia`; "config + projects stay per-user". `init_cecelia!` deep-merges the bundled
  `app/config.toml` with the user's `custom.toml` (`app/src/config.jl:108`), so **every key is
  user-overridable today — there is no admin-owned layer.** The bundled file in a system install is
  admin-owned and read-only to other accounts, which makes a sibling, *non-merged* policy file in
  the install dir one possible home for an admin ceiling. **Not taken:** the ceiling is a
  `[sharedWorkstation]` item held by peanut (§8.2).
- **Two different "shared" setups exist and the code serves only one.** `[sharedWorkstation]` targets
  *one OS login used by several people* (profiles share `~/.cecelia`). A system install targets
  *several OS accounts on one box*; there each person already has their own `~/.cecelia`, credentials
  and `[sharedWorkstation]` table, and no admin can set anything for all of them.
- **Credentials belong to the profile** (`user-profiles/<name>/`), except `default`, which shares the
  terminal's `~/.claude` (plan D21).
- **Projects move** between users/machines via `.ccbundle`, which carries `<proj>/` including the
  blackboard, lab log and `settings/push_target.json` (pairing record; see §5 for stored content).

## 8. Design (proposal, not built)

### 8.1 States

**Recommend two states, off and on, for now.** "On request" is what on already means here: no turn
starts without a click (summary item 1). The brief's "on request" state matters only if something ever
runs a model unprompted. Nothing does today; add the state when something is planned to. *Open
question 2.*

### 8.2 Where the setting lives

| Layer | Proposal | Storage | Notes |
|---|---|---|---|
| **Ceiling (machine)** | `off` / `allowed` | **decided (2026-11-07):** a new item in `[sharedWorkstation]` (`custom.toml`), set in Settings → *Shared workstation*, which only peanut (`default`, `appControl.canManageMachine`) sees | peanut is the workstation's admin profile. Like the rest of that section this is a visibility gate, not security: it matches plan D1 (a policy for the people using the box, not a defence against them). It is a single item, not a switch over the other items, so the section's "no master switch" rule stands |
| **User** | `off` / `on` within the ceiling | **decided:** per profile, server-side (`profile_settings.jl`), set in Preferences; **not** `localStorage` | a profile, not an OS account: Cecelia has no accounts |
| **Project** | optional `off` | `<proj>/settings/` (travels with the bundle) | later. The useful case is "this dataset never leaves the machine", travelling with the data |

The effective state is the strictest of the three, computed on the server by one function
(`ai_enabled(project_uid)`). `GET /api/observer/status` returns it along with **who set it**
(`{enabled, setBy: "workstation"|"profile"|"project"}`), so the UI can say "AI is turned off for this workstation".

### 8.3 Default

**Off for every install.** The brief's reasons hold, and the audit adds one: pixels leave the machine
(§3), not only text. Every AI surface also already needs a separate Claude login, so someone who wants
AI goes through a setup step anyway, and the opt-in dialog can sit on that path. For the cost to
discoverability, see 8.5.

### 8.4 Opt-in dialog

Shown once when the user turns AI on. The `CLAUDE_DATA_NOTICE` line stays in Kiwi as the standing
reminder, and the dialog reuses its wording.

> **Turn on AI features?**
> They're off by default because they send data to a third-party service (Anthropic). Whether
> that's OK is up to you, your lab and your data policy.
> - **Sent:** your messages and, per feature, parts of your project: {registry `sends`, per surface}.
>   This can include **image crops and captures**, not only numbers and names.
> - **Gain:** {registry `gain`, per surface. Today: ask about your project (Kiwi), a second reading
>   of a claim (Review), Claude in your terminal}.
> - **Risks:** data leaves your machine. The AI can be wrong or give different answers each time, and
>   you stay responsible for the analysis. Use counts against your Claude account.
> - {`CLAUDE_DATA_NOTICE.detail`: "Check your institute's rules for unpublished or patient data"}
> - Anthropic's terms, as a link. **Link, don't paraphrase.**
>
> You can turn this off at any time, and on a shared workstation it can be turned off for everyone. Everything
> else in Cecelia works the same with AI off.
> [Turn on] [Cancel]

**Needs verification before shipping:** the per-surface `sends` lists (generated, but the generator's
input is written by hand per tool); the terms link for the account types users actually have; "counts
against your account" for every plan type. The "acquisition advice" the brief listed under Gain is
**removed**, because that feature doesn't exist (Milestone 1 of `ACQUISITION_ADVISOR_PLAN.md`).

### 8.5 Entry point when off

**Recommend keeping the Kiwi launcher as the single entry point.** When AI is off it opens a short
panel: "AI features are off", one line on why, and "Open AI settings". It never turns AI on itself;
consent stays in 8.4. If the workstation ceiling is off, the panel says "AI is turned off for this workstation"
and has no button. The panel offers "Hide this button", which removes the launcher (per profile,
reversible in Settings).

The alternative is a Settings entry plus a one-time first-run line, with no launcher. That nags less,
but leaves no way back for someone who dismissed the first-run line, and the Kiwi launcher is already
where people look. Both cost about the same to build.

### 8.6 Shared workstations: what exists and what the toggle changes

| Brief's requirement | Today | With the toggle |
|---|---|---|
| AI off by default on shared machines; enabling is an admin step | no AI switch; no admin layer | peanut's item in Settings → *Shared workstation* (decided) |
| Credentials belong to the person | yes for named profiles (`user-profiles/<name>/`); `default` shares `~/.claude` | unchanged |
| No carry-over between people | partial: `oneClaudeLogin`, `claudeLogoutOnQuit`, `purgeClaudeConversations`, all **off by default**. Kiwi turns are stored per **project**, so a shared project shows the previous person's turns | turning AI off purges nothing (deliberately: it is not a cleanup). The three items stay separate |
| Visible effective state, and who set it | none | `setBy` from 8.2 |
| Provenance mark on model content | mostly (§5), with the listed gaps | close the gaps in slice 5 |

### 8.7 Enforcement below the UI

With AI off, three things are refused, all server-side:

1. **Turns.** `kiwi_start_turn` returns 403 `{error: "AI is off", setBy}` before anything spawns. This
   covers Kiwi and Review, and sits beside the existing missing-CLI 503.
2. **Requests from Claude (D1).** Refuse when `X-Cecelia-Client: claude`, in the stream handler next to
   the API-token check (`api/src/server.jl:864`), **not** in `handle_http`, which binary routes and the
   WS upgrade skip. Add the header to the autonomous WS `connect` (`autonomous.py:283`), with a test
   that every MCP-side request path sends it.
3. **Routes that hand out a model path:** `GET /api/observer/chat`, `GET /api/kiwi/terminal/command`,
   `POST /api/observer/register`, `POST /api/kiwi/profiles/login`, the `auth status` spawn, and
   `push_capture_notification` (B4: outbound, so there is no request to catch).

**Still allowed with AI off:** logout and conversation purge (they remove credentials), and reading
stored AI content.

**Not a security boundary.** The token holder can leave the header off. That is acceptable, because
the switch is a policy for that person's own use (plan D1).

### 8.8 Registry

One data file, one row per surface:

```
id, label, kind (turn | handout | credential | mcp | display), layer (ui | api | mcp | process),
routes[], tools[] (observer tool names, or "*"), sends (user wording), gain (user wording),
ui[] (component ids), enforcedBy (turns | d1 | route-refusal | none), maturity
```

**Home:** extend `frontend/src/lib/claudeTools.ts`, which already holds `CAPABILITIES` /
`TOOL_GROUPS` in user wording plus a test that parses the tool registrations. Add the non-tool
surfaces (turn, handout, credential) as new rows. Generate the backend's refused-route list from the
same file, or pin both sides with a test that they agree. The hardening manifest (step 7) can then read
`kind` as its AI column. *Open question 8.*

**Test:** (a) every `_agent_spawn_cmd` caller, every route in 8.7.3 and every observer tool appears in
the registry; (b) with AI off, each row's `enforcedBy` path refuses. (a) fails when a new surface
appears unregistered, which is the ratchet the brief asked for.

### 8.9 Degradation rules (no dead ends)

- **Kiwi:** the launcher becomes the 8.5 panel. "Add to Kiwi", "Attach to Kiwi" and "Send to paired
  session" are hidden.
- **Claim entry:** Findings (S0–S2) stay. The Review section is hidden, and the "Not checked" line
  says "review: AI off". Hiding it needs a structured marker on the section, not a text match on
  `· Review ·`.
- **Blackboard:** Claude-written entries and sections **stay, with their ✦ mark**. "Claude proposes"
  verdicts stay visible but read-only. Agent-run records stay behind their filter.
- **Lab log:** `[Claude]` entries stay, marked. The launcher badge stops announcing "Claude noted".
- **Gates, figures, notebooks, chains, ROI Claude labels:** stay, marked. They are data or proposals
  the user can act on.
- **Guides:** "AI help" is forced off and its toggle hidden. The "Assist with Kiwi" guide and the
  Claude tip are hidden.
- **Profiles:** stay. The "Kiwi's login" wording and the MCP registration on create are skipped.
- **Settings → Shared workstation:** stays, because it is cleanup and works either way.

**Stored content, recommendation: show it, marked; never hide it.** Hiding it would make a shared
project look different to two people, and would hide why an analysis looks the way it does. The mark
already exists for most content. The gaps in §5 (lab log via, saved Kiwi turns, the review write-back,
autonomous creations) are slice 5.

## 9. Slice plan

| # | Slice | Size | Risk | Test | Depends on |
|---|---|---|---|---|---|
| 1 | This audit | docs | — | — | — |
| 2 | **Turn gate + D1 + route refusals.** One `ai_enabled()` (a single user-level key for now), checked in `kiwi_start_turn`, the stream handler and the 8.7.3 routes; header added to the autonomous WS | S–M (~6 files, Julia + one Python line) | low: off = refuse, and the default stays **on** in this slice, so nothing changes for users | fake-engine test: off → 403, nothing spawned; stream-handler test with and without the header, covering a binary route and a WS upgrade; MCP test that every request path sends the header | nothing. **Can start now** |
| 3 | Registry (extend `claudeTools.ts`) + "every surface registered" test | M | low | 8.8 (a) | open question 8 |
| 4 | Setting UI + default off + opt-in dialog + 8.5 entry panel + `setBy` | M | **medium**: flips the default for every user, and the dialog wording needs your eyes | frontend logic tests; dialog `sends` generated from slice 3 | 3; open questions 1, 3, 4, 5 |
| 5 | Provenance gaps (lab log via, saved Kiwi turns, review write-back, WS and autonomous creations) + a structured Review marker — **built 2026-11-08**: the lab log's tag must match the caller (403); Kiwi writes are stamped `via: kiwi` + model + session (the review write-back; a saved turn adds `turnId`); API-created projects, sets and images carry `meta.createdBy`. WS runs were already stamped on main (`caller_stamp` on task and chain requests) | M | low | `authorship.jl` cases | none (can run alongside 2) |
| 6 | Hide surfaces one area at a time, per 8.9: Kiwi and hand-offs first, then Guides, then marks | S each | low | per-area frontend tests | 2, 4 |
| 7 | Workstation ceiling: a `[sharedWorkstation]` item, peanut-only | S | low: one item on each side of `SHARED_WORKSTATION_ITEMS` | `ai_enabled` test: a profile on can't exceed the ceiling | decided |
| 8 | User docs: what AI does, what it sends, what stays on | S | — | `userDocs.test.ts` | 3 |

**The seam answer sets the cost.** In-app turns have one seam, so slice 2 is small. Every other path is
one route, plus one header fix and the push writer. Nothing needs consolidating first.

## 10. Open questions for the maintainer

1. ~~Admin ceiling~~: **decided (2026-11-07).** peanut holds the master switch, as an item in Settings
   → *Shared workstation*; it only matters on workstations.
2. **Is an "on request" state needed?** Recommendation: no, not until something runs unprompted (8.1).
3. ~~User setting~~: **decided:** per profile, in Preferences. Still open: is a project-level "off"
   wanted (it would travel in `.ccbundle`)?
4. **Default off for single-user installs too?** Recommendation: yes (8.3).
5. **Entry point when off:** the Kiwi launcher with "Hide this button" (recommended), or Settings only?
6. **Stored AI content:** shown and marked (recommended), or hidden?
7. ~~MCP observer~~: decided, plan D1.
8. **Registry home:** extend `claudeTools.ts` (recommended), or a separate file?
9. **Is a local model planned?** If so, the switch splits into "a model runs" and "data leaves".
10. ~~**Should a shared project show the previous person's Kiwi turns?**~~ **Decided (2026-11-08):**
    yes. The turns stay with the project as the record behind saved claims. The pairing token no
    longer travels in a `.ccbundle` (`_SKIP_FILES`, `app/src/project_io.jl`).
11. **New: should AI off also stop the MCP server's own `[Claude]` lab-log observations?** They are
    written with no model involved. D1 refuses the write anyway; confirm that is what you want.

## 11. Assumptions and unverified points

- **Inferred:** the claim-review write-back is stamped with the via of the review's caller (ScopedValue
  inheritance into `Threads.@spawn`); Kiwi transcripts persist in the profile dir (no persistence-off
  flag found). Check both when slice 5 starts.
- **Unverified:** whether `claude auth logout` contacts the provider; the account-type terms the dialog
  links to; whether the MCP server's `[Claude]` observations ever carry model output.
- **Not checked:** how the GUI writes image notes; LabArchives' own MCP (on the claude.ai side, not
  Cecelia's).
- **Stale comments found (not fixed here):** "Watch" in `agent_runner.jl:43,797,944` and
  `observer_api.jl:29`; `/api/observer/feedback` in `observer_api.jl:12` is not a route.
