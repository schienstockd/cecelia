# Observer-in-app integration — parked plan

Bring the MCP observer (Slices A–C, see `docs/ai-assist/OBSERVER.md`) *inside* Cecelia: the user runs
the AI assistant from the app — a toggle to have it watch and comment in the lab log automatically, a
button to ask for feedback on demand — instead of wiring a separate terminal Claude session. The lab
log is the medium; the assistant's output is `[Claude]` entries, not a chat window.

Status: **not built.** Phase 1 (observe/read/append + the 10-attempts pattern + throttle) is done and
validated live; this plan is the in-app *driver* on top of it.

---

## Why (the payoff)

The observer arc's real value is a self-filling **methodology record** — the assistant notices patterns
and records the *why*, so it survives into figures/papers. Today that only happens if a human runs a
terminal Claude session and prompts it. Integrating it into the app makes it a one-click,
always-available feature that most users will actually use.

---

## Locked decisions

1. **The app spawns a headless assistant; it does not reimplement an agent loop.** The backend runs
   `claude -p "<prompt>" --output-format json --mcp-config <cfg> --allowedTools <observer tools>`
   (Claude Code print mode) as a subprocess — the same shape as the napari bridge spawn
   (`app/src/napari.jl` `_bridge_cmd` / `python_bin_path()` / `Base.Process`). The assistant uses the
   MCP tools it already has to read state + append to the lab log, then exits. We reuse: the MCP
   server (unchanged), `run_py`-style process management, `append_lab_log`, and `broadcast_ws` for
   notifications.

2. **The MCP server stays model-agnostic; the runner is a swappable adapter.** MCP is an open protocol
   (Claude, OpenAI Agents SDK/ChatGPT, Gemini CLI, … all consume it). Only the *spawn command + auth*
   is assistant-specific. So the agent-runner is an interface — `AgentBackend` — with **Claude as the
   first implementation**; adding Gemini/ChatGPT later is a new adapter, not a rewrite. The observer
   *prompt* lives server-side (shared across backends). This is an explicit product requirement (user:
   "it would be nice if people could choose other agents in the future").
   - **Model is user-selectable, default Sonnet.** The observer's work (spot a repeat pattern, read a
     task log to diagnose a failure, write a brief lab-log line) fits Sonnet; Haiku is enough for the
     frequent auto-Watch passes; **Opus is overkill** (slower, costlier). The panel exposes a
     Haiku/Sonnet/Opus picker (`settings.labLogObserverModel`), sent per run (Ask-Claude + Watch) and
     allow-listed server-side (`OBSERVER_MODELS`); the default is overridable via config.toml
     `[ai] model`. A second backend would define its own list.

3. **The control surface is exactly four things** (user: "that's the only two readouts we need" +
   two actions):
   - **Toggle — "sit next to me"** (auto-observe; default **off**). While on, the backend runs an
     observer turn on a heartbeat, gated on new activity.
   - **Button — "give feedback on what I did"** (one-shot, on demand; not continuous).
   - **Readout — token use** (REAL usage, accumulated from each run's `--output-format json` result —
     supersedes Slice C's estimate). Per project + session.
   - **Button — clear context** (reset the assistant's accumulated session; see Decision 4).
   - (+ a **"Chat about this"** button that hands off to the real Claude Code console — see Decision 9;
     it's a launch/handoff, not part of the in-app readouts.)

4. **Context continuity via a per-project assistant session; "clear context" starts a fresh one.**
   Each project keeps an assistant session id so the observer remembers within a work session (better
   "you've been at this a while" continuity than N stateless turns). Runs use `--resume <sid>` (or
   `--continue`); **clear context** drops the stored sid so the next run forks a new session
   (`--fork-session`) — and resets that session's token counter. Sid + token totals persist in a
   sidecar (`settings/observer-session.json`), like the lab-log mutes/tuning sidecars.

5. **The lab log is the conversation surface — no separate chat.** The assistant writes short
   `[Claude]` observations, and when it can't explain a choice, writes a `[Claude]` *question*; the
   user answers with a `[User]` entry / reaction (the panel already supports both — see the reactions
   feature). Q&A in the log = the methodology record.

6. **Notify on write — a glanceable badge, NOT a toast.** People won't keep the panel open, so a
   `[Claude]` append broadcasts a WS frame → a **bell / Claude icon badge on the lab-log toggle**
   (persistent, so you can notice it whenever), **plus an abbreviated one-line preview of the addition
   under the toggle** (the gist without opening the panel). The badge clears when the panel is opened.
   No transient toast (Cecelia has none; a badge is lighter and doesn't vanish). Same WS→badge
   mechanism as the update badge. (User: "a bell or claude icon next to lab log, and an abbreviated
   version of the addition underneath the lab log toggle.")

7. **Signal discipline is enforced by the server-side prompt**, not left to a freeform session: one
   line per event, imperative, numbers in the detail; only write on a >3-attempt pattern / real error
   / genuine anomaly; never re-note the same stuck task (the monitor already coalesces); respect the
   throttle + `set_observer_active`.

8. **Shipped, it's an opt-in feature gated on availability — shown DISABLED, not hidden.** It requires
   an assistant CLI installed + authenticated (dev: `claude` at `~/.local/bin`). When none is
   available, the controls render **greyed-out with an explainer** ("Needs Claude Code — with it, an
   assistant can watch your analysis and note things in the lab log") + a link on how to set it up.
   Rationale (user): hiding it means nobody discovers the capability; a disabled-with-why state teaches
   people it exists and that it *would* be useful. `GET /api/observer/status`'s `available` flag drives
   the disabled state + the hint (not removal).

9. **NEVER build an in-app chat console.** The app does only *autonomous/one-shot* work (the toggle +
   the feedback button → headless `claude -p` → lab log). When the user wants a real back-and-forth
   ("chat about this"), we **hand off to the actual Claude Code console** — a "Chat about this" button
   that launches interactive `claude` with the observer MCP + a seeded starter prompt referencing the
   current project/image. Rationale (user): building an in-app chat means rebuilding + maintaining a
   Claude Code console, which we explicitly won't own. Handoff mechanism, cross-platform-safe:
   best-effort launch in the OS terminal (`x-terminal-emulator -e` / `open -a Terminal` / `wt`), with
   a **copy-the-command fallback** always shown (zero-spawn, works everywhere, nothing to maintain).
   This keeps the two modes cleanly separated: app owns *observation → lab log*; Claude Code owns
   *conversation*.

---

## Architecture (cross-file)

**Backend (`app/` + `api/`)**
- `app/src/ai/agent_runner.jl` (**new**) — the `AgentBackend` interface + `ClaudeAgent` impl. Builds
  the command (mirrors `_bridge_cmd`), spawns via `Base.Process` (register for cancel like tasks),
  captures the `--output-format json` result (assistant text + `usage` tokens + `session_id`),
  bounded by a timeout. Model-agnostic entry: `run_observer_turn(project, prompt; session, mode)`.
- `app/src/ai/observer_prompt.jl` (**new**) — the canonical observer prompt text (Decision 7), one
  place, passed via `--append-system-prompt`. Feedback-mode vs auto-mode variants.
- `app/src/ai/observer_session.jl` (**new**) — the per-project session sidecar
  (`settings/observer-session.json`): session id + cumulative token/cost totals; read/clear/add.
- MCP config for the spawn: a small generated JSON pointing at the same `cecelia_mcp.server` (reuse
  `mcp/`), `--allowedTools mcp__cecelia-observer__*` (read tools + append only — the MCP allow-list is
  still the hard guarantee).
- `api/src/observer_api.jl` (**new**, wired into `server.jl`): `POST /api/observer/feedback`
  (one-shot), `POST /api/observer/auto` ({enabled}), `GET /api/observer/status` (enabled, tokens,
  session, availability), `POST /api/observer/clear`. Auto-mode heartbeat: a backend async loop that,
  while enabled, every N min checks the run-log delta (reuse the `capture_context!` "new activity"
  gate) and runs a turn only if something changed.
- On a `[Claude]` append (already flows through `api_lablog_append`), broadcast an `observer:wrote`
  WS frame (extend the existing `lab_log_entry_added` path — it currently fires for user entries only).

**Frontend (`frontend/`)**
- `LabLogPanel.vue` — add the control strip: [🟢 sit next to me] toggle · [Ask Claude] button ·
  token readout · [clear context]. Also a settings entry point (SettingsModule) mirroring the toggle.
- `stores/observer.ts` (**new**) + `utils/serviceApi.ts` — `observerApi` {feedback, setAuto, status,
  clear}; poll/subscribe status.
- WS handler (`stores/ws.ts`) — on `observer:wrote`: toast + lab-log badge + refresh the panel.
- Availability gate: hide controls when `GET /api/observer/status` reports the assistant CLI absent.

**Reused as-is:** `mcp/` (the server), `append_lab_log!` + the panel + reactions, `broadcast_ws`,
the `capture_context!` new-activity gate, the task-cancel/process registry, `python_bin_path`-style
executable resolution.

---

## Build order (phases — each its own PR)

1. **One-shot feedback, end-to-end (the proof).** `agent_runner.jl` (Claude impl) +
   `observer_prompt.jl` + `POST /api/observer/feedback` + a minimal [Ask Claude] button in
   `LabLogPanel` + toast. No heartbeat, no session, no token readout yet — one click → one turn →
   a `[Claude]` entry appears. Proves the whole spawn→MCP→append→notify loop with a hard cost ceiling.
2. **Token readout + session continuity + clear-context.** `observer_session.jl` sidecar; parse
   `usage` from the JSON result; `--resume`; `GET /api/observer/status` + `POST /api/observer/clear`;
   the token readout + clear button in the UI.
3. ✅ **DONE — Auto "sit next to me".** Implemented **frontend-driven** (not a backend heartbeat): the
   panel subscribes to task-completion WS frames (`task:status`/`chain:node:*` terminal, via
   `utils/observerAuto.ts` `isObserverTrigger`) and, while the `labLogObserverAuto` toggle is on + an
   assistant is available, fires a **debounced** (8s) observer pass (reusing the feedback path). Chosen
   over a server loop because it's simpler, spends nothing when the app is closed, and is naturally
   scoped to "while you're working." Default off. **Badge:** when the observer appends a `[Claude]`
   entry while the lab-log panel is closed, `settings.labLogUnseen` (a one-line preview) lights a
   sparkles badge on the sidebar lab-log toggle (`AppSidebar.vue`), cleared on open — the glanceable
   notification (no toast, per Decision 6). (A backend heartbeat for closed-app/overnight watching is
   Phase 3 of the *arc* — the Analyst — not this integration.)
4. **Availability gating + polish + the AgentBackend seam made real** (document how a second backend
   would slot in; hide UI when unavailable).

Cohort QC / `qc_flag_fired` remain out of scope (blocked on QC work).

---

## Reservations / open questions

- **Assistant availability & auth for shipped builds** — dev works (`claude` present + authed).
  Shipping needs: detect the CLI, surface "connect your assistant" onboarding, hide if absent. Decide
  before Phase 4.
- **Cost of auto mode** — mitigated by the new-activity gate + throttle + explicit off-default toggle
  + notify-on-write + the real token readout. Still real; the readout is the safeguard.
- **Exact `claude -p` result schema** (`usage` fields, `session_id` key) — confirm against a real
  `--output-format json` run in Phase 1/2; treat as adapter detail.
- **Concurrency** — one observer turn per project at a time (a run in flight blocks another); the
  heartbeat must not stack turns. Guard in `observer_session.jl`.
- **Prompt-injection surface** — the assistant reads task logs / user notes, which are user-authored;
  it can only append to the lab log (MCP allow-list), so the blast radius is bounded to lab-log noise,
  not mutation. Keep the allow-list as the guarantee; never widen it for this feature.
