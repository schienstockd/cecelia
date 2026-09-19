# Kiwi (assist cockpit) — plan

**Status:** planning (2026-09-19). Written to be picked up cold after a context break.

## Goal

A dedicated always-visible surface for AI-assistant-adjacent controls, so "using Claude (or
any future MCP assistant) with Cecelia" doesn't require an open image or a scavenger hunt
across three unrelated panels. Same shape as `CorrectionCockpit` (a floating panel gated by
a settings toggle, mounted from `App.vue`) — hence the "cockpit" name.

**Codename Kiwi** (locked 2026-09-19). Fruit lineage from Feijoa. Kiwi is the *panel*, not
the assistant; the panel's job is to display + control whatever assistant is paired, not to
be one. All identifiers (`stores/kiwi.ts`, `components/kiwi/`, `--cc-kiwi` accent token,
`Ctrl+Shift+K` shortcut) take the codename; the assistant itself is referred to in neutral
terms ("the paired assistant", "your assistant session").

**Provider-agnostic on purpose.** Claude is primary today, but the MCP protocol the observer
speaks is a standard, and other assistants could pair against Cecelia in the future. Every
label, guidance string, and code identifier that isn't a specific-Claude-Code technical
reference (the `cecelia-observer` MCP server name, `CLAUDE_CODE_MESSAGING_*` env vars) uses
provider-neutral wording ("the paired assistant", "the assistant session", "chat with your
assistant").

## Why gather (not scatter more)

AI-adjacent state and controls already live in three surfaces that don't know about each
other, forcing the user to know where to look:

| Surface | What it holds today | Why it's the wrong place |
|---|---|---|
| `ViewerPanel.vue` push chip (`#1051`) | pairing state, socket tooltip | requires an open image; pairing is project-scoped |
| `LabLogPanel.vue` "Chat to Claude" button + `buildChatPrompt` machinery | copies a starter prompt to the clipboard for a terminal session | chat-with-your-assistant is not a lab-log concern; the button is a scavenger-hunt away from the actual pairing state |
| Settings → MCP connections (`AppHeader.vue`) | observer available / auth failed, connect-CTA | modal, dismissed on click, not glanceable |

Adding push-related widgets (recent captures, session identity, throttle state, "send this
to my assistant" quick actions) to any of them worsens the scatter. The cockpit is one
place where "what's my assistant doing / what can I make it do" is a single glance.

## Cross-cutting constraints (non-negotiable)

- **Provider-agnostic language.** UI labels never say "Claude" unless the specific Claude Code
  install is being addressed (setup CTA, version check). The store, component, and file names
  use "assistant" / "MCP" / neutral terms.
- **Additive-first.** Nothing existing gets removed until it's proven in the cockpit.
  `ViewerPanel.vue`'s push chip stays through v1; the cockpit mirrors it. `LabLogPanel.vue`
  keeps its Chat button through v1; v2 removes it once the cockpit's version is proven.
- **Same shape as `CorrectionCockpit`.** Floating panel component under
  `frontend/src/components/assist/` (mirror of `frontend/src/components/correction/`);
  mounted once from `App.vue`; visibility gated by a `settings.assistCockpitOpen` boolean;
  keyboard shortcut toggles the setting. NOT a new UI primitive — reuse the styling +
  positioning conventions the correction cockpit already established.
- **MCP-only for anything the assistant drives.** Same discipline as
  [`BIDIR_CONTEXT_PLAN.md`](BIDIR_CONTEXT_PLAN.md). The cockpit is a HUMAN-facing display +
  control surface for state that already exists; no new capability the assistant can call.
- **Reuse existing primitives** per [`docs/ui/PRIMITIVES.md`](../ui/PRIMITIVES.md).
  `CollapsibleSection` for each row-group, `cc-btn*` for buttons, `CcToggle` for the
  observer switch, `TeleportPopover` for anything that overflows.
- **Project-scoped.** Cockpit reads current project from `projectMeta.current`; if no project
  open, disabled state reads "open a project to pair" — no auto-select.
- **No new secrets on screen.** Tokens live on disk only. Socket path may show in a debug
  tooltip since it's not sensitive; the token never renders in the DOM.
- **Maintainability discipline** ([`docs/MAINTAINABILITY.md`](../MAINTAINABILITY.md)) applies
  from the outset — typed props, one canonical helper per job, comment/docstring rules.

## Locked decisions

Numbered so code / other docs can cite them (`Decision N`). Marked **OPEN** where user input
is needed before implementation.

1. **UI label = Kiwi** (locked 2026-09-19). Fruit name, matching the project's Feijoa
   codename lineage. Provider-agnostic — Kiwi is the *panel*, not the assistant; the
   distinction is important because the panel's job is to display + control whatever
   assistant is paired, not to be one. Store / component / file names take the `kiwi` /
   `Kiwi` prefix accordingly (`stores/kiwi.ts`, `components/kiwi/KiwiCockpit.vue`,
   `utils/kiwi.ts`).
2. **Primitive = `CorrectionCockpit`-shape floating panel.** Same file layout
   (`components/kiwi/KiwiCockpit.vue`), same mount point (`App.vue` at the same level as
   the correction cockpit), same visibility gating (`settings.kiwiOpen`), same
   drag-across-viewport + remembers-position behaviour. Reuse `CorrectionCockpit.vue`'s
   `<style scoped>` panel chrome patterns verbatim; do not invent a second floating-panel
   look. Colour: a new `--cc-kiwi` token to distinguish from correction cockpit's purple
   — see OPEN 4.
3. **Entry = sidebar footer button + keyboard shortcut.** Same footer band + button
   pattern the correction cockpit uses in `AppSidebar.vue`. Keyboard shortcut default
   `Ctrl+Shift+K` (K for "Kiwi" — grep confirms free in `frontend/src`). See OPEN 5.
4. **v1 content set** — the six rows below in *Contents — v1*.
5. **v2 candidate content** — the list below in *Contents — v2 candidates*; needs OPEN 2
   input before locking.
6. **Displaces at v1 (immediate migration, not deferred):** `LabLogPanel.vue`'s "Chat to
   your assistant" button + the `buildChatPrompt` call site move to the cockpit. Rationale
   (from the user 2026-09-19): the chat handoff is not a lab-log concern; the button is a
   scavenger hunt away from the pairing state it depends on. `buildChatPrompt` itself
   stays in `lib/chatHandoff.ts` (still a pure helper); only the invocation moves.
7. **Displaces at v2:** `ViewerPanel.vue`'s push chip → cockpit only. Kept in v1 so the
   cockpit's version is proven before removing the current-shipping surface.
8. **Does NOT displace:** `/analysis`, `/notebooks`, `/blackboard` (from
   BIDIR_CONTEXT_PLAN Part 4 if it ships), MCP config in Settings, Share button in
   ViewerPanel, the lab log ITSELF (only the chat button leaves — the log stays a log).
9. **Availability gate.** Cockpit is visible whenever a project is open. Push-related rows
   inside gate on `observerSetupReason(...) === null` — same rule the lab-log install band
   uses. When Claude Code isn't set up, the cockpit surfaces the setup CTA instead of
   pretending pairing might work.
10. **State ownership.** All cockpit state derives from existing stores — `pushStore` (WS
    dispatcher shipped in `#1051`), `projectMeta`, `useSettingsStore().observer*` — plus a
    new thin `stores/kiwi.ts` for view state (open/closed, position, minimize). No new
    backend endpoint; every read is a route that already exists.
11. **Test discipline.** Pure logic in `frontend/src/utils/kiwi*.ts`; no component-level
    tests per frontend rule. `utils/pushTarget.ts` is the template.
12. **Naming discipline enforcement.** A frontend test greps
    `frontend/src/components/kiwi/` for the literal word "Claude" and fails if it appears
    anywhere outside comments / `data-guide` attributes / the specific Claude Code setup
    CTA row. Provider-neutral wording ratchet, same shape as the existing zarr-access /
    h5ad ratchets. Kiwi is the panel, not the assistant.

## Contents — v1

Each row: what it shows / what it reads.

- **Pairing chip** (mirror of `ViewerPanel.vue` via `usePushStore` + `fetchPushTarget`).
  Reads: `GET /api/push/target`. WS: `push_target:changed`, `push:sent`. Same three-state
  render (`not paired` / `paired ✓ <label>` / transient `sent ✓`).
- **Explicit re-pair button.** Calls the `register_push_target` MCP tool via a small
  backend proxy (a POST that just triggers the same server-side write). Rarely needed —
  auto-pair handles the common case — but the button gives the user a visible "reset" for
  stuck states.
- **Chat handoff button** (moved from `LabLogPanel.vue`). One click copies the starter
  prompt to the clipboard for a new external assistant session. Wording stays
  provider-neutral: "Copy chat starter for your assistant". Uses the existing
  `buildChatPrompt(projectUid, projectName)` unchanged.
- **Recent captures.** Last 10 shared frames from
  `GET /api/viewer/captures?projectUid=…&limit=10`. Per-row: timestamp, address (image /
  t / z / surface), captureId. Clickable rows either (a) open the CaptureViewSurface at
  that capture, or (b) copy the captureId to clipboard for pasting into an assistant
  prompt. Read-only view; no delete.
- **Observer state.** `available`, `authenticated`, `throttled`, `surfacedCount /
  surfaceCap`, `enabled`. Reads from `/api/observer/status`. Toggle for
  `set_observer_active(bool)` mirrors the Settings toggle.
- **Session identity.** For the currently-paired session: `sessionLabel`, socket path
  (debug tooltip), `pairedFromPid`, `pairedAt`. Answers "which of my three assistant
  sessions did I pair with here?" without opening a terminal.

Approximate line count for v1: ~600 lines across the three PRs below.

## Contents — v2 candidates (need OPEN 2 to lock)

Prospective, in rough dependency order:

- **Latest assistant-authored lab-log entry** (one-line + "show more" that scrolls
  LabLogPanel into view). Consumes `lab_log_updated` WS; filters by author tag
  client-side. Read-only glance.
- **"Share what I'm looking at" quick action.** Duplicate the Share button here so a user
  driving a plot rather than an image can share plot/UI captures. Depends on
  BIDIR_CONTEXT_PLAN PR #6+.
- **Send message to another assistant session.** Wraps `SendMessage` for handing something
  off between sessions. Nontrivial — sessions are name-addressable; picking the target
  needs `/list-agents` output.
- **Blackboard entries** (if BIDIR_CONTEXT_PLAN Part 4 ships): quick list + create button.
- **MCP connection health.** Currently Settings → MCP; a badge here would surface
  auth-lost faster.
- **Setup CTA / re-authenticate.** Merge the current install band from `LabLogPanel.vue`
  into the cockpit when the assistant isn't set up. Would let v3 remove the install band
  entirely.
- **Chat prompt bar.** One-line input that sends the string as a message to the paired
  session (via the push channel — same socket writer, prepend `[cecelia][user]` so the
  assistant distinguishes it from a capture push). Would let the user prompt without
  alt-tabbing to the terminal. Speculative — needs OPEN 2.

## Open items — resolve before implementation

1. **v2 content set.** The whole reason the surface exists is that it grows. Give a rough
   list of the "other controls" beyond v1 so the plan can bound them; the v2-candidates
   list above is prospective, not confirmed.
2. **v1 displacement scope.** Decision 6 is explicit: chat button leaves LabLogPanel at
   v1 (per user 2026-09-19). Decision 7 defers the ViewerPanel push chip to v2 to prove
   the cockpit first. Confirm this split, or flip Decision 7 to also-v1 if you want the
   ViewerPanel chip gone immediately.
3. **Cockpit accent colour.** Correction cockpit uses `--cc-accent` (purple) as its
   floating-panel border. Kiwi needs its own hue so a user with both open can tell them
   apart at a glance. Propose kiwi green (`#93c47d`-ish, plays on the codename); needs a
   `--cc-kiwi` token allocation in `docs/ui/PRIMITIVES.md`.
4. **Keyboard shortcut.** `Ctrl+Shift+K` (Decision 3). Confirm or override — the
   correction cockpit's shortcut convention is worth checking against.

## PR sequence

Independently mergeable in this order. Each PR ships a working, tested slice.

1. **Cockpit primitive + chat handoff migration.** New
   `components/kiwi/KiwiCockpit.vue` (floating panel via `CorrectionCockpit`'s pattern);
   new `stores/kiwi.ts` (view state); new sidebar footer button in `AppSidebar.vue`;
   keyboard shortcut wired; **chat-to-assistant button moved from `LabLogPanel.vue` to the
   cockpit** (Decision 6); pairing chip mirrored inside via `usePushStore` +
   `fetchPushTarget`. ~350 lines. Ships the surface + the first displacement.
2. **Recent captures + observer state.** Two `CollapsibleSection` blocks. Reuses
   `/api/viewer/captures` and `/api/observer/status`. Row-click on captures: pick between
   open-surface vs copy-id — needs a small ADR at PR time. ~200 lines.
3. **Session identity + explicit re-pair button.** Session identity is a read-only info
   block; re-pair posts to the small backend proxy that calls `register_push_target`
   server-side (no MCP invocation from the frontend). ~150 lines.
4. **v2 controls per OPEN 2** — split into further PRs based on which items land.

Total v1 estimate: ~700 lines across three PRs. v2 sizing depends on the OPEN 2 answer.

## References

- Shipped source of state the cockpit consumes: [`BIDIR_PUSH_PLAN.md`](BIDIR_PUSH_PLAN.md)
  (all three PRs shipped 2026-09-19: `#1048` pairing, `#1049` writer, `#1051` chip + WS).
- Related design: [`BIDIR_CONTEXT_PLAN.md`](BIDIR_CONTEXT_PLAN.md) — Blackboard and
  point-out surfaces the cockpit may host in v2.
- Primitive precedent: `frontend/src/components/correction/CorrectionCockpit.vue` — same
  shape.
- UI primitives catalog: [`docs/ui/PRIMITIVES.md`](../ui/PRIMITIVES.md).
- Existing AI-adjacent surfaces the cockpit consolidates:
  - `frontend/src/components/ViewerPanel.vue` — push chip (v2 removal)
  - `frontend/src/components/LabLogPanel.vue` — chat-to-assistant button (v1 removal)
  - `frontend/src/components/AppHeader.vue` — Settings → MCP connections
  - `frontend/src/utils/observerSetup.ts` — availability + setup reason
  - `frontend/src/lib/chatHandoff.ts` — `buildChatPrompt` (kept, invocation moves)
- Existing stores the cockpit reads: `stores/push.ts` (shipped `#1051`),
  `stores/projectMeta.ts`, `stores/settings.ts` (observer state, cockpit-open toggle).
