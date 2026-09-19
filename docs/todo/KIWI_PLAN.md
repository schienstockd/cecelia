# Kiwi (assist cockpit) — plan

**Status:** planning (2026-09-19). Written to be picked up cold after a context break. All four
OPEN items resolved 2026-09-19 (see *Open items — resolved* below); implementation of PR #1 is
unblocked.

## Goal

A dedicated always-visible surface for AI-assistant-adjacent controls, so "using Claude (or
any future MCP assistant) with Cecelia" doesn't require an open image or a scavenger hunt
across three unrelated panels. Same shape as `CorrectionCockpit` (a floating panel gated by
a settings toggle, mounted from `App.vue`) — hence the "cockpit" name.

**Codename Kiwi** (locked 2026-09-19). Fruit lineage from Feijoa. Kiwi is the *panel*, not
the assistant; the panel's job is to display + control whatever assistant is paired, not to
be one. All identifiers (`stores/kiwi.ts`, `components/kiwi/`, `--cc-kiwi` accent token) take
the codename; the assistant itself is referred to in neutral terms ("the paired assistant",
"your assistant session"). No keyboard shortcut at v1 (matches correction cockpit — sidebar
button only; see Decision 3).

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
- **Additive-first, with two named exceptions.** `LabLogPanel.vue`'s Chat button and
  `ViewerPanel.vue`'s push chip both migrate to the cockpit at v1 (Decisions 6 + 7). Every
  OTHER surface stays intact through v1 and gets touched only when the cockpit's version has
  demonstrably taken over.
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
   look. Accent: `--cc-kiwi` = teal (~`#14b8a6`), locked 2026-09-19. Chosen over kiwi-fruit
   green because `--cc-viewer` is already `#22c55e` (bright green) and two on-screen
   greens would collapse into one UI class. Family follows the accent-family shape:
   `--cc-kiwi-strong` / `--cc-kiwi-soft` / `--cc-kiwi-tint`. Token allocation lands in
   `frontend/src/style.css` + `docs/ui/PRIMITIVES.md` as part of PR #1.
3. **Entry = sidebar footer button ONLY. No keyboard shortcut at v1** (amended 2026-09-19).
   Correction cockpit has no shortcut either; matching the pattern keeps the keymap
   uncluttered and avoids a per-floating-panel inconsistency. Add one later only if real
   usage demands it.
4. **v1 content set** — the six rows below in *Contents — v1*.
5. **v2 content set — locked 2026-09-19.** Kiwi is a GLANCE surface, not a driver — "that's
   what Claude Code is for" (user). IN for v2 (four rows): MCP connection health badge;
   setup CTA merge from `LabLogPanel.vue`'s install band; assistant-authored lab-log peek;
   Blackboard entries list (contingent on BIDIR_CONTEXT_PLAN PR #7 shipping). OUT
   permanently: chat prompt bar, cross-session `SendMessage`, and the "share what I'm
   looking at" cockpit shortcut — all three turn Kiwi into a way to *drive* the assistant,
   which the terminal already does. See *Contents — v2* below.
6. **Displaces at v1 (immediate migration, not deferred):** `LabLogPanel.vue`'s "Chat to
   your assistant" button + the `buildChatPrompt` call site move to the cockpit. Rationale
   (from the user 2026-09-19): the chat handoff is not a lab-log concern; the button is a
   scavenger hunt away from the pairing state it depends on. `buildChatPrompt` itself
   stays in `lib/chatHandoff.ts` (still a pure helper); only the invocation moves.
7. **Also displaces at v1** (amended 2026-09-19, flipped from v2): `ViewerPanel.vue`'s push
   chip → cockpit only. Kiwi PR #1 removes it from `ViewerPanel.vue` in the same commit
   that mirrors it inside the cockpit. Rationale (user 2026-09-19): one place for pairing
   state from day one. If Kiwi's chip proves off in real use, re-adding is straightforward.
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

## Contents — v2 (locked 2026-09-19)

Four rows. Each ships when the row's data source is available.

- **MCP connection health badge.** Reads `useSettingsStore().observer*` (already tracked
  for AppHeader Settings → MCP). Renders `available` / `authenticated` / `throttled` as a
  small dot beside the section header. Surfaces auth-lost without opening Settings.
- **Setup CTA row.** When `observerSetupReason(...) !== null`, render the same install /
  re-auth CTA `LabLogPanel.vue` currently shows in its install band. Once this row is
  proven, v3 can remove the install band from `LabLogPanel.vue` entirely (v3, not v2 —
  additive-first).
- **Assistant-authored lab-log peek.** One-line preview of the most recent lab-log entry
  authored by the paired assistant (filter by author tag client-side). "Show more"
  scrolls `LabLogPanel` into view. Consumes `lab_log_updated` WS.
- **Blackboard entries list.** Quick list + "New entry" button. Contingent on
  BIDIR_CONTEXT_PLAN PR #7 shipping — ships as v2 tail-end once Blackboard is live.

**Out of scope permanently** (locked 2026-09-19): chat prompt bar (input → push channel),
cross-session `SendMessage`, "share what I'm looking at" cockpit shortcut. All three turn
Kiwi into a way to *drive* the assistant, which the terminal already does. Kiwi displays
what the assistant is doing; it does not become a chat client.

## Open items — resolved

All four resolved 2026-09-19:

1. **v2 content set** → Decision 5 above (four rows in; three permanently out).
2. **v1 displacement scope** → Decision 7 flipped to also-v1 (ViewerPanel push chip goes
   in the same PR #1 commit as the cockpit chip mirror).
3. **Cockpit accent colour** → Decision 2: `--cc-kiwi` = teal (~`#14b8a6`). Token
   allocation happens in PR #1.
4. **Keyboard shortcut** → Decision 3: none at v1 (match correction cockpit's
   sidebar-only entry).

## PR sequence

Independently mergeable in this order. Each PR ships a working, tested slice.

1. **Cockpit primitive + both v1 displacements + token allocation.** New
   `components/kiwi/KiwiCockpit.vue` (floating panel via `CorrectionCockpit`'s pattern);
   new `stores/kiwi.ts` (view state); new sidebar footer button in `AppSidebar.vue` (no
   keyboard shortcut per Decision 3); **chat-to-assistant button moved from
   `LabLogPanel.vue` to the cockpit** (Decision 6); **push chip removed from
   `ViewerPanel.vue`, mirrored inside the cockpit** via `usePushStore` +
   `fetchPushTarget` (Decision 7); `--cc-kiwi` token family allocated in
   `frontend/src/style.css` + documented in `docs/ui/PRIMITIVES.md` (Decision 2). Also
   the naming-ratchet grep test (Decision 12). ~400 lines. Ships the surface + both
   displacements.
2. **Recent captures + observer state.** Two `CollapsibleSection` blocks. Reuses
   `/api/viewer/captures` and `/api/observer/status`. Row-click on captures: pick between
   open-surface vs copy-id — needs a small ADR at PR time. ~200 lines.
3. **Session identity + explicit re-pair button.** Session identity is a read-only info
   block; re-pair posts to the small backend proxy that calls `register_push_target`
   server-side (no MCP invocation from the frontend). ~150 lines.
4. **v2 controls per OPEN 2** — split into further PRs based on which items land.

Total v1 estimate: ~750 lines across three PRs (PR #1 grew ~50 lines when Decision 7 flipped
to v1). v2 estimate: ~400 lines across two PRs — one for MCP-health + setup CTA + lab-log
peek (three glance rows sharing a WS listener), one for Blackboard list once PR #7 lands.

## References

- Shipped source of state the cockpit consumes: [`BIDIR_PUSH_PLAN.md`](BIDIR_PUSH_PLAN.md)
  (all three PRs shipped 2026-09-19: `#1048` pairing, `#1049` writer, `#1051` chip + WS).
- Related design: [`BIDIR_CONTEXT_PLAN.md`](BIDIR_CONTEXT_PLAN.md) — Blackboard and
  point-out surfaces the cockpit may host in v2.
- Primitive precedent: `frontend/src/components/correction/CorrectionCockpit.vue` — same
  shape.
- UI primitives catalog: [`docs/ui/PRIMITIVES.md`](../ui/PRIMITIVES.md).
- Existing AI-adjacent surfaces the cockpit consolidates:
  - `frontend/src/components/ViewerPanel.vue` — push chip (v1 removal — Decision 7)
  - `frontend/src/components/LabLogPanel.vue` — chat-to-assistant button (v1 removal —
    Decision 6); install band stays through v2 (v3 removal candidate)
  - `frontend/src/components/AppHeader.vue` — Settings → MCP connections
  - `frontend/src/utils/observerSetup.ts` — availability + setup reason
  - `frontend/src/lib/chatHandoff.ts` — `buildChatPrompt` (kept, invocation moves)
- Existing stores the cockpit reads: `stores/push.ts` (shipped `#1051`),
  `stores/projectMeta.ts`, `stores/settings.ts` (observer state, cockpit-open toggle).
