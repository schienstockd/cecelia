> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in [`docs/todo/CONTEXTUAL_HINTS_PLAN.md`](../todo/CONTEXTUAL_HINTS_PLAN.md).
>
> **Outcome:** run 2026-11-24 against `origin/main` e703aa12; the plan above is the output. Nothing
> built yet. One premise had moved: the brief treats hints as new, but a first-use hint system
> (`ONBOARDING_PLAN.md` P4) was built and its hints retired on 2026-08-13 (`HintCallout` remains, unused), and `docs/UI.md` →
> *First-use hints* sets the bar a hint must clear. Decided 2026-11-25: clarity first. Each hidden
> gesture gets a visible surface; that rule replaces the bar, and the hint system is built only if a
> re-score leaves more than five targets invisible.

# CONTEXTUAL_HINTS_AUDIT_PROMPT

## Goal

Audit the Cecelia GUI to find elements, buttons and functions a user is unlikely to discover or understand on their own, and produce a ranked list of **hint targets** plus a design for a lightweight hint system.

The in-app guides are at capacity and cannot carry more. Hints must cover the gap without annoying users.

This is an audit and plan. **Do not implement.** Output goes to `CONTEXTUAL_HINTS_PLAN.md`.

## Context

- The GUI is getting dense. Related: `GUI_CLARITY_AUDIT_PROMPT.md` (density, cutting) and its plan. This audit does not repeat that one. Cutting or restructuring is that audit's job. Here, the question is: **for what stays, how does a user find out it exists?**
- Success criterion: users never ask "where do I click now" and never miss important features (acquisition checks, claim checks, bulk actions, shortcuts) because they are buried or invisible.
- Feel: light, loose, not dense. Hints must not add weight.
- Existing guides: read them first and list what they already cover. Do not duplicate.

## Step 1: Inventory

Walk every page and surface (main GUI, task panes, gating, populations, analysis board, viewer popout). For each, list every interactive element: buttons, icon-only controls, menus, context menus, keyboard shortcuts, drag/drop and click-modifier behaviours, collapsed panels, toggles, hover-only affordances.

Record: page, element, what it does, how it is revealed (always visible / hover / right-click / shortcut / hidden panel), and whether any guide or tooltip already covers it.

Cite file paths and component names (e.g. `PlotPicker.vue`). Do not guess; read the code.

## Step 2: Score discoverability

Score each element 0-3 on each axis:

| Axis | 0 | 3 |
|---|---|---|
| Visibility | always visible, labelled | hidden: shortcut, right-click, modifier, hover-only |
| Self-explanation | label/icon is clear | icon-only or jargon, intent unclear |
| Stakes of missing it | cosmetic | users get wrong results or waste major time (e.g. skipped acquisition or claim checks) |
| Frequency of need | rare | most users need it on this page |

Priority = (Visibility + Self-explanation) x (Stakes + Frequency). List the scoring so it can be challenged.

Also flag **manual long ways**: places where a user can repeat an action many times that a bulk or shortcut function handles in one step. These are the best behaviour-trigger candidates.

## Step 3: Pick hint targets

From the scored list, select the top targets. For each, specify:

- `id` (stable, kebab-case)
- `page`
- `anchor` (selector or component ref)
- `trigger` (see below)
- `text` (one line, max ~12 words, plain language, no jargon)
- `retire-when` (user has used the feature, even without seeing the hint)
- why it was chosen (the score and the failure it prevents)

Triggers, in order of preference:
1. **Behaviour signal**: fires on something the user did (selected N items, repeated an action 3x, opened an empty panel).
2. **Empty state / inline**: placed where the user already looks.
3. **Idle**: user sat on the page N seconds without touching the relevant area.
4. **Passive beacon**: small dot on the control, tip on hover/click.
5. **Per-page tips drawer**: pull, never push; unread badge.

Reject any target that would be better solved by a clearer label, a visible affordance or removing the element. List those separately as "fix in the GUI, not with a hint" and feed them to the clarity audit.

## Step 4: Hint system design

Propose the smallest system that works. Check the existing frontend for anything reusable first (tooltips, onboarding, guide components, state stores).

Constraints:
- One hint per page visit at most; a per-session cap.
- Never blocks the UI: no modals, no dimmed overlays, no focus stealing.
- Every hint is dismissible, with a "Don't show tips" opt-out.
- Seen/dismissed state is persisted per user, per hint. Never shown again after dismissal.
- Hints retire automatically once the feature is used.
- A single hint manager enforces the caps; adding a hint is one registry entry (`{id, page, trigger, anchor, text}`), not new component code.
- Must not collide with the popout viewer, the checks card or the next-step pill. State how hint placement avoids them.
- Accessible: keyboard reachable, screen-reader text, respects reduced motion.
- Visual style matches the lighter, looser direction. No heavy tour chrome.

Decide where state lives and how it is reset. Decide whether hints are also usable by Claude/Kiwi (e.g. the assistant can point at the same registry instead of writing new help text).

## Step 5: Keeping hints current

The main risk is staleness: functionality keeps being added and changed, and hints must not drift. Design a mechanism that makes staleness a build or review failure, not a discipline problem. Check existing infrastructure first (recital reviewers, CI tests, `CLAUDE.md` rules, docshots) and reuse it.

Evaluate and decide on:
- **Anchor test:** every registry `anchor` is resolved in a test; CI fails if the component or selector is gone.
- **Stable anchors:** `data-hint` attributes instead of CSS classes or DOM paths.
- **Coverage check:** user-facing controls must have a registry entry or an explicit `hint: none` with a reason, so new buttons force a decision.
- **PR-time gate:** a reviewer check (alongside the existing recital reviewers) that flags PRs which add or change a user-facing control without touching the registry or stating why not.
- **Feature link:** each hint references the function, route or task it describes; a change to that code flags the hint for review.
- **Text drift:** how hint text is verified against real behaviour.
- **Agent rule:** the instruction in `CLAUDE.md` so implementing agents update hints when they add functionality.

State the cost of each (CI time, friction on PRs, false positives) and recommend the smallest set that actually prevents drift.

## Step 6: Risks and measurement

- Annoyance risks and how the caps address them.
- How to tell the system works: which signals to log (hint shown, dismissed, feature used after hint) without adding tracking the project does not already have.
- Failure modes: stale anchors after UI refactors (propose a test that fails if a registry anchor no longer exists), hint text drifting from real behaviour.
- What should NOT get a hint.

## Output format

Write `CONTEXTUAL_HINTS_PLAN.md` with:

1. **Existing coverage**: what guides and tooltips already do.
2. **Inventory and scores**: table, sorted by priority.
3. **Top hint targets**: the registry entries from Step 3, ready to paste.
4. **Fix in the GUI instead**: elements that need clarity changes, not hints.
5. **System design**: manager, state, caps, placement, accessibility.
6. **Keeping hints current**: the chosen mechanism, its cost, and the `CLAUDE.md` rule text.
7. **Phasing**: smallest first slice (suggest 5-8 hints on 2-3 pages), then expansion.
8. **Risks and open questions**: keep to what needs a decision from the developer.

## Rules

- Read the code and the existing guides before judging. Cite files.
- Be blunt. If an element is fine, say so; do not pad the list.
- Prefer fewer, higher-value hints over coverage.
- Do not invent features. Every hint must describe something that exists and works today.
- Keep the plan terse.
