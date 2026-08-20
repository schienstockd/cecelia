# Frontend — Vue 3 + TypeScript

This file loads only when a session touches `frontend/`. Root [`CLAUDE.md`](../CLAUDE.md) holds the
cross-cutting rules; this holds the frontend-only ones, so a Julia or Python session never pays for
them.

## Two MANDATORY lookups before you render anything

| Before you… | Read | Size |
|---|---|---|
| render **any** button, on/off toggle, slider, dialog/modal, popover, tabs, chips, empty state, spinner, badge, or collapsible | [`docs/ui/PRIMITIVES.md`](../docs/ui/PRIMITIVES.md) | 26 KB |
| write **any** user-facing text — labels, tooltips, tips, empty states, QC findings | [`docs/ui/COPY.md`](../docs/ui/COPY.md) | 20 KB |

Rendering a new variant of a primitive that already has a canonical form (`CcToggle`, `.cc-btn*`,
`ChipSelect`, `SwatchSelect`, `BaseModal`, `TeleportPopover`, `TabbedCanvas`, `CollapsibleSection`,
`ConfirmButton`, …) is a **bug**, not a style choice — same rule as H5AD/zarr/`run_py`. Both files are
enforced by tests, so skipping them fails the build. Unification status:
[`docs/todo/UX_PRIMITIVES_PLAN.md`](../docs/todo/UX_PRIMITIVES_PLAN.md).

## What already exists

[`docs/inventory/FRONTEND.md`](../docs/inventory/FRONTEND.md) — every shared component, store and util.
It's a flat bullet list, so `grep -n -i '<thing>' docs/inventory/FRONTEND.md` is the whole lookup; you
never need to read the file.

## Conventions

**Keep UI copy short.** Default to NO explanatory text in the app — no page subtitles, no paragraph tooltips, no prose QC findings. Where orientation genuinely isn't self-evident, one phrase under ~10 words, never two sentences. Long in-app prose is noise forever for the person who uses the page daily, and is the most reliable tell that a screen was generated. The explanation goes in the relevant `../docs/<AREA>.md`. Per-surface budgets: [`docs/ui/COPY.md`](../docs/ui/COPY.md).

**Persist every user-settable option.** Any option on a module page / canvas (chart type, scope, compare mode, highlights, sliders, …) MUST live in persisted view state (`useViewState` over a store-backed bag / panel `state`), never a bare `ref()` — a `ref()` resets on remount, so options vanish when the user navigates away and back. This is a hard convention for new pages; see `../docs/MODULES.md` → "RULE: persist every user-settable option" and `../docs/UI.md` → "Persisting view state".

**A continuous control's effect is coalesced, never per event.** A slider (`<input type="range">`, a drag handle, a wheel gesture) fires an event per pixel of travel. Its `@input` may WRITE the value; anything slower goes through one of the **three canonical schedulers** — `utils/debouncedLatest.ts` (a request), `utils/rafCoalesce.ts` (a paint), `utils/debouncedSave.ts` (a write-behind autosave) — or moves to `@change` (once, on release). **Never hand-roll a fourth `setTimeout` + sequence-token pair**, and **put the coalescing at the sink, not the call site**: one scheduler per slow endpoint, so a new caller can't reintroduce the spam. Enforced by `utils/continuousControls.test.ts`; rule + which to pick in `../docs/UI.md` → "Continuous controls".

## Full reference

[`docs/UI.md`](../docs/UI.md) — design tokens, button utilities, module page authoring, component
catalog, plot integration, WS event patterns. **It is 188 KB — slice it**:
`grep -n '^#\{2,3\} ' docs/UI.md` for the index, then `sed -n 'START,ENDp'`.

## Tests

`pixi run test-frontend` (Vitest). Scope is deliberately narrow: **pure logic in `src/utils/*.ts` only
— no component mounting, no jsdom/DOM/E2E.** Extract logic out of the `.vue` SFC first, then test that.
Conventions + rationale: [`docs/DEV.md`](../docs/DEV.md) → *Tests*.

Do **not** add task JSONs here — the spec is served from `app/src/tasks/` via
`GET /api/tasks/definitions`. `frontend/src/tasks/definitions/` is intentionally empty.
