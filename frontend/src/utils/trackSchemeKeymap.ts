// Keybindings for the track scheme timeline (docs/todo/TRACK_SCHEME_PLAN.md → P6).
//
// Inspired by napari-vizsla (Tamas Nagy, MIT — https://github.com/tlnagy/napari-vizsla): its two-
// letter op hotkeys (`l` link, `b` break) are the plugin's whole hook. We keep the letters over our
// richer op vocabulary — `l` = Join (same act), `b` = Split at the current frame (same semantic:
// cut one track into two), `r` = Remove — and add queue-level actions vizsla does not have because
// it commits per edit and cecelia batches through `tracking.correct_measures`.
//
// Kept pure so it can be unit-tested (frontend/CLAUDE.md → "extract logic to utils"). The SFC
// hands us a `KeyboardEvent` and calls whichever action name we return.

export type TrackSchemeAction =
  | 'join' | 'split' | 'remove'   // the op buttons
  | 'undo' | 'apply'              // queue-level
  | 'clearSel'                    // Escape

/** The shape of an event target these predicates read — duck-typed so tests need no DOM. */
interface KeyTarget { tagName?: string; isContentEditable?: boolean }

/**
 * True when a keystroke comes from a text field — a user typing in a search or a number knob is
 * NOT curating. Includes `contenteditable`. `<select>` deliberately excluded: its own key handling
 * consumes letters (type-ahead select), so an `l` inside a `<select>` never reaches this listener
 * anyway, and treating it as "typing" would mask a legitimate binding for the panel's own selects
 * (there are none today, but the rule reads better without the exception).
 *
 * Duck-typed on `tagName`/`isContentEditable` rather than `instanceof HTMLElement` because the
 * frontend test suite runs without jsdom (frontend/CLAUDE.md → pure logic in utils), so no DOM
 * constructors exist in-test. Runtime always has both.
 */
export function isTypingTarget(target: EventTarget | null): boolean {
  const t = target as KeyTarget | null
  if (!t || typeof t.tagName !== 'string') return false
  const tag = t.tagName.toUpperCase()
  if (tag === 'INPUT' || tag === 'TEXTAREA') return true
  if (t.isContentEditable === true) return true
  return false
}

/**
 * True when a keystroke is one the target element already handles itself — Enter/Space on a
 * button is the browser's click, and shadowing it with our `apply`/(nothing) binding double-fires
 * the intended action (user tabs to Join, presses Enter to click it — our Enter also fires apply,
 * submitting the whole queue). Left as an explicit predicate so the intent reads at the call site.
 */
export function isActivatingButton(e: KeyboardEvent): boolean {
  const t = e.target as KeyTarget | null
  if (!t || typeof t.tagName !== 'string') return false
  const tag = t.tagName.toUpperCase()
  if (tag !== 'BUTTON' && tag !== 'A') return false
  return e.key === 'Enter' || e.key === ' ' || e.key === 'Spacebar'
}

/**
 * Map a `KeyboardEvent` to the action it triggers, or `null` for a key we don't own.
 *
 * MODIFIERS ARE REFUSED. `Ctrl+L` opens the browser location bar, `Meta+L`/`Alt+L` are OS chrome —
 * a plain letter is the whole binding, and any modifier means "this is not for us". Shift on a
 * plain letter is accepted (some layouts type capitals with Shift held) — `event.key` is
 * lower-cased before comparison.
 */
export function keyToAction(e: KeyboardEvent): TrackSchemeAction | null {
  if (e.ctrlKey || e.metaKey || e.altKey) return null
  if (isTypingTarget(e.target)) return null
  if (isActivatingButton(e)) return null  // do not shadow Enter/Space on a focused button
  const k = e.key.toLowerCase()
  switch (k) {
    case 'l': return 'join'
    case 'b': return 'split'
    case 'r': return 'remove'
    case 'backspace': return 'undo'
    case 'enter': return 'apply'
    case 'escape': return 'clearSel'
    default: return null
  }
}

/** The letter each action is bound to, for tooltip hints — `Join (l)`, `Split (b)`, … */
export const KEY_HINT: Record<TrackSchemeAction, string> = {
  join: 'l', split: 'b', remove: 'r',
  undo: '⌫', apply: '⏎', clearSel: 'Esc',
}
