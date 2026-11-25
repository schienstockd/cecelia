/**
 * Is this event coming from somewhere the user is TYPING?
 *
 * Every window-level keyboard shortcut needs this, and needs it identically: a bare
 * `keydown` listener also fires while the user is in a rename field, so without the check
 * typing "z" in a population name silently triggers whatever `z` is bound to. The guard was
 * written out inline in `MoviesModule` (zoom keys) and `useCanvasZoom`; this is the third
 * caller (gating undo/redo), so it lives in one place instead.
 *
 * `isContentEditable` is the part that is easy to forget and hard to notice missing — it
 * covers the rich-text/editable surfaces where `tagName` is a `div`.
 */
export function isTypingTarget(e: Event): boolean {
  const t = e.target as HTMLElement | null
  return !!t && (t.tagName === 'INPUT' || t.tagName === 'TEXTAREA' || t.isContentEditable)
}

/**
 * Does this keydown clear the page's selection? Escape, not while typing, and not when something
 * nearer already claimed it (a gate being drawn, a popover closing — they `preventDefault`).
 * Shared by every page with an Escape-to-clear selection (Behaviour, Gating) so the guards match.
 */
export function escapeClearsSelection(e: KeyboardEvent, hasSelection: boolean): boolean {
  return e.key === 'Escape' && hasSelection && !e.defaultPrevented && !isTypingTarget(e)
}

const ARROW_KEYS = new Set(['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight'])

/**
 * Is this keydown about to move nodes through Vue Flow's keyboard nudge? Vue Flow (keyboard a11y
 * on) moves the selected nodes on an arrow key pressed on a focused selected node or on the
 * box-selection rectangle — with no drag-start event, so an editor that records undo on drag must
 * record on this too. Call it in the capture phase, before Vue Flow moves anything.
 */
export function nudgesSelection(e: KeyboardEvent, hasSelectedNodes: boolean): boolean {
  if (!ARROW_KEYS.has(e.key) || !hasSelectedNodes || isTypingTarget(e)) return false
  const t = e.target as HTMLElement | null
  return !!t?.closest?.('.vue-flow__node, .vue-flow__nodesselection-rect')
}
