// Where a viewport-fixed floating panel (`components/FloatingPanel.vue`) is allowed to sit, and the
// rectangle it fills when maximised. Pure arithmetic — the caller measures the viewport and reads the
// header height — so the rules are unit-testable rather than buried in two copies inside the SFC.
//
// ── Why the top bound is NOT zero ────────────────────────────────────────────────────────────────
// Panels stack from `PANEL_Z_BASE` = 60 (utils/panelStack.ts); the app header is `z-index: 100` and
// `--cc-header-h` tall. So a panel dragged above the header's bottom edge is not merely hidden — the
// header paints over it and swallows its pointer events. The part that disappears first is the panel's
// OWN header, which is the only thing you can drag it back by, so the panel becomes unrecoverable
// without clearing localStorage. The floor is therefore the app header, not the viewport top.
//
// The old clamp used 0 for both the drag path and the mount/resize path — two copies of the bound, and
// both wrong by exactly the header height.
//
// ── This is not a third viewport-clamp ──────────────────────────────────────────────────────────
// `utils/anchorPosition.ts` places a box relative to an ANCHOR element (flip + align); a dragged window
// has no anchor. `composables/useFloatingPanel.ts` clamps CANVAS panels inside a zoomable offsetParent
// (different coordinate system, and it deliberately lets a panel run off to the left). Those two are a
// documented split (INVENTORY.md); this is the maths the top-level window already had, lifted out of
// the component so both of its call sites share one definition.

/** Panel pixels that must stay on screen horizontally — enough of the header to grab and drag. */
export const KEEP_VISIBLE_X = 60
/** Panel pixels that must stay on screen vertically — enough to show the header bar itself. */
export const KEEP_VISIBLE_Y = 40

export interface PanelBounds { minX: number; minY: number; maxX: number; maxY: number }
export interface Point { x: number; y: number }
export interface Rect { x: number; y: number; w: number; h: number }

/**
 * The travel limits for a panel's top-left corner.
 *
 * `maxY` is floored at `minY`: in a viewport shorter than the header plus the keep-visible strip there
 * is no legal position, and an inverted range would otherwise clamp the panel ABOVE the header — the
 * exact failure this function exists to prevent.
 */
export function panelBounds(viewportW: number, viewportH: number, headerH: number): PanelBounds {
  const minY = Math.max(0, headerH)
  return {
    minX: 0,
    minY,
    maxX: Math.max(0, viewportW - KEEP_VISIBLE_X),
    maxY: Math.max(minY, viewportH - KEEP_VISIBLE_Y),
  }
}

/** Pull a top-left corner back inside `b`. */
export function clampPanel(x: number, y: number, b: PanelBounds): Point {
  return {
    x: Math.min(Math.max(x, b.minX), b.maxX),
    y: Math.min(Math.max(y, b.minY), b.maxY),
  }
}

/**
 * The maximised rectangle: full width, and everything below the app header.
 *
 * Deliberately not the whole viewport — covering the header would hide the app's own controls behind a
 * panel that is now the size of the screen, and `y` still has to satisfy `panelBounds` so that
 * un-maximising cannot strand it.
 */
export function maximisedRect(viewportW: number, viewportH: number, headerH: number): Rect {
  const y = Math.max(0, headerH)
  return { x: 0, y, w: Math.max(0, viewportW), h: Math.max(0, viewportH - y) }
}
