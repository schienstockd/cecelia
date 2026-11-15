// Folding the main window's menu and functions panel on a narrow window (a laptop, or the app on one
// half of a screen with the viewer on the other) — docs/todo/GUI_LAYOUT_PLAN.md → *Narrow windows*.
//
// Below `NARROW_LAYOUT_PX` both start folded, whatever the user saved: at 683 px (half of 1366) the
// two together leave the page a ~220 px column. The SAVED choice is never written by the fold: a
// click while narrow is a session override, dropped when the window widens again, so a wide window
// always shows exactly what the user last chose on a wide window. Opened while narrow, the two slide
// OVER the page instead of pushing it (App.vue / CollapsiblePanel `overlay`).

/** Window width (CSS px) below which the menu and the functions panel fold by default. */
export const NARROW_LAYOUT_PX = 1100

/** Is the shown state folded? `override` is a click made while narrow (`null` = none yet). */
export function effectiveFold(saved: boolean, narrow: boolean, override: boolean | null): boolean {
  return narrow ? (override ?? true) : saved
}

/** Where a click that sets the fold to `folded` goes: the saved choice on a wide window, the
 *  session override on a narrow one. */
export function foldWrite(narrow: boolean, folded: boolean):
  { saved: boolean } | { override: boolean } {
  return narrow ? { override: folded } : { saved: folded }
}
