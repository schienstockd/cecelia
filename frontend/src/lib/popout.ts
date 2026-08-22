// Opening one of the app's views in its OWN browser window — the single place that builds the URL,
// names the window and knows which routes are popouts.
//
// Two surfaces do this (the console's ↗ and the Task Manager's ↗) and both need the same three
// things, none of which is obvious:
//
//  - a HASH url (`origin + pathname + '#/console'`), because the popup boots the same SPA from the
//    same document and the hash is what tells it which view to render bare;
//  - a STABLE window name, so clicking ↗ twice re-uses the window instead of stacking copies — plus
//    the `focus()` a bare `window.open` does not do, which is what makes the second click feel like
//    "show me that window" rather than nothing at all;
//  - `isPopoutWindow()`, so the shell can decline to install its background workers a second time.
//    A popup is a full second app instance with its own WS: whatever App.vue starts, it starts again
//    (see App.vue — the napari overlay restore and the lab-log auto-capture both act on the BACKEND,
//    so two windows meant two of every request).
//
// `/setup` is deliberately NOT here. It is bare but it is not a popout: it is the main window on a
// first launch, and it navigates to `/` when the wizard finishes — a window that started there must
// end up with the whole shell running.
export const POPOUT_ROUTES = ['/console', '/tasks-window'] as const

/** The absolute URL that boots this app on `hashPath` (e.g. `/tasks-window?project=abc`).
 *  `base` is a seam for the unit test — the suite runs in node, with no `location`. */
export function popoutUrl(
  hashPath: string, base: { origin: string; pathname: string } = location,
): string {
  return base.origin + base.pathname + '#' + hashPath
}

/** Open (or re-focus) the popup window for `hashPath`. `name` is what makes it one window, not many. */
export function openPopoutWindow(
  hashPath: string, name: string, width: number, height: number,
): Window | null {
  const w = window.open(popoutUrl(hashPath), name, `width=${width},height=${height}`)
  // A blocked popup returns null, and a cross-origin window would throw on focus() — neither can
  // happen here (same origin, user gesture), but a failed re-focus must not take the click with it.
  try { w?.focus() } catch { /* the window is open; focusing it is a nicety */ }
  return w
}

/** Is the window running this code one of the popouts? Reads the hash, not the router: this is asked
 *  during App.vue's setup, before the first navigation has resolved. Query string ignored. */
export function isPopoutWindow(hash: string = location.hash): boolean {
  const path = hash.replace(/^#/, '').split('?')[0]
  return (POPOUT_ROUTES as readonly string[]).includes(path)
}
