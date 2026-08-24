// Opening one of the app's views in its OWN browser window — the single place that builds the URL,
// names the window and knows which routes are popouts.
//
// Three surfaces do this (the console's ↗, the Task Manager's ↗ and the viewer panel's ↗) and they all
// need the same three things, none of which is obvious:
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
// Not every popout follows the main window. The Task Manager does (a list scoped to the project you
// just left, still labelled as current, is worse than an empty one); the volume viewer deliberately
// does NOT — it is a comparison surface, and a view being measured against napari must not move
// because someone clicked another row.
//
// **The name is the window's identity, and it is the durable half of it.** The hash says which view
// this window is showing *right now*; the name says which view the window IS. They come apart — a
// popout that ends up on a shell route (a stale dev bundle, a restored session, a hand-edited URL)
// still has its name, and reading only the hash meant such a window rendered the whole app shell
// inside a 1100×700 popup: header, sidebar with every module locked, no project, an empty task list.
// So popout-ness is answered from the name first (`popoutRouteOfWindow`), and `main.ts` sends a
// misrouted popout back to its own route rather than leaving it as a second, crippled copy of the app.
//
// `/setup` is deliberately NOT here. It is bare but it is not a popout: it is the main window on a
// first launch, and it navigates to `/` when the wizard finishes — a window that started there must
// end up with the whole shell running.

/** The popout routes and the window name each one owns. One map, so a route can never be opened into
 *  the wrong window (or into an unnamed one, which is how you get two copies stacked on top). */
export const POPOUT_WINDOW_NAMES = {
  '/console':       'cecelia-console',
  '/tasks-window':  'cecelia-tasks',
  '/viewer-window': 'cecelia-viewer',
} as const

export type PopoutRoute = keyof typeof POPOUT_WINDOW_NAMES

export const POPOUT_ROUTES = Object.keys(POPOUT_WINDOW_NAMES) as PopoutRoute[]

/** The absolute URL that boots this app on `hashPath` (e.g. `/tasks-window?project=abc`).
 *  `base` is a seam for the unit test — the suite runs in node, with no `location`. */
export function popoutUrl(
  hashPath: string, base: { origin: string; pathname: string } = location,
): string {
  return base.origin + base.pathname + '#' + hashPath
}

/** Which popout view a window NAMED `windowName` is — `null` for the main window (name `''`) or any
 *  window the app did not open. Durable: the name outlives navigation and reload, the hash does not. */
export function popoutRouteOfWindow(windowName: string = window.name): PopoutRoute | null {
  return POPOUT_ROUTES.find(r => POPOUT_WINDOW_NAMES[r] === windowName) ?? null
}

/** Open (or re-focus) the popup window for `route`, with an optional `query` (`?project=…`). The name
 *  comes from the map, not the caller: it is what makes it one window, not many. */
export function openPopoutWindow(
  route: PopoutRoute, width: number, height: number, query = '',
): Window | null {
  const w = window.open(popoutUrl(route + query), POPOUT_WINDOW_NAMES[route],
                        `width=${width},height=${height}`)
  // A blocked popup returns null, and a cross-origin window would throw on focus() — neither can
  // happen here (same origin, user gesture), but a failed re-focus must not take the click with it.
  try { w?.focus() } catch { /* the window is open; focusing it is a nicety */ }
  return w
}

/** Is the window running this code one of the popouts? The NAME decides (see the header); the hash is
 *  the fallback, for a window that has the URL but not the name — someone opening `#/console` in a
 *  tab by hand. Asked during App.vue's setup, before the first navigation has resolved, so it reads
 *  neither the router nor anything async. Query string ignored. */
export function isPopoutWindow(
  hash: string = location.hash, windowName: string = window.name,
): boolean {
  if (popoutRouteOfWindow(windowName)) return true
  const path = hash.replace(/^#/, '').split('?')[0]
  return (POPOUT_ROUTES as readonly string[]).includes(path)
}
