// Opening one of the app's views in its OWN browser window — the single place that builds the URL,
// names the window and knows which routes are popouts.
//
// Three surfaces do this (the console's ↗, the Task Manager's ↗ and the image table's ↗) and they all
// need the same three things, none of which is obvious:
//
//  - a HASH url (`origin + pathname + '#/console'`), because the popup boots the same SPA from the
//    same document and the hash is what tells it which view to render bare;
//  - a STABLE window name, so clicking ↗ twice re-uses the window instead of stacking copies — plus
//    the `focus()` a bare `window.open` does not do, which is what makes the second click feel like
//    "show me that window" rather than nothing at all;
//  - `isPopoutWindow()`, so the shell can decline to install its background workers a second time.
//    A popup is a full second app instance with its own WS: whatever App.vue starts, it starts again
//    (see App.vue — the viewer overlay restore and the lab-log auto-capture both act on the BACKEND,
//    so two windows meant two of every request).
//
// Not every popout follows the main window. The Task Manager does (a list scoped to the project you
// just left, still labelled as current, is worse than an empty one); the volume viewer deliberately
// does NOT — it is a comparison surface, and a view being measured against viewer must not move
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
import { taskLanding } from '../utils/kiwiTurn'
import { profileStorage } from '../utils/profileStorage'

/** The popout routes and the window name each one owns. One map, so a route can never be opened into
 *  the wrong window (or into an unnamed one, which is how you get two copies stacked on top). */
export const POPOUT_WINDOW_NAMES = {
  '/console':       'cecelia-console',
  '/tasks-window':  'cecelia-tasks',
  '/viewer-window': 'cecelia-viewer',
} as const

export type PopoutRoute = keyof typeof POPOUT_WINDOW_NAMES

export const POPOUT_ROUTES = Object.keys(POPOUT_WINDOW_NAMES) as PopoutRoute[]

// Handles of popouts THIS window opened, keyed by route. Populated by `openPopoutWindow`; consumed by
// `getOpenPopoutWindow` so a caller (e.g. the analysis-board strip's capture) can reach into the
// popup's own window object (same-origin) without accidentally opening a fresh blank window — which is
// what `window.open('', name)` does when no such name exists in this browser session.
const POPOUT_HANDLES: Partial<Record<PopoutRoute, Window>> = {}
/** The live handle for a popout THIS session opened, or `null` when the popup was closed or never
 *  opened. Same-origin so the caller can read properties / call exposed functions on it. */
export function getOpenPopoutWindow(route: PopoutRoute): Window | null {
  const w = POPOUT_HANDLES[route]
  if (w && !w.closed) return w
  if (w) delete POPOUT_HANDLES[route]
  return null
}

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
  const url = popoutUrl(route + query)
  const w = window.open(url, POPOUT_WINDOW_NAMES[route], `width=${width},height=${height}`)
  // A blocked popup returns null, and a cross-origin window would throw on focus() — neither can
  // happen here (same origin, user gesture), but a failed re-focus must not take the click with it.
  try { w?.focus() } catch { /* the window is open; focusing it is a nicety */ }
  // Second click on a DIFFERENT target — the eye on another image, or the ↗ on another set — reuses
  // the named window. `window.open` on an existing window is spec'd to load the new URL into it, but
  // browsers differ on hash-only navigations and on whether the third `features` argument suppresses
  // the load; the viewer popup once stayed on the first image forever because the `imageUid` was
  // captured at setup. Assign `href` explicitly: the viewer's own watch on `route.query.image`
  // takes it from there and hard-reloads for a clean re-init. Guarded on same-origin (a cross-origin
  // read throws) and on URL difference (avoids a spurious reload of the same target).
  try {
    if (w && w.location.href !== url) w.location.href = url
  } catch { /* cross-origin — impossible for our own popouts */ }
  if (w) POPOUT_HANDLES[route] = w                  // cached for `getOpenPopoutWindow`
  return w
}

// ── The way back: a popout raising the MAIN window ──────────────────────────────────────────────────
// `window.opener.focus()` is ignored by Chrome — a page may not raise another window. Targeting a
// window by NAME with `window.open` does raise it (it is a navigation in that window, on a user
// gesture). So the main window names itself once (`nameMainWindow`, App.vue) and a popout opens its
// current URL by that name: the same hash, so the main window stays exactly where it was.
export const MAIN_WINDOW_NAME = 'cecelia-main'

/** Give the main window its name, unless it already has one (a popout is never renamed). */
export function nameMainWindow(win: { name: string } = window): void {
  if (!win.name) win.name = MAIN_WINDOW_NAME
}

/** The URL to open in the main window: its own, when the opener can be read, else the app's root. */
export function mainWindowUrl(opener: { closed?: boolean; location?: { href: string } } | null,
                              origin: string, pathname: string): string {
  try {
    if (opener && !opener.closed && opener.location?.href) return opener.location.href
  } catch { /* closed or unreadable opener */ }
  return origin + pathname + '#/'
}

/** Raise (or, if it was closed, reopen) the main window from a popout. Call it from a click. */
export function focusMainWindow(): void {
  const w = window.open(mainWindowUrl(window.opener as Window | null, location.origin, location.pathname), MAIN_WINDOW_NAME)
  try { w?.focus() } catch { /* opened; raising it is the browser's call */ }
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

// ── Side by side: the viewer on the right half of the screen ────────────────────────────────────────
// A page may move and resize a window that `window.open` made (one tab, no toolbar), never a tab — so
// the viewer popout can place ITSELF on the right half, and the main window is snapped by the user
// (Super+← / Win+←); below 1100 px it folds its menu and panel to fit (utils/narrowLayout.ts).
// docs/todo/GUI_LAYOUT_PLAN.md → *Narrow windows*.
//
// Order matters, measured on GNOME under XWayland: the window manager clamps a move that would push
// the window off-screen. So both ways go SIZE FIRST, then move: the target size always fits the
// screen, so the move that follows can never be clamped. Move-first landed a 1200 px wide viewer at
// x = 720 (clamped short of the half) and took three clicks to arrive; on the way back it left the
// big window where it was. The resize lands asynchronously (a read 600 ms later still saw the old
// size), so the move waits `PLACE_MOVE_DELAY_MS`, and one settle pass `PLACE_SETTLE_MS` later
// places it again if it is not there yet.
//
// Whether the window IS side by side is read at the click: a saved placement alone said "snapped"
// after the user dragged the window away by hand, and the next click sent it "back" instead of to
// the half. `isSideBySide` = a placement is saved AND the window still sits on the right half
// (within the title bar's offset — GNOME reports the content 37 px below the target, that much
// shorter).

export interface WinRect { x: number; y: number; w: number; h: number }
/** `screen`, with Chrome's non-standard origin of the available area (the dock / top bar). */
export type ScreenArea = { availLeft?: number; availTop?: number; availWidth: number; availHeight: number }

/** The right half of the screen's available area — the odd pixel goes to the right half. */
export function rightHalf(s: ScreenArea): WinRect {
  const half = Math.floor(s.availWidth / 2)
  return { x: (s.availLeft ?? 0) + half, y: s.availTop ?? 0, w: s.availWidth - half, h: s.availHeight }
}

/** Where the window was before it went to the half. Session storage: per window, and it survives
 *  the viewer's hard reload on an image switch, so the second click still finds its way back. */
const SIDE_BY_SIDE_KEY = 'cc.vw.sideBySide'

/** Delay between the resize and the move (the resize lands asynchronously). */
export const PLACE_MOVE_DELAY_MS = 300
/** When the one settle pass checks the window landed, and places it again if not. */
export const PLACE_SETTLE_MS = 1000

type PlaceableWindow = Pick<Window, 'moveTo' | 'resizeTo' | 'screenX' | 'screenY' | 'outerWidth' | 'outerHeight'>
type KeyStore = Pick<Storage, 'getItem' | 'setItem' | 'removeItem'>

function safeStore(): KeyStore | null {
  try { return sessionStorage } catch { return null }
}

/** The placement saved before going side by side, or `null` when the window is not side by side. */
export function savedPlacement(store: KeyStore | null = safeStore()): WinRect | null {
  try {
    const r = JSON.parse(store?.getItem(SIDE_BY_SIDE_KEY) ?? 'null') as WinRect | null
    return r && [r.x, r.y, r.w, r.h].every(Number.isFinite) ? r : null
  } catch { return null }
}

/** How far the window may sit from the half and still count as on it: a title bar's worth down and
 *  shorter, a little sideways for a frame — a hand drag moves it further. */
export const PLACEMENT_SLACK = { x: 40, y: 80 } as const

/** Is `win` on `to`, give or take the window frame? */
export function isAtPlacement(win: PlaceableWindow, to: WinRect): boolean {
  return Math.abs(win.screenX - to.x) <= PLACEMENT_SLACK.x && Math.abs(win.outerWidth - to.w) <= PLACEMENT_SLACK.x
    && Math.abs(win.screenY - to.y) <= PLACEMENT_SLACK.y && Math.abs(win.outerHeight - to.h) <= PLACEMENT_SLACK.y
}

/** Side by side right now: a way back is saved AND the window is still on the right half. Moved or
 *  resized by hand since, it is not — the next click places it again. */
export function isSideBySide(
  win: PlaceableWindow = window, area: ScreenArea = screen as ScreenArea,
  store: KeyStore | null = safeStore(),
): boolean {
  return savedPlacement(store) !== null && isAtPlacement(win, rightHalf(area))
}

/** Size first, then move (never clamped: the size fits), then one settle pass. */
function place(win: PlaceableWindow, to: WinRect): void {
  const go = () => {
    win.resizeTo(to.w, to.h)
    setTimeout(() => win.moveTo(to.x, to.y), PLACE_MOVE_DELAY_MS)
  }
  go()
  setTimeout(() => { if (!isAtPlacement(win, to)) go() }, PLACE_SETTLE_MS)
}

/** Put the window on the right half, or back where it was. Returns the new state (true = side by
 *  side). Call it from a click; a window the browser will not move just stays put. A window moved
 *  away by hand goes to the half again, and where it was moved to becomes the way back. */
export function toggleSideBySide(
  win: PlaceableWindow = window, area: ScreenArea = screen as ScreenArea,
  store: KeyStore | null = safeStore(),
): boolean {
  const back = isSideBySide(win, area, store) ? savedPlacement(store) : null
  if (back) {
    try { store?.removeItem(SIDE_BY_SIDE_KEY) } catch { /* the window still goes back */ }
    place(win, back)
    return false
  }
  const was: WinRect = { x: win.screenX, y: win.screenY, w: win.outerWidth, h: win.outerHeight }
  try { store?.setItem(SIDE_BY_SIDE_KEY, JSON.stringify(was)) } catch { /* no way back, still placed */ }
  place(win, rightHalf(area))
  return true
}

/** Open a task's form on its page in the MAIN window, from a popout — the task selected, never started.
 *  The selection rides the TaskRunner's own remembered pick (`cc-fn:<module>`, `taskLanding`); a page
 *  already showing picks it up from the storage event (TaskRunner). Call it from a click. */
export function openTaskInMainWindow(funName: string, def?: { task: string }): boolean {
  const land = taskLanding(funName, def)
  if (!land) return false
  profileStorage.setItem(`cc-fn:${land.module}`, land.task)
  const w = window.open(`${location.origin}${location.pathname}#${land.path}`, MAIN_WINDOW_NAME)
  try { w?.focus() } catch { /* opened; raising it is the browser's call */ }
  return true
}
