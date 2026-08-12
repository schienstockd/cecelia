// Resolving a guide step's anchor id to a live element, and deciding whether it is actually
// REACHABLE (plan D4/D5). The DOM half of the guide runtime, kept out of the store so the store
// stays about progression and this stays about the page.
//
// Two anchor schemes:
//   'segment.run'   → `[data-guide="segment.run"]`  — an explicit attribute in the markup
//   'nav:/segment'  → `a[href="#/segment"]`          — a sidebar nav item, addressed via its href
//                     (the nav is data-driven in AppSidebar, so it needs no attributes)
//
// Anchor ids are namespaced `<area>.<control>` and are asserted to exist in source by
// `lib/guides/guides.test.ts` — a renamed button fails CI instead of silently killing a guide for
// the one user being onboarded.

export const NAV_PREFIX = 'nav:'

// `location.hash` → the router path a step's `route` is compared against. Pure so it can be tested
// (the app tsconfig/vitest have no DOM), and because the value has to be RE-READ rather than only
// listened for: vue-router navigates a hash history with `history.pushState`, which fires no
// `hashchange`. See stores/guide.ts.
export function routePathFromHash(hash: string): string {
  return hash.replace(/^#/, '').split('?')[0] || '/'
}

// Only `"` and `\` need escaping inside a quoted attribute selector — deliberately not `CSS.escape`,
// which would mangle the `/` in a route path into `\/` and isn't available outside a browser.
const q = (s: string) => s.replace(/["\\]/g, '\\$&')

export function anchorSelector(id: string): string {
  if (id.startsWith(NAV_PREFIX)) {
    // hash history: the sidebar's RouterLink renders href="#/segment"
    return `a[href="#${q(id.slice(NAV_PREFIX.length))}"]`
  }
  return `[data-guide="${q(id)}"]`
}

// One anchor id can match SEVERAL live elements: each floating gating plot carries its own axis
// controls, each table row its own eye. Which one to point at, as a pure ranking so it can be tested
// (the DOM half below is a thin adapter; this project's vitest has no DOM by design).
//
// The rule, in order:
//   1. a visible candidate beats a hidden one — never ring something you can't see;
//   2. one inside the ACTIVE panel wins — with two gating plots open, the controls the user is working
//      in are the ones on the panel the canvas has marked active (`.panel.active`). Taking the first in
//      DOM order instead rang plot 1 while the user worked in plot 2, and since the ring sits above the
//      app it drew straight across the panel in front (Dominik, 2026-08-12);
//   3. an unoccluded candidate beats a covered one — a control under another panel is not the one being
//      pointed at.
export interface AnchorCandidate { reachable: boolean; inActive: boolean; occluded: boolean }

export function rankAnchorCandidates(cands: AnchorCandidate[]): number {
  if (cands.length === 0) return -1
  const score = (c: AnchorCandidate) =>
    (c.reachable ? 4 : 0) + (c.inActive ? 2 : 0) + (c.occluded ? 0 : 1)
  let best = 0
  for (let i = 1; i < cands.length; i++) if (score(cands[i]) > score(cands[best])) best = i
  return best                                // ties keep the earliest, i.e. DOM order
}

// Is something else drawn on top of this element's centre? The guide's own bubble does not count —
// it is deliberately placed beside the anchor, but a tooltip or its shadow can clip the midpoint.
function isOccluded(el: HTMLElement): boolean {
  const r = el.getBoundingClientRect()
  const hit = document.elementFromPoint(r.left + r.width / 2, r.top + r.height / 2)
  if (!hit) return true
  if (hit.closest('.guide-bubble')) return false
  return !(el === hit || el.contains(hit) || hit.contains(el))
}

export function resolveAnchor(id: string | undefined, root: ParentNode = document): HTMLElement | null {
  if (!id) return null
  let all: HTMLElement[]
  try {
    all = Array.from(root.querySelectorAll<HTMLElement>(anchorSelector(id)))
  } catch {
    return null                              // a malformed id must not take the guide down
  }
  if (all.length <= 1) return all[0] ?? null

  const idx = rankAnchorCandidates(all.map(el => ({
    reachable: isReachable(el),
    // `.panel.active` is CanvasPanel's active marker; `.closest` walks the floating-panel ancestry
    inActive: el.closest('.panel.active') !== null,
    occluded: isOccluded(el),
  })))
  return all[idx] ?? all[0]
}

// Is the element actually on screen and pointable? `offsetParent === null` catches `display: none`
// (the `v-show`n sidebar nav, a collapsed pane), and a zero rect catches a clipped/empty one. This
// is what makes a `reveal` step fire instead of the bubble pointing at nothing.
export function isReachable(el: HTMLElement | null): boolean {
  if (!el) return false
  if (el.offsetParent === null && getComputedStyle(el).position !== 'fixed') return false
  const r = el.getBoundingClientRect()
  if (r.width < 1 || r.height < 1) return false
  return getComputedStyle(el).visibility !== 'hidden'
}

// The value a control currently holds, for gates over state that lives in no store (`TaskRunner`'s
// function `<select>`, a checkbox, a text input). Checkboxes report 'true'/'false' rather than their
// `value` attribute, which is what a predicate actually wants to ask about.
export function readAnchorValue(id: string | undefined): string | null {
  const el = resolveAnchor(id)
  if (!el) return null
  if (el instanceof HTMLInputElement && (el.type === 'checkbox' || el.type === 'radio')) {
    return String(el.checked)
  }
  if (el instanceof HTMLSelectElement || el instanceof HTMLInputElement || el instanceof HTMLTextAreaElement) {
    return el.value
  }
  return el.getAttribute('data-guide-value') ?? el.textContent?.trim() ?? null
}

export interface VisibleRect { top: number; left: number; width: number; height: number }

// The part of an element you can ACTUALLY see: its rect intersected with every ancestor that clips
// its overflow, then with the viewport. `null` when nothing of it is visible.
//
// This exists because `getBoundingClientRect()` ignores clipping. `TaskRunner`'s parameters block is
// taller than the panel that scrolls it, so its rect reported the full height and the highlight ring
// drawn from it framed a region mostly outside the panel — a frame around nothing (Dominik,
// 2026-08-12). Same trap for any anchor inside a scroll container: the image table, the file list.
export function visibleRect(el: HTMLElement | null): VisibleRect | null {
  if (!el) return null
  const r = el.getBoundingClientRect()
  let top = r.top, left = r.left, right = r.right, bottom = r.bottom

  for (let p = el.parentElement; p; p = p.parentElement) {
    const cs = getComputedStyle(p)
    if (cs.overflowX === 'visible' && cs.overflowY === 'visible') continue
    const pr = p.getBoundingClientRect()
    top = Math.max(top, pr.top)
    left = Math.max(left, pr.left)
    right = Math.min(right, pr.right)
    bottom = Math.min(bottom, pr.bottom)
  }
  top = Math.max(top, 0)
  left = Math.max(left, 0)
  right = Math.min(right, window.innerWidth)
  bottom = Math.min(bottom, window.innerHeight)

  if (right - left < 1 || bottom - top < 1) return null
  return { top, left, width: right - left, height: bottom - top }
}

// Bring the target into view before the bubble is placed beside it — a step is useless if its control
// is scrolled out of the panel. `nearest` rather than `center` for the block axis would leave a row
// half under a sticky header, so centre it and accept the jump.
export function scrollAnchorIntoView(el: HTMLElement | null) {
  if (!el) return
  const r = el.getBoundingClientRect()
  const fullyVisible = r.top >= 0 && r.left >= 0 && r.bottom <= window.innerHeight && r.right <= window.innerWidth
  if (fullyVisible) return                   // don't jolt the page when it's already readable
  el.scrollIntoView({ block: 'center', inline: 'center', behavior: 'smooth' })
}
