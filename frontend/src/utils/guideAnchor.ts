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

export function resolveAnchor(id: string | undefined, root: ParentNode = document): HTMLElement | null {
  if (!id) return null
  try {
    return root.querySelector<HTMLElement>(anchorSelector(id))
  } catch {
    return null                              // a malformed id must not take the guide down
  }
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
