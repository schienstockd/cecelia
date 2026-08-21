// View profiles — the pure logic behind a curated sidebar (docs/todo/VIEW_PROFILES_PLAN.md).
//
// A profile is a named, ORDERED SUBSET of the sidebar routes that already exist. It can include,
// exclude and reorder pages; it can never invent one, and it is NOT access control — a hidden page
// stays reachable by direct URL, which is why nothing here touches the router.
//
// Everything is a plain function over `{ heading, items: [{ to }] }` so it is unit-testable and the
// SFC keeps no logic (docs/DEV.md → Tests). `AppSidebar.vue` filters its `allGroups` through
// `applyProfile`; the Settings editor and the guide picker use the same helpers, so they cannot
// disagree with what the sidebar renders. Nothing here picks a landing page — `/` is its own neutral
// welcome route (`modules/WelcomeModule.vue`) precisely so no page has to be chosen.

import { NAV_PREFIX } from './guideAnchor'

export interface ProfileNavItem { to: string }
export interface ProfileNavGroup<T extends ProfileNavItem = ProfileNavItem> {
  heading: string
  items: T[]
}

export interface ViewProfile {
  id: string
  label: string
  items: string[]
}

/** Every route path the groups offer, in sidebar order. The set a profile may draw from. */
export function availablePaths<T extends ProfileNavItem>(groups: ProfileNavGroup<T>[]): string[] {
  return groups.flatMap(g => g.items.map(i => i.to))
}

/**
 * The groups a profile shows: built-in group order preserved, items filtered to `items` and ordered
 * by their position in it, and any group left empty dropped entirely (`customGroup` already behaves
 * that way when the user has no custom modules).
 *
 * A null/empty `items` means the implicit "All" profile — the groups pass through untouched, which is
 * today's behaviour and always the fallback.
 */
export function applyProfile<T extends ProfileNavItem>(
  groups: ProfileNavGroup<T>[],
  items: string[] | null | undefined,
): ProfileNavGroup<T>[] {
  if (!items || items.length === 0) return groups
  const rank = new Map(items.map((p, i) => [p, i]))
  const out: ProfileNavGroup<T>[] = []
  for (const g of groups) {
    const kept = g.items
      .filter(i => rank.has(i.to))
      .sort((a, b) => rank.get(a.to)! - rank.get(b.to)!)
    if (kept.length) out.push({ ...g, items: kept })
  }
  return out
}

/**
 * Paths a profile lists that no longer exist — a renamed page, a removed custom module. Dropped from
 * the menu silently (a broken entry must not break the sidebar) but reported in Settings, because a
 * profile that quietly shrinks gives the user no way to notice.
 */
export function unknownPaths(items: string[] | null | undefined, available: string[]): string[] {
  if (!items) return []
  const have = new Set(available)
  return items.filter(p => !have.has(p))
}

/**
 * The distinct route paths a guide's steps visit, in step order. A `GuideStep.route` is optional (the
 * orientation tour and the lab-log guide are not tied to a page), so a guide can legitimately declare
 * none.
 *
 * A `nav:/movies` ANCHOR counts as one of those pages: it points at the sidebar row for that page, so
 * a profile that hides the page removes the very element the step is about. Reading `route` alone
 * missed the two steps that only ever name a page that way ("Where they land" / "Where it lands",
 * both pointing at Movies) — the picker said Ready and the step then rendered the centred "That
 * control isn't on screen right now" card.
 */
export function guideRoutes(steps: { route?: string; anchor?: string }[]): string[] {
  const out: string[] = []
  const add = (p: string) => { if (!out.includes(p)) out.push(p) }
  for (const s of steps) {
    if (s.anchor?.startsWith(NAV_PREFIX)) add(s.anchor.slice(NAV_PREFIX.length))
    if (s.route) add(s.route)
  }
  return out
}

/**
 * Which of a guide's pages the active profile hides. Non-empty ⇒ the guide walks somewhere the user's
 * menu does not show, so the picker says so as a prerequisite miss (amber, Start still works — a
 * missing prereq is a warning about fit, not a lock; the page is still reachable).
 *
 * `visible` is the profile-filtered path list; `curatable` is the UNfiltered one — every path a
 * profile is allowed to list. Both are needed: a route outside the catalogue is not a page a profile
 * declined to show, it is chrome that was never on offer (`/settings` lives in the sidebar footer,
 * `/` is the welcome route, `/console` is a popout), so it can never be hidden and must not be
 * counted. Comparing against `visible` alone flagged the orientation tour's two `/settings` steps for
 * everyone, including a first launch with no profile chosen.
 *
 * A guide with no routes is never affected.
 */
export function hiddenGuideRoutes(
  steps: { route?: string; anchor?: string }[],
  visible: string[],
  curatable: string[],
): string[] {
  if (!visible.length) return []          // no profile resolved ⇒ nothing is hidden
  const shown = new Set(visible)
  const canHide = new Set(curatable)
  return guideRoutes(steps).filter(r => canHide.has(r) && !shown.has(r))
}
