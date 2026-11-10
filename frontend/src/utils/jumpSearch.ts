// The header's jump search (docs/todo/GUI_LAYOUT_PLAN.md, M8; Ctrl K): one box that finds a page, a
// task function or an image of the open project. Matching is the app's one free-text rule
// (`matchesQuery`: every word somewhere in the fields); ranking puts a hit whose LABEL starts with the
// query first, then pages before functions before images, then label order. Pure, so it is tested.
import { matchesQuery } from './textQuery'
import type { MentionTarget } from './taskDiscovery'

export type JumpKind = 'page' | 'function' | 'image'
export interface JumpHit {
  kind: JumpKind
  label: string
  /** the second line: the group of a page, the page of a function, the set of an image */
  sub: string
  /** page: its route */
  route?: string
  /** function: where it lands and what it pre-selects */
  fn?: MentionTarget
  /** image: its uid and the set it is in */
  image?: { uid: string; setUid: string }
}

export interface JumpSources {
  pages: readonly { to: string; label: string; group: string; tip?: string; disabled?: boolean }[]
  functions: readonly MentionTarget[]
  images: readonly { uid: string; name: string; setUid: string; setName: string }[]
}

const KIND_ORDER: Record<JumpKind, number> = { page: 0, function: 1, image: 2 }

export function jumpHits(query: string, src: JumpSources, limit = 10): JumpHit[] {
  const q = query.trim()
  if (!q) return []
  const hits: JumpHit[] = []
  for (const p of src.pages) {
    if (!p.disabled && matchesQuery([p.label, p.group, p.tip], q))
      hits.push({ kind: 'page', label: p.label, sub: p.group, route: p.to })
  }
  for (const f of src.functions) {
    if (matchesQuery([f.label, f.page], q)) hits.push({ kind: 'function', label: f.label, sub: f.page, fn: f })
  }
  for (const i of src.images) {
    if (matchesQuery([i.name, i.uid, i.setName], q))
      hits.push({ kind: 'image', label: i.name, sub: i.setName, image: { uid: i.uid, setUid: i.setUid } })
  }
  const lq = q.toLowerCase()
  const starts = (h: JumpHit) => (h.label.toLowerCase().startsWith(lq) ? 0 : 1)
  return hits
    .sort((a, b) => starts(a) - starts(b) || KIND_ORDER[a.kind] - KIND_ORDER[b.kind] || a.label.localeCompare(b.label))
    .slice(0, limit)
}

/** Ctrl K / ⌘ K — the shortcut that focuses the box. */
export const isJumpShortcut = (e: { key: string; ctrlKey?: boolean; metaKey?: boolean; altKey?: boolean; shiftKey?: boolean }) =>
  !!(e.ctrlKey || e.metaKey) && !e.altKey && !e.shiftKey && e.key.toLowerCase() === 'k'
