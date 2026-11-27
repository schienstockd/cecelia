// The header's jump search (docs/todo/GUI_LAYOUT_PLAN.md, M8; Ctrl K): one box that finds a page, an
// in-app guide, a task function or an image of the open project. Guides make it answer "how do I …?"
// by SHOWING the control (docs/todo/HELP_SEARCH_PLAN.md, H1). Matching is the app's per-word rule
// (`matchesWords`: every word somewhere in the fields, filler words of a typed question skipped).
// Ranking, two tiers: a hit whose LABEL starts with the query first (pages, then guides, functions,
// images — so Enter on "track" still opens the page); then the rest with guides first, because a
// how-to question should open the walkthrough before a page that only mentions the words in its tip.
// Pure, so it is tested.
import { matchesWords } from './textQuery'
import type { MentionTarget } from './taskDiscovery'

export type JumpKind = 'page' | 'guide' | 'function' | 'image'
export interface JumpHit {
  kind: JumpKind
  label: string
  /** the second line: the group of a page or guide, the page of a function, the set of an image */
  sub: string
  /** page: its route */
  route?: string
  /** guide: its id, for `guide.start` */
  guideId?: string
  /** function: where it lands and what it pre-selects */
  fn?: MentionTarget
  /** image: its uid and the set it is in */
  image?: { uid: string; setUid: string }
}

export interface JumpSources {
  pages: readonly { to: string; label: string; group: string; tip?: string; disabled?: boolean }[]
  /** the guides this window offers (AI-filtered by the caller), with how many prerequisites are unmet */
  guides?: readonly { id: string; title: string; group: string; keywords?: readonly string[]; missing?: number }[]
  functions: readonly MentionTarget[]
  images: readonly { uid: string; name: string; setUid: string; setName: string }[]
}

// The words of a typed question that say nothing about WHAT is wanted. Not "find", "show", "open" or
// "record": those start page and guide titles.
const FILLER = new Set(['where', 'is', 'are', 'the', 'a', 'an', 'to', 'how', 'do', 'does', 'i', 'can',
  'what', 'which', 'button', 'my', 'me', 'for', 'please'])
const match = (fields: readonly (string | undefined)[], q: string) => matchesWords(fields, q, { stop: FILLER })

const PREFIX_ORDER: Record<JumpKind, number> = { page: 0, guide: 1, function: 2, image: 3 }
const REST_ORDER: Record<JumpKind, number> = { guide: 0, page: 1, function: 2, image: 3 }

export function jumpHits(query: string, src: JumpSources, limit = 10): JumpHit[] {
  const q = query.trim()
  if (!q) return []
  const hits: JumpHit[] = []
  for (const p of src.pages) {
    if (!p.disabled && match([p.label, p.group, p.tip], q))
      hits.push({ kind: 'page', label: p.label, sub: p.group, route: p.to })
  }
  // title + keywords, not the summary: a summary names other tools ("instead of gating …")
  for (const g of src.guides ?? []) {
    if (match([g.title, ...(g.keywords ?? [])], q))
      hits.push({ kind: 'guide', label: g.title, sub: g.missing ? `Guide · ${g.missing} missing` : `Guide · ${g.group}`, guideId: g.id })
  }
  for (const f of src.functions) {
    if (match([f.label, f.page], q)) hits.push({ kind: 'function', label: f.label, sub: f.page, fn: f })
  }
  for (const i of src.images) {
    if (match([i.name, i.uid, i.setName], q))
      hits.push({ kind: 'image', label: i.name, sub: i.setName, image: { uid: i.uid, setUid: i.setUid } })
  }
  const lq = q.toLowerCase()
  const starts = (h: JumpHit) => (h.label.toLowerCase().startsWith(lq) ? 0 : 1)
  const order = (h: JumpHit) => (starts(h) === 0 ? PREFIX_ORDER : REST_ORDER)[h.kind]
  return hits
    .sort((a, b) => starts(a) - starts(b) || order(a) - order(b) || a.label.localeCompare(b.label))
    .slice(0, limit)
}

/** Ctrl K / ⌘ K — the shortcut that focuses the box. */
export const isJumpShortcut = (e: { key: string; ctrlKey?: boolean; metaKey?: boolean; altKey?: boolean; shiftKey?: boolean }) =>
  !!(e.ctrlKey || e.metaKey) && !e.altKey && !e.shiftKey && e.key.toLowerCase() === 'k'
