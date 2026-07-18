// Pure helpers for the lab-log panel (frontend half). The heavy lifting — parsing markdown into
// entries, injecting the dated author header, append-only writes — lives in the backend
// (app/src/lab_log.jl); the panel just renders what /api/lablog returns and formats a new entry.
// Keeping author classification + the correction template here (one source of truth) means the
// colour coding, the badge, and the correction prefill all agree. See docs/ai-assist/LAB-LOG.md.

export interface LabLogEntry {
  date: string        // YYYY-MM-DD (injected by the backend)
  author: string      // e.g. "Claude", "User", "User — correction"
  lines: string[]     // bullet lines
  raw: string         // the full block markdown
}

export type AuthorKind = 'claude' | 'correction' | 'cecelia' | 'user' | 'other'

// A thumbs verdict, and the panel's feedback mode.
//  - 'notes'  → thumbs+comment judge the DECISION → recorded as a [User] log note.
//  - 'tuning' → thumbs judge the ENTRY TYPE (useful/noise) → config sidecar, not the log.
export type Vote = 'up' | 'down'
export type FeedbackMode = 'notes' | 'tuning'

/** Only app/AI entries get thumbs — you don't rate your own notes. */
export function isRatable(author: string): boolean {
  const k = authorKind(author)
  return k === 'cecelia' || k === 'claude'
}

/**
 * Stable content id for an entry (keys tuning ratings). FNV-1a 32-bit over the raw block, hex. Must
 * be deterministic and stable across sessions — the backend is dumb storage keyed by whatever this
 * returns, so it can't drift with a Julia hash-seed change. Entries are append-only, so `raw` (hence
 * the id) never changes once written.
 */
export function entryId(raw: string): string {
  let h = 0x811c9dc5
  for (let i = 0; i < raw.length; i++) {
    h ^= raw.charCodeAt(i)
    h = Math.imul(h, 0x01000193)
  }
  return (h >>> 0).toString(16).padStart(8, '0')
}

/** Entries the panel should show: those whose `entryId` is NOT in the dismissed (hidden) list. Hiding
 *  is view-only — the lab-log file keeps every entry (append-only); this just filters the panel. */
export function visibleEntries(entries: LabLogEntry[], dismissed: string[]): LabLogEntry[] {
  const hidden = new Set(dismissed ?? [])
  return (entries ?? []).filter(e => !hidden.has(entryId(e.raw)))
}

/** Notes-mode decision-assessment prefill: a verdict + reference the user completes with the why. */
export function decisionPrefill(entry: Pick<LabLogEntry, 'date' | 'author'>, vote: Vote): string {
  return `${vote === 'up' ? '👍' : '👎'} re ${entry.date} [${entry.author}]: `
}

// The author tags the panel submits. The backend wraps these into the `[author]` header.
export const USER_AUTHOR = 'User'
export const CORRECTION_AUTHOR = 'User — correction'
// App-generated activity digests (backend author is CONTEXT_AUTHOR = "Cecelia").
export const CECELIA_AUTHOR = 'Cecelia'

/**
 * Classify an entry author for styling. Correction is checked BEFORE user because a correction
 * author string ("User — correction") also contains "user" — order matters.
 */
export function authorKind(author: string): AuthorKind {
  const a = (author ?? '').trim().toLowerCase()
  if (a.includes('correction')) return 'correction'
  if (a.includes('cecelia')) return 'cecelia'
  if (a.includes('claude')) return 'claude'
  if (a.includes('user')) return 'user'
  return 'other'
}

/** Prefill for correcting an entry: the user completes the reason after the colon. */
export function correctionPrefill(entry: Pick<LabLogEntry, 'date' | 'author'>): string {
  return `Corrects ${entry.date} [${entry.author}]: `
}

/**
 * Split a free-text draft into bullet lines for the append payload. Splits on newlines and drops
 * blank lines (the backend also drops blanks, but doing it here keeps the payload honest and lets
 * the panel reject an all-whitespace draft before calling the API).
 */
export function draftToLines(draft: string): string[] {
  return (draft ?? '')
    .split('\n')
    .map(l => l.trim())
    .filter(l => l.length > 0)
}

/** Split the muteable digest categories into the panel's two display groups: all module-page
 *  categories (always shown), and the general Operations group. Any orphaned mute — a category no
 *  longer in either backend list (e.g. a renamed/removed custom category) — is folded into Operations
 *  so it still renders and can be un-muted. Both lists keep the backend's (pipeline) order. */
export function muteGroups(pageCategories: string[], operationCategories: string[], mutes: string[]):
  { pages: string[]; operations: string[] } {
  const pages = pageCategories ?? []
  const ops = operationCategories ?? []
  const known = new Set<string>([...pages, ...ops])
  const orphans = (mutes ?? []).filter(m => !known.has(m))
  return { pages: [...pages], operations: [...ops, ...orphans] }
}

/** Display label for a mute chip: the category tag with a capitalised first letter, so the chips read
 *  uniformly (task specs tag some categories lower-case, e.g. "import"). The raw category stays the
 *  mute KEY — only the label is prettified. */
export function muteCategoryLabel(category: string): string {
  const c = category ?? ''
  return c ? c.charAt(0).toUpperCase() + c.slice(1) : c
}

/** Count entries authored by Claude that are newer than the last one the user has seen (by date +
 *  position). Used to badge unreviewed Claude entries. `seenRaw` is the `raw` of the newest entry
 *  the user has already seen; null/absent means everything is unseen. Entries are newest-first. */
export function unseenClaudeCount(entries: LabLogEntry[], seenRaw: string | null): number {
  let n = 0
  for (const e of entries) {
    if (seenRaw != null && e.raw === seenRaw) break
    if (authorKind(e.author) === 'claude') n++
  }
  return n
}
