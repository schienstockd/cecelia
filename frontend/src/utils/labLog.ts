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
