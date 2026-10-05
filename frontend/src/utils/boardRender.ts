// The headless board render (modules/BoardRenderView.vue, driven by scripts/agent_eval/stage_boards.py):
// the pure half — what the page was asked to show, and which tab a board name means.

export interface BoardRenderQuery {
  projectUid: string
  imageUids: string[]   // empty = every image in the project
}

/** `?project=<uid>&images=<uid,uid>` → what to open. */
export function parseBoardRenderQuery(q: Record<string, unknown>): BoardRenderQuery {
  const one = (v: unknown): string => String(Array.isArray(v) ? v[0] ?? '' : v ?? '').trim()
  return {
    projectUid: one(q.project),
    imageUids: one(q.images).split(',').map(s => s.trim()).filter(Boolean),
  }
}

/** The tab a board NAME means — exact match, else the one match ignoring case/space; null when none or
 *  ambiguous (a render of the wrong board is worse than none). */
export function findBoardTab(tabs: { id: number; name: string }[], name: string): number | null {
  const exact = tabs.filter(t => t.name === name)
  if (exact.length === 1) return exact[0].id
  const norm = (s: string) => s.trim().toLowerCase()
  const loose = tabs.filter(t => norm(t.name) === norm(name))
  return loose.length === 1 ? loose[0].id : null
}

/** One captured slot, as the harness reads it back. */
export interface RenderedSlot { index: number; name: string; title?: string; png: string | null }

/** What two captures of a board must share to count as settled: every slot's image, by content. */
export function captureSignature(slots: { png: string | null }[]): string {
  // a hash of the whole data URL — a PNG's tail is its IEND chunk, the same for every image
  const hash = (t: string) => {
    let h = 0x811c9dc5
    for (let i = 0; i < t.length; i++) h = Math.imul(h ^ t.charCodeAt(i), 0x01000193)
    return (h >>> 0).toString(36)
  }
  return slots.map(s => s.png ? `${s.png.length}:${hash(s.png)}` : '-').join('|')
}
