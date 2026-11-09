/**
 * The ONE free-text search rule for list filters: every whitespace-separated word of `query` must
 * appear (case-insensitive substring) somewhere in `fields`. An empty query matches everything.
 * Shared by the task manager, model vault and project manager searches so every list answers the
 * same query the same way.
 */
export function matchesQuery(fields: readonly (string | null | undefined)[], query: string): boolean {
  const words = query.toLowerCase().split(/\s+/).filter(Boolean)
  if (!words.length) return true
  const hay = fields.filter(Boolean).join(' ').toLowerCase()
  return words.every(w => hay.includes(w))
}
