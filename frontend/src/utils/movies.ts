// Movie-player (/movies) pure helpers — kept out of the .vue SFC so they're unit-testable. The player
// lists the project's rendered .mp4s (GET /api/movies) and streams them via the range-capable serve
// route (GET /api/movies/file). See api/src/server.jl → try_serve_movie / api_movies_list.

export interface MovieEntry {
  name: string     // file name, e.g. "myImage_animation.mp4" — the id, and what the player streams
  size: number     // bytes
  mtime: number    // unix seconds (Float)
  // ── registry fields (settings/movies.json, merged in by /api/movies) ─────────────────────────────
  // All optional: every movie recorded before the registry existed has none of them, and the page
  // must read exactly the same for those rows.
  displayName?: string   // what the list SHOWS; the file is never renamed (MOVIE_MANAGEMENT_PLAN Decision 2)
  starred?: boolean
  tags?: string[]        // free-form; the growing taxonomy (Decision 3)
  producedBy?: string    // 'viewer' | 'animation' | 'batch' — written by the recorder, not the user
  hasConfig?: boolean    // a saved generation config exists (fetched on demand, not in the list)
  configKind?: string    // 'look' | 'keyframes' (Decision 7)
  configStale?: boolean  // the file was re-recorded after the config was saved (Decision 5)
}

/** The producers, and how each is labelled. A movie with no `producedBy` predates the registry. */
export const MOVIE_PRODUCERS: Record<string, string> = {
  viewer: 'viewer', animation: 'animation', batch: 'batch',
}

// URL the <video> element points at. The name is a server-sanitised basename ([A-Za-z0-9._-]+.mp4),
// but encode it anyway so it survives as a query param regardless.
export function movieStreamUrl(projectUid: string, name: string): string {
  return `/api/movies/file?projectUid=${encodeURIComponent(projectUid)}&name=${encodeURIComponent(name)}`
}

// Display label for a movie: the user's display name when they set one, else the file name without
// `.mp4`. Everything else (uid suffix, attr parts) is left as-is — that's how the recorder named it,
// and the user chose those file attrs.
//
// Takes the whole entry OR a bare file name, because the player caption only ever has the name and
// the table always has the row. A blank display name reads as "not set" rather than as an empty
// label — that is how clearing an inline edit gets back to the filename.
export function movieDisplayName(movie: string | MovieEntry): string {
  if (typeof movie !== 'string') {
    const custom = (movie.displayName ?? '').trim()
    if (custom) return custom
    return movie.name.replace(/\.mp4$/i, '')
  }
  return movie.replace(/\.mp4$/i, '')
}

// Newest-first, then by name for stable ordering of same-mtime files. The backend already sorts by
// mtime desc, but re-sorting here keeps the UI correct if the list is ever assembled client-side.
export function sortMovies(movies: MovieEntry[]): MovieEntry[] {
  return [...movies].sort((a, b) => (b.mtime - a.mtime) || a.name.localeCompare(b.name))
}

// One row of the movie list, as `SelectionTable` wants it: a DISPLAY string per column plus the RAW
// value each formatted column sorts by. The table renders what it is handed and never parses it back,
// so "3.4 MB" and a locale date would otherwise sort as text — 900 KB above 1 MB, and months
// alphabetically. Formatters are injected rather than imported so this stays pure and locale-free
// under test; the page passes `formatBytes` and its own date format.
export interface MovieRow {
  name: string       // the file name — the row id, and what the player streams
  label: string      // the display name, or the file name without .mp4
  sizeText: string
  size: number       // bytes — `sizeText`'s sort key
  timeText: string
  mtime: number      // unix seconds — `timeText`'s sort key
  // carried onto the row so the table's actions/filters read one shape rather than looking each
  // movie up again by name
  starred: boolean
  tags: string[]
  tagText: string    // the tags as one string — what the Tags column SORTS by and seeds its editor with
  producedBy: string
  renamed: boolean       // a display name is set — the file name is then worth showing on hover
  configStale: boolean
}
export function movieRows(movies: MovieEntry[],
                          formatSize: (bytes: number) => string,
                          formatTime: (mtime: number) => string): MovieRow[] {
  return movies.map(m => ({
    name: m.name,
    label: movieDisplayName(m),
    sizeText: formatSize(m.size), size: m.size,
    timeText: formatTime(m.mtime), mtime: m.mtime,
    starred: m.starred === true,
    tags: m.tags ?? [],
    tagText: (m.tags ?? []).join(', '),
    producedBy: m.producedBy ?? '',
    renamed: !!(m.displayName ?? '').trim(),
    configStale: m.configStale === true,
  }))
}

/**
 * Rows surviving the active filters — starred-only, and an ANY-of tag/producer selection.
 *
 * Star and tags COMPOSE rather than replace each other (they answer different questions: "the ones I
 * marked" vs "the ones of this kind"), so a row must pass both. Within the tag list it is ANY, not
 * ALL: picking two tags means "either", which is what a chip row of categories reads as.
 * `producedBy` joins the same list — a producer is a category the recorder assigned, and forcing the
 * user to think about which list a chip came from would be a distinction with no purpose.
 */
export function filterMovieRows(rows: MovieRow[], starredOnly: boolean, tags: string[]): MovieRow[] {
  const wanted = new Set(tags)
  return rows.filter(r =>
    (!starredOnly || r.starred) &&
    (!wanted.size || r.tags.some(t => wanted.has(t)) || wanted.has(r.producedBy)))
}

/** Every tag in use, plus every producer present, as one sorted list — the filter's chip options.
 *  Producers come last so the user-authored vocabulary reads first. */
export function movieFilterOptions(movies: MovieEntry[]): { tags: string[]; producers: string[] } {
  const tags = new Set<string>(), producers = new Set<string>()
  for (const m of movies) {
    for (const t of m.tags ?? []) tags.add(t)
    if (m.producedBy) producers.add(m.producedBy)
  }
  return { tags: [...tags].sort(), producers: [...producers].sort() }
}

/** Parse the tag editor's free text into a clean list — comma OR whitespace separated, deduped,
 *  order preserved. Mirrors the backend's `_clean_movie_tags`, which is the one that decides. */
export function parseMovieTags(input: string): string[] {
  const out: string[] = []
  for (const raw of input.split(/[,\n]+/)) {
    const t = raw.trim().replace(/\s+/g, ' ')
    if (t && !out.includes(t)) out.push(t)
  }
  return out
}

export interface Box { w: number; h: number }

// Mouse/centre-anchored zoom for the player's scroll viewport. Given the content box BEFORE and AFTER
// a zoom, the viewport size, the focal point (in viewport coords) and the current scroll, return the
// scroll that keeps the same content point under the focal point — so zooming grows about the cursor
// (or the viewport centre for keyboard zoom), not the top-left corner. Handles the `margin:auto`
// centring offset that applies while the content is smaller than the viewport (no scroll in that axis).
// Pure → unit-tested (movies.test.ts). Mirrors the intent of the plot canvas's zoom but with real
// scrollbars (layout-based box, not a CSS transform).
export function anchoredScroll(
  before: Box, after: Box, vp: { w: number; h: number },
  focal: { x: number; y: number }, scroll: { left: number; top: number },
): { left: number; top: number } {
  const axis = (b: number, a: number, v: number, f: number, s: number): number => {
    if (a <= v) return 0                                   // fits after zoom → centred, no scroll
    const off0 = b <= v ? (v - b) / 2 : -s                 // viewport-x of content pixel 0, before
    const frac = b > 0 ? (f - off0) / b : 0.5              // content fraction under the focal point
    return Math.max(0, Math.min(a - v, frac * a - f))      // keep that fraction under the focal point
  }
  return {
    left: axis(before.w, after.w, vp.w, focal.x, scroll.left),
    top:  axis(before.h, after.h, vp.h, focal.y, scroll.top),
  }
}
