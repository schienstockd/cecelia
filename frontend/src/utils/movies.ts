// Movie-player (/movies) pure helpers — kept out of the .vue SFC so they're unit-testable. The player
// lists the project's rendered .mp4s (GET /api/movies) and streams them via the range-capable serve
// route (GET /api/movies/file). See api/src/server.jl → try_serve_movie / api_movies_list.

// The ONE filename-sanitiser, shared with the recorder's own filename preview — a movie name has to be
// taken apart here exactly the way it was put together there (see resolveMovieImageUid).
import { safeNamePart } from './batchMovie'

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
  imageUid?: string      // which image it was recorded from; '' when the registry cannot say
  channels?: string[]    // the channels the movie SHOWS (recorder-banked, image order)
  suffix?: string        // the user's "name" addition for this recording, RAW (not the `_sanitised`
                         // fragment in the filename). Recorder-banked so the next recording can offer
                         // it back — it is not recoverable from the filename, which also carries uid
                         // and attribute parts with no marker saying which is which.
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

// ── Joining a movie back to its image ─────────────────────────────────────────
// So the list can show an image's channels and attributes beside its movies (the Details columns).

/** What this needs from an image — structurally a subset of `CciaImage`, declared here so the module
 *  stays pure and independent of the project store. */
export interface MovieImage {
  uid: string
  name: string
  channelNames?: string[]
  attr?: Record<string, string>
}

/** `safeNamePart(s)` split into the `_`-separated tokens a movie filename is assembled from. */
const nameTokens = (s: string): string[] => safeNamePart(s).split('_').filter(Boolean)

/** Does `hay` contain `needle` as a contiguous run of tokens? */
function containsRun(hay: string[], needle: string[]): boolean {
  if (!needle.length || needle.length > hay.length) return false
  for (let i = 0; i + needle.length <= hay.length; i++)
    if (needle.every((t, j) => hay[i + j] === t)) return true
  return false
}

/**
 * Which image a movie is of: the uid the registry banked, else the one its FILENAME names.
 *
 * The fallback carries every movie recorded before the registry knew — which for a batch is all of
 * them, since a batch's saved config lists the whole selection rather than the one image each file is
 * (`_entry_image_uid`, api/src/movies_api.jl). Two shapes have to be recognised because the recorders
 * name files differently: a batch terminates with the uid (`_movie_basename`), the viewer STARTS with
 * the image name (`_movie_named_path`), and either can carry attribute parts and a user suffix around
 * it. So: an exact uid token wins; otherwise the image whose sanitised name appears as a run of tokens.
 *
 * Ambiguity resolves to `''` rather than a guess — two images CAN share a name, and labelling a movie
 * with the wrong one's attributes is worse than labelling it with none. The longest name match wins
 * first, so "cell" doesn't beat "cell_2" on a file that names the latter.
 */
export function resolveMovieImageUid(fileName: string, bankedUid: string,
                                     images: MovieImage[]): string {
  const known = new Set(images.map(i => i.uid))
  if (bankedUid && known.has(bankedUid)) return bankedUid
  const tokens = nameTokens(fileName.replace(/\.mp4$/i, ''))
  const byUid = tokens.find(t => known.has(t))
  if (byUid) return byUid
  let best: MovieImage[] = [], bestLen = 0
  for (const img of images) {
    const parts = nameTokens(img.name)
    if (!parts.length || !containsRun(tokens, parts)) continue
    if (parts.length > bestLen) { best = [img]; bestLen = parts.length }
    else if (parts.length === bestLen) best.push(img)
  }
  // a banked uid naming an image that has since been deleted is still the truthful answer
  return best.length === 1 ? best[0].uid : bankedUid
}

/** Which list fills the channel columns: the image's own channels, or the ones the movie shows. */
export type MovieChannelMode = 'image' | 'movie'
export const MOVIE_CHANNEL_MODES: MovieChannelMode[] = ['image', 'movie']

/**
 * The channel columns for one row — `count` slots, so the Nth column means the same thing on every row.
 *
 * In `'movie'` mode a slot is filled only when the movie actually shows that channel, which is what
 * makes the columns comparable: reading down column 2 answers "which of these movies has CD8 in it".
 * A shown channel that matches no slot (the image's channel names were edited after the recording) is
 * appended past the image's own, so nothing the registry banked is silently dropped.
 */
export function movieChannelCells(imageChannels: string[], movieChannels: string[],
                                  count: number, mode: MovieChannelMode): string[] {
  if (mode === 'image')
    return Array.from({ length: count }, (_, i) => imageChannels[i] ?? '')
  const shown = new Set(movieChannels.map(_chKey))
  const cells = Array.from({ length: count }, (_, i) =>
    i < imageChannels.length && shown.has(_chKey(imageChannels[i])) ? imageChannels[i] : '')
  let next = imageChannels.length
  for (const c of _unmatchedChannels(imageChannels, movieChannels))
    if (next < count) cells[next++] = c
  return cells
}

// Channel names are matched leniently (trimmed, case-insensitive): the recorder banks what the napari
// layer was called, and that has been through a round trip the image's own list has not.
const _chKey = (s: string): string => s.trim().toLowerCase()
const _unmatchedChannels = (imageChannels: string[], movieChannels: string[]): string[] =>
  movieChannels.filter(c => !imageChannels.some(ic => _chKey(ic) === _chKey(c)))

/** How many channel slots the table needs — the widest row, in the mode being shown. */
export function movieChannelCount(rows: Array<{ imageChannels: string[]; movieChannels: string[] }>,
                                  mode: MovieChannelMode): number {
  return rows.reduce((n, r) => Math.max(n, mode === 'image' ? r.imageChannels.length
    : Math.max(r.imageChannels.length + _unmatchedChannels(r.imageChannels, r.movieChannels).length,
               r.movieChannels.length)), 0)
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
  // A saved generation config exists, and which page can edit it (Phase 6 — utils/movieRestore.ts).
  // Carried on the row so the table can offer the action without a second lookup per row; '' means the
  // movie predates the registry, which is most of them in an older project.
  hasConfig: boolean
  configKind: string
  // ── the image this movie is of, joined in so the Details columns need no second lookup per row.
  // Empty/blank when it can't be identified, which is what an older project's movies look like.
  imageUid: string
  imageName: string
  imageChannels: string[]   // the image's own channel names, in order
  movieChannels: string[]   // the channels the movie SHOWS (recorder-banked)
  attr: Record<string, string>
  // Each attribute ALSO flattened as `attr:<key>`, because `SelectionTable` sorts by reading the sort
  // key straight off the row (`sortRows`), so a column's value has to be a top-level field. Missing on
  // a row whose image has no such attribute — `sortRows` puts blanks last either way.
  [key: `attr:${string}`]: string
}
export function movieRows(movies: MovieEntry[],
                          formatSize: (bytes: number) => string,
                          formatTime: (mtime: number) => string,
                          images: MovieImage[] = []): MovieRow[] {
  const byUid = new Map(images.map(i => [i.uid, i]))
  return movies.map(m => {
    const uid = resolveMovieImageUid(m.name, m.imageUid ?? '', images)
    const img = byUid.get(uid)
    const attr = img?.attr ?? {}
    return {
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
      hasConfig: m.hasConfig === true,
      configKind: m.configKind ?? '',
      imageUid: uid,
      imageName: img?.name ?? '',
      imageChannels: img?.channelNames ?? [],
      movieChannels: m.channels ?? [],
      attr,
      ...Object.fromEntries(Object.entries(attr).map(([k, v]) => [`attr:${k}`, v])),
    }
  })
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

/**
 * The distinct `name` suffixes already used in this project, for the recorder's suggestion list.
 *
 * Registry-backed, deliberately: the suffix sits in the filename beside uid and attribute parts with
 * nothing marking where one ends, so recovering it by parsing would mean encoding three recorders'
 * naming conventions in a fourth place — and a wrong guess offers nonsense. Movies recorded before the
 * field existed simply contribute nothing.
 */
export function movieSuffixesInUse(movies: MovieEntry[]): string[] {
  const out = new Set<string>()
  for (const m of movies) {
    const s = (m.suffix ?? '').trim()
    if (s) out.add(s)
  }
  return [...out].sort()
}
