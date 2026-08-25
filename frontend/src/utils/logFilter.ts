// Console filtering — the pure half of ErrorConsole.vue.
//
// The console started as one stream (the backend's own @info/@warn/@error) with one facet (level).
// It now carries seven producers, and a flat list of nineteen ad-hoc `source:` strings is not a filter
// anybody can use. So there are two facets: LEVEL (unchanged) and GROUP — one chip per *component*,
// not per call site, which is the same "generalise by role, not by widget" rule the rest of the UI
// follows. `source` stays on the row as the fine-grained tag it always was.
//
// Kept out of the SFC because this is exactly the logic worth pinning (see logFilter.test.ts) — and
// because `SERVER_LOG_SOURCES` is a CONTRACT with `app/src/log_stream.jl`, asserted by the
// "log sources agree across languages" testset.

export type LogLevel = 'info' | 'warn' | 'error'

/** The chip axis: one per runtime component that can talk. */
export type LogGroup =
  'app' | 'backend' | 'tasks' | 'viewer' | 'napari' | 'preview' | 'runner' | 'notebooks'

/**
 * Sources the BACKEND stamps on a `server:log` frame — mirrored from `LOG_SOURCES` in
 * `app/src/log_stream.jl`. Keep the two in step: a source Julia can emit with no chip here is a
 * message that arrives and is unreachable.
 */
export const SERVER_LOG_SOURCES = ['backend', 'napari', 'preview', 'runner', 'notebooks'] as const

/**
 * Group definitions, in chip order. `quiet: true` means the group is HIDDEN by default — the child
 * processes are chatty (the napari bridge prints a line per label layer) and nobody wants that in the
 * default view. They are still *captured*, and their errors still show regardless (see `isVisible`),
 * so nothing is lost — it is a default, not a filter on what gets recorded.
 */
export const LOG_GROUPS: { value: LogGroup; label: string; tip: string; quiet?: boolean }[] = [
  { value: 'app',       label: 'App',       tip: 'This browser UI — actions, fetch failures, render errors' },
  { value: 'backend',   label: 'Backend',   tip: 'The Julia server (:8080)' },
  { value: 'tasks',     label: 'Tasks',     tip: 'Task and chain runs' },
  // The VIEWERS, as opposed to the napari process. Its own chip rather than a napari one because the
  // napari half is going: the browser volume viewer is what replaces it, and its diagnostics —
  // which GPU, what geometry, a lost device — are the ones that survive (Dominik, 2026-08-25).
  { value: 'viewer',    label: 'Viewer',    tip: 'Opening images — GPU, geometry, load failures' },
  { value: 'napari',    label: 'Napari',    tip: 'Viewer bridge output (:7655) — errors always show', quiet: true },
  { value: 'preview',   label: 'Preview',   tip: 'Task-preview worker (:7656) — errors always show',  quiet: true },
  { value: 'runner',    label: 'Runner',    tip: 'Detached task runner (:7657) — errors always show', quiet: true },
  { value: 'notebooks', label: 'Notebooks', tip: 'Pluto server (:7660) — errors always show',         quiet: true },
]

/** The groups shown until the user says otherwise. */
export const DEFAULT_GROUPS: LogGroup[] = LOG_GROUPS.filter(g => !g.quiet).map(g => g.value)

/**
 * The chips that existed while the persisted selection was a bare array of group names.
 *
 * Frozen deliberately. A saved array cannot tell "the user turned this off" from "this chip did not
 * exist yet", so a new non-quiet chip would arrive switched OFF for everyone who had ever opened the
 * console — which is how a feature ships and appears not to work. Anything not in this list is new to
 * such a selection and starts on; the newer shape records what was known, so this list never grows.
 */
const V1_GROUPS: readonly LogGroup[] =
  ['app', 'backend', 'tasks', 'napari', 'preview', 'runner', 'notebooks'] as const

/** The persisted shape. The array form is what v1 wrote; `known` is what made it self-describing. */
interface StoredGroups { groups: LogGroup[]; known: LogGroup[] }

/**
 * The chip selection to restore from `raw`, with chips added since it was saved switched on.
 *
 * Pure, so the migration can be asserted rather than discovered by a user whose new chip is silently
 * off. Anything unreadable falls back to the defaults — a corrupt filter should not hide the console.
 */
export function restoreGroups(raw: string | null): LogGroup[] {
  if (!raw) return [...DEFAULT_GROUPS]
  let parsed: unknown
  try { parsed = JSON.parse(raw) } catch { return [...DEFAULT_GROUPS] }

  const stored: StoredGroups | null =
    Array.isArray(parsed) ? { groups: parsed as LogGroup[], known: [...V1_GROUPS] }
    : parsed && typeof parsed === 'object' && Array.isArray((parsed as StoredGroups).groups)
      ? { groups: (parsed as StoredGroups).groups,
          known: (parsed as StoredGroups).known ?? [...V1_GROUPS] }
    : null
  if (!stored) return [...DEFAULT_GROUPS]

  const on = new Set(stored.groups)
  const known = new Set(stored.known)
  for (const g of LOG_GROUPS) if (!g.quiet && !known.has(g.value)) on.add(g.value)
  // Chip order, not selection order: the console renders from LOG_GROUPS anyway, and a stable order
  // makes the stored value comparable between saves.
  return LOG_GROUPS.map(g => g.value).filter(v => on.has(v))
}

/** What to persist for a selection — records the chips that existed, so the next added one can tell
 *  "off" from "new". */
export function storeGroups(groups: LogGroup[]): string {
  return JSON.stringify({ groups, known: LOG_GROUPS.map(g => g.value) } satisfies StoredGroups)
}

// Fine-grained `source` → chip group. Anything unlisted is the UI's own — which is the right default,
// because every one of the nineteen ad-hoc frontend tags ('manageImages', 'gating', 'movies', …) is a
// thing this browser did, and a new one should not need a change here to be reachable.
const GROUP_OF: Record<string, LogGroup> = {
  backend: 'backend', server: 'backend',
  napari: 'napari', viewer: 'viewer',
  preview: 'preview',
  runner: 'runner',
  notebooks: 'notebooks',
  task: 'tasks', tasks: 'tasks', chain: 'tasks',
}

export function logGroup(source?: string): LogGroup {
  return (source && GROUP_OF[source]) || 'app'
}

export interface FilterableEntry {
  level: LogLevel
  message: string
  detail?: string
  source?: string
}

/**
 * Does this entry survive the current filter?
 *
 * The one rule worth stating: **an error is never hidden by a group chip.** Turning a chatty child off
 * means "stop narrating", not "stop telling me when you break" — and a console that can silently
 * withhold a stacktrace is the thing this whole rework exists to stop. Level and search still apply to
 * it, so an explicit `warn`-only or a search still narrows as expected.
 */
export function isVisible(
  e: FilterableEntry,
  opts: { groups: LogGroup[]; level: LogLevel | 'all'; query?: string },
): boolean {
  if (opts.level !== 'all' && e.level !== opts.level) return false
  if (!matchesQuery(e, opts.query)) return false
  return e.level === 'error' || opts.groups.includes(logGroup(e.source))
}

/** Case-insensitive substring over the row AND its detail — a stacktrace is where the file name is. */
export function matchesQuery(e: FilterableEntry, query?: string): boolean {
  const q = (query ?? '').trim().toLowerCase()
  if (!q) return true
  return e.message.toLowerCase().includes(q)
    || (e.source ?? '').toLowerCase().includes(q)
    || (e.detail ?? '').toLowerCase().includes(q)
}

/**
 * Missing `seq` numbers between two live frames — the frames the WS dropped.
 *
 * `broadcast_ws` drops a frame for a client whose queue is full rather than block a worker thread, and
 * log lines (unlike task frames) had no way to notice. With a monotonic `seq` the gap is arithmetic:
 * anything other than `last + 1` means lines exist that this client never got, and it can ask for them
 * (`GET /api/logs/recent?since=last`). Returns `null` when there is nothing to fetch — first frame,
 * a contiguous one, or a backend that restarted and began counting again (seq went backwards, in which
 * case the ring is a different one and re-fetching from `last` would be meaningless).
 */
export function gapBefore(seq: number, last: number): number | null {
  if (!Number.isFinite(seq) || last <= 0) return null
  if (seq <= last) return null            // duplicate, or a restarted backend's fresh counter
  return seq === last + 1 ? null : last
}

/** One entry as a copyable text line — the format the Copy button writes. */
export function formatEntry(e: FilterableEntry & { timestamp?: Date }): string {
  const ts = e.timestamp ? e.timestamp.toTimeString().slice(0, 8) : ''
  const head = [ts, e.level.toUpperCase(), e.source ? `[${e.source}]` : '', e.message]
    .filter(Boolean).join(' ')
  return e.detail ? `${head}\n${e.detail}` : head
}
