// Typed HTTP wrappers for /api/blackboard/*. Same pattern as kiwiCaptures.ts (kept small, no framework
// wrapper): every function catches its own network error and returns a defaulted value so the caller
// can render "no entries" / "not found" without a try/catch of its own. Backend contract is in
// api/src/blackboard_api.jl and the plan is docs/todo/BIDIR_CONTEXT_PLAN.md → Part 4.

/** One row in the entry list — the shape returned by `GET /api/blackboard`. */
export interface BlackboardEntrySummary {
  entryId: string
  title: string
  current: number                // snapshot version the LIVE entry.md reflects (0 = never snapshotted)
  updatedAt: string              // Julia `string(Dates.now())` — ISO-ish, parseable by new Date(...)
  attachmentsCount: number
}

/** One full entry — the shape returned by `GET /api/blackboard/entry`. `content` is the LIVE entry.md
 *  unless a `version` was requested, in which case it is that snapshot; the other fields always
 *  describe the LIVE state. */
export interface BlackboardEntry {
  entryId: string
  title: string
  content: string
  current: number
  updatedAt: string
  versions: number[]             // sorted ascending
  attachments: string[]          // captureIds — resolvable via kiwiCaptures.fetchCaptureFrame
}

interface ListResp    { entries?: unknown[] }
interface EntryResp   { entry?: unknown }

function parseSummary(raw: unknown): BlackboardEntrySummary | null {
  if (!raw || typeof raw !== 'object') return null
  const r = raw as Record<string, unknown>
  const entryId = typeof r.entryId === 'string' ? r.entryId : ''
  if (!entryId) return null
  return {
    entryId,
    title:            typeof r.title === 'string' ? r.title : '',
    current:          typeof r.current === 'number' ? r.current : 0,
    updatedAt:        typeof r.updatedAt === 'string' ? r.updatedAt : '',
    attachmentsCount: typeof r.attachmentsCount === 'number' ? r.attachmentsCount : 0,
  }
}

function parseEntry(raw: unknown): BlackboardEntry | null {
  if (!raw || typeof raw !== 'object') return null
  const r = raw as Record<string, unknown>
  const entryId = typeof r.entryId === 'string' ? r.entryId : ''
  if (!entryId) return null
  const versions = Array.isArray(r.versions)
    ? (r.versions as unknown[]).filter((v): v is number => typeof v === 'number')
    : []
  const attachments = Array.isArray(r.attachments)
    ? (r.attachments as unknown[]).filter((v): v is string => typeof v === 'string')
    : []
  return {
    entryId,
    title:     typeof r.title === 'string' ? r.title : '',
    content:   typeof r.content === 'string' ? r.content : '',
    current:   typeof r.current === 'number' ? r.current : 0,
    updatedAt: typeof r.updatedAt === 'string' ? r.updatedAt : '',
    versions,
    attachments,
  }
}

/** GET /api/blackboard?projectUid=… — newest-first. Failure ⇒ []. */
export async function listBlackboardEntries(
  projectUid: string, apiBase = '',
): Promise<BlackboardEntrySummary[]> {
  if (!projectUid) return []
  try {
    const res = await fetch(`${apiBase}/api/blackboard?projectUid=${encodeURIComponent(projectUid)}`)
    if (!res.ok) return []
    const json = await res.json() as ListResp
    return (json.entries ?? []).map(parseSummary).filter((e): e is BlackboardEntrySummary => e !== null)
  } catch { return [] }
}

/** GET /api/blackboard/entry — pass `version` to read a historical snapshot; leave off for LIVE. */
export async function getBlackboardEntry(
  projectUid: string, entryId: string, version?: number, apiBase = '',
): Promise<BlackboardEntry | null> {
  if (!projectUid || !entryId) return null
  try {
    let url = `${apiBase}/api/blackboard/entry?projectUid=${encodeURIComponent(projectUid)}`
            + `&entryId=${encodeURIComponent(entryId)}`
    if (typeof version === 'number') url += `&version=${version}`
    const res = await fetch(url)
    if (!res.ok) return null
    const json = await res.json() as EntryResp
    return parseEntry(json.entry)
  } catch { return null }
}

async function postJson(url: string, body: Record<string, unknown>): Promise<Record<string, unknown> | null> {
  try {
    const res = await fetch(url, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(body),
    })
    if (!res.ok) return null
    return await res.json() as Record<string, unknown>
  } catch { return null }
}

/** POST /api/blackboard/create — returns the new entryId, or '' on failure. */
export async function createBlackboardEntry(
  projectUid: string, title: string, content: string, attachments: string[] = [], apiBase = '',
): Promise<string> {
  const r = await postJson(`${apiBase}/api/blackboard/create`, { projectUid, title, content, attachments })
  return r && typeof r.entryId === 'string' ? r.entryId : ''
}

/** POST /api/blackboard/revise — server snapshots CURRENT first. Returns the new snapshot version, or 0.
 *  Pass `attachments = undefined` to keep the existing list; pass `[]` to explicitly clear. */
export async function reviseBlackboardEntry(
  projectUid: string, entryId: string, content: string, attachments?: string[], apiBase = '',
): Promise<number> {
  const body: Record<string, unknown> = { projectUid, entryId, content }
  if (attachments !== undefined) body.attachments = attachments
  const r = await postJson(`${apiBase}/api/blackboard/revise`, body)
  return r && typeof r.version === 'number' ? r.version : 0
}

/** POST /api/blackboard/restore — snapshots current, then replaces with `version`. Returns `version` on success. */
export async function restoreBlackboardEntry(
  projectUid: string, entryId: string, version: number, apiBase = '',
): Promise<number> {
  const r = await postJson(`${apiBase}/api/blackboard/restore`, { projectUid, entryId, version })
  return r && typeof r.version === 'number' ? r.version : 0
}

/** POST /api/blackboard/prune — keep the N most-recent snapshots. Returns count actually pruned. */
export async function pruneBlackboardEntry(
  projectUid: string, entryId: string, keep: number, apiBase = '',
): Promise<number> {
  const r = await postJson(`${apiBase}/api/blackboard/prune`, { projectUid, entryId, keep })
  return r && typeof r.pruned === 'number' ? r.pruned : 0
}

/** POST /api/blackboard/delete — idempotent. Returns true when a directory was actually removed. */
export async function deleteBlackboardEntry(
  projectUid: string, entryId: string, apiBase = '',
): Promise<boolean> {
  const r = await postJson(`${apiBase}/api/blackboard/delete`, { projectUid, entryId })
  return r?.deleted === true
}

/** Relative "how long ago" — matches `kiwiCaptures.formatWhen` (kept local to avoid a cross-util
 *  dependency in case the two evolve). */
export function formatWhen(iso: string, now: Date = new Date()): string {
  if (!iso) return ''
  const then = new Date(iso)
  const ms = now.getTime() - then.getTime()
  if (!Number.isFinite(ms) || ms < 0) return ''
  if (ms < 60_000)         return 'just now'
  if (ms < 60 * 60_000)    return `${Math.floor(ms / 60_000)}m`
  if (ms < 24 * 3_600_000) return `${Math.floor(ms / 3_600_000)}h`
  return `${Math.floor(ms / 86_400_000)}d`
}
