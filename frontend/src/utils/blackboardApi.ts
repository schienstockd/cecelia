// Typed HTTP wrappers for /api/blackboard/*. Same pattern as kiwiCaptures.ts (kept small, no framework
// wrapper): every function catches its own network error and returns a defaulted value so the caller
// can render "no entries" / "not found" without a try/catch of its own. Backend contract is in
// api/src/blackboard_api.jl and the plan is docs/todo/BIDIR_CONTEXT_PLAN.md → Part 4.

/** PROJECT_MEMORY_PLAN Decision 3 — `status ∈ (open | resolved | parked)` on every entry, missing
 *  backfills as "open" server-side; kept as a literal union so a bad enum value gets caught at compile
 *  time on both the list and set paths. */
export type BlackboardStatus = 'open' | 'resolved' | 'parked'
export const BLACKBOARD_STATUSES: readonly BlackboardStatus[] = ['open', 'resolved', 'parked'] as const
function parseStatus(v: unknown): BlackboardStatus {
  return v === 'resolved' || v === 'parked' ? v : 'open'
}

/** PROJECT_MEMORY_PLAN Decision 11 — outcome tag on an entry. Absent on the wire ⇒ untagged ("no
 *  signal", not "neutral"); a returned dict here is always well-formed (server drops malformed
 *  outcomes on read). Note is REQUIRED on set — a verdict without a reason is useless in a future
 *  briefing (Decision 11 D2). */
export type BlackboardVerdict = 'good' | 'bad'
export interface BlackboardOutcome {
  verdict: BlackboardVerdict
  note: string
  taggedAt: string
}
function parseOutcome(v: unknown): BlackboardOutcome | undefined {
  if (!v || typeof v !== 'object') return undefined
  const o = v as Record<string, unknown>
  const verdict = o.verdict === 'good' || o.verdict === 'bad' ? o.verdict : null
  const note = typeof o.note === 'string' ? o.note : ''
  if (!verdict || !note) return undefined
  return { verdict, note, taggedAt: typeof o.taggedAt === 'string' ? o.taggedAt : '' }
}

/** One row in the entry list — the shape returned by `GET /api/blackboard`. */
export interface BlackboardEntrySummary {
  entryId: string
  title: string
  current: number                // snapshot version the LIVE entry.md reflects (0 = never snapshotted)
  updatedAt: string              // Julia `string(Dates.now())` — ISO-ish, parseable by new Date(...)
  attachmentsCount: number
  status: BlackboardStatus       // D3 — backfilled to "open" if missing on disk
  outcome?: BlackboardOutcome    // D11 — absent when untagged
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
  status: BlackboardStatus
  outcome?: BlackboardOutcome
  /** KIWI_CAPTURE_AND_BLACKBOARD_PLAN Decision 9. Per-ref sidecar for a saved Kiwi turn — absent
   *  on a normal (non-Kiwi) Blackboard entry. Shape mirrors `kiwiTurnSave.KiwiRefsSidecar`; parsed
   *  loosely here so a schema drift on either side degrades gracefully. */
  kiwiRefs?: Record<string, unknown>
}

interface ListResp    { entries?: unknown[] }
interface EntryResp   { entry?: unknown }

function parseSummary(raw: unknown): BlackboardEntrySummary | null {
  if (!raw || typeof raw !== 'object') return null
  const r = raw as Record<string, unknown>
  const entryId = typeof r.entryId === 'string' ? r.entryId : ''
  if (!entryId) return null
  const outcome = parseOutcome(r.outcome)
  return {
    entryId,
    title:            typeof r.title === 'string' ? r.title : '',
    current:          typeof r.current === 'number' ? r.current : 0,
    updatedAt:        typeof r.updatedAt === 'string' ? r.updatedAt : '',
    attachmentsCount: typeof r.attachmentsCount === 'number' ? r.attachmentsCount : 0,
    status:           parseStatus(r.status),
    ...(outcome ? { outcome } : {}),
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
  const outcome = parseOutcome(r.outcome)
  const kiwiRefs = (r.kiwiRefs && typeof r.kiwiRefs === 'object' && !Array.isArray(r.kiwiRefs))
    ? r.kiwiRefs as Record<string, unknown> : undefined
  return {
    entryId,
    title:     typeof r.title === 'string' ? r.title : '',
    content:   typeof r.content === 'string' ? r.content : '',
    current:   typeof r.current === 'number' ? r.current : 0,
    updatedAt: typeof r.updatedAt === 'string' ? r.updatedAt : '',
    versions,
    attachments,
    status:    parseStatus(r.status),
    ...(outcome ? { outcome } : {}),
    ...(kiwiRefs ? { kiwiRefs } : {}),
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

/** POST /api/blackboard/create — returns the new entryId, or '' on failure. `kiwiRefs` is the
 *  optional per-ref sidecar for a saved Kiwi turn (KIWI_CAPTURE_AND_BLACKBOARD_PLAN Decision 9);
 *  a normal Blackboard write omits it. */
export async function createBlackboardEntry(
  projectUid: string, title: string, content: string, attachments: string[] = [],
  opts?: { kiwiRefs?: Record<string, unknown> },
  apiBase = '',
): Promise<string> {
  const body: Record<string, unknown> = { projectUid, title, content, attachments }
  if (opts?.kiwiRefs && Object.keys(opts.kiwiRefs).length > 0) body.kiwiRefs = opts.kiwiRefs
  const r = await postJson(`${apiBase}/api/blackboard/create`, body)
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

/** POST /api/blackboard/status — additive metadata flip; NO snapshot fired. Returns the applied
 *  status on success, or `null` on failure. Server rejects an enum value outside `BLACKBOARD_STATUSES`
 *  with 400; the type union above catches that at compile time on this side. */
export async function setBlackboardStatus(
  projectUid: string, entryId: string, status: BlackboardStatus, apiBase = '',
): Promise<BlackboardStatus | null> {
  const r = await postJson(`${apiBase}/api/blackboard/status`, { projectUid, entryId, status })
  return r && typeof r.status === 'string' ? parseStatus(r.status) : null
}

/** POST /api/blackboard/outcome — additive metadata flip; NO snapshot fired. Note is REQUIRED
 *  (Decision 11 D2). Returns the applied outcome on success, or `null` on failure (empty note ⇒ 400,
 *  bad verdict ⇒ 400, note > 2 KiB ⇒ 400 — keep the note to a couple of sentences). */
export async function setBlackboardOutcome(
  projectUid: string, entryId: string, verdict: BlackboardVerdict, note: string, apiBase = '',
): Promise<BlackboardOutcome | null> {
  const r = await postJson(`${apiBase}/api/blackboard/outcome`,
    { projectUid, entryId, verdict, note })
  return r ? (parseOutcome(r.outcome) ?? null) : null
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
