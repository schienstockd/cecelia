// Kiwi turns — the pure half of the cockpit's prompt box + claims feed (KIWI_ASSISTANT_PLAN Phase 4).
//
// A turn is one question → one validated, structured reply (`api/src/kiwi_api.jl` runs it; the reply
// shape is `kiwi_reply_schema` in `api/src/kiwi_turn.jl`). This file holds what the SFCs render from
// and what a chip click does, as data, so both are tested without mounting anything:
//
//   refKey / refLabel      identity + a readable fallback label for any KiwiRef
//   chipState              Decision 10 — a chip shows HOW FAR it was checked, never more
//   pointTarget            what clicking a ref does (open the viewer, a page, a capture, a mark)
//   draft helpers          the attached-refs list the prompt box sends
//   API                    start / cancel / list / clear
//
// The composable that PERFORMS a pointTarget (stores, router, pop-out) is `composables/useKiwiPoint.ts`.

import type { KiwiRef, KiwiRefResult } from './kiwiRef'
import { svcPost } from './serviceApi'
import { formatTaskDuration } from './taskElapsed'

export type KiwiClaimKind = 'observation' | 'interpretation' | 'question'

/** One ref on a claim, as validated by the backend: resolved (`result`) and seen this turn. */
export interface KiwiClaimRef { ref: KiwiRef; result: KiwiRefResult; seen?: boolean }
export interface KiwiClaim { kind: KiwiClaimKind; text: string; refs: KiwiClaimRef[] }

export interface KiwiReply {
  ok: boolean
  abstain: boolean
  claims: KiwiClaim[]
  errors: string[]
  reasked: boolean
  reasoning: string
  usage: { input: number; output: number }
  toolCalls: number
  seconds: number
  /** one line that is not a claim — what Kiwi couldn't look at, or needs from the user */
  note?: string
  /** how Kiwi WROTE that its re-ask didn't fix (a bundled claim) — kept for the record, not shown */
  shapeErrors?: string[]
  /** the engine session — what a follow-up continues */
  sessionId?: string
}

export type KiwiTurnStatus = 'running' | 'done' | 'failed' | 'cancelled'

export interface KiwiTurn {
  turnId: string
  projectUid: string
  prompt: string
  refs: { ref: KiwiRef; result: KiwiRefResult }[]
  reasoning: boolean
  model: string
  engine: string
  status: KiwiTurnStatus
  startedAt: string
  finishedAt?: string
  steps: string[]
  reply?: KiwiReply | null
  error?: string
  /** the turn this one follows up (same engine session) */
  followUp?: string
}

// ── Identity + labels ──────────────────────────────────────────────────────────────────────────────

/** Key-order-independent identity of a ref — so attaching the same thing twice is a no-op. */
export function refKey(ref: KiwiRef): string {
  const r = ref as unknown as Record<string, unknown>
  return JSON.stringify(Object.keys(r).sort().map(k => [k, r[k]]))
}

function ids(xs: number[]): string {
  return xs.length <= 3 ? xs.join(', ') : `${xs.slice(0, 3).join(', ')} +${xs.length - 3}`
}

/** A readable label from the ref alone — shown until (or instead of) the resolver's label. */
export function refLabel(ref: KiwiRef): string {
  switch (ref.kind) {
    case 'project':    return 'this project'
    case 'set':        return `set ${ref.setUid}`
    case 'image':      return `image ${ref.imageUid}`
    case 'population': return `${ref.popPath} · ${ref.valueName}`
    case 'cells':      return `cell${ref.labelIds.length > 1 ? 's' : ''} ${ids(ref.labelIds)} · ${ref.valueName}`
    case 'tracks':     return `track${ref.trackIds.length > 1 ? 's' : ''} ${ids(ref.trackIds)} · ${ref.valueName}`
    case 'viewer': {
      const tz = [ref.t != null ? `t=${ref.t}` : '', ref.z != null ? `z=${ref.z}` : ''].filter(Boolean).join(' ')
      return `viewer ${ref.imageUid}${tz ? ' ' + tz : ''}`
    }
    case 'plot':       return 'plot'
    case 'tile':       return `tile ${ref.cellId}`
    case 'capture':    return 'capture'
    case 'task':       return ref.funName
    case 'ui':         return ref.anchor.startsWith('nav:') ? ref.anchor.slice(4) : ref.anchor
    case 'blackboard': return `note ${ref.entryId}${ref.version != null ? ` v${ref.version}` : ''}`
    case 'proposedPlot': return [ref.measure ?? ref.plot, ...(ref.pops ?? [])].join(' · ')
  }
}

/** The kind as a chip or row shows it — the schema's name, except where that reads as code. A proposed
 *  plot that is already on a board is just a plot (the resolver says so in `detail`). */
export function kindLabel(kind: KiwiRef['kind'], result?: KiwiRefResult): string {
  if (kind !== 'proposedPlot') return kind
  return onBoard(result) ? 'plot' : 'plot this'
}

const onBoard = (r?: KiwiRefResult) => !!r?.ok && !!r.detail?.startsWith('already on board')

// ── Decision 10: a chip shows how far it was checked ───────────────────────────────────────────────

export type ChipTone = 'ok' | 'soft' | 'fail'

/** Tone + one-line tooltip for a ref chip. `ok` = found on disk; `soft` = true only while something is
 *  open (a live plot / landscape) or only checkable in the browser (a UI place); `fail` = it doesn't
 *  resolve, or the engine cited it without having seen it this turn. No state says "supports the
 *  claim" — nothing checks that yet (Open decision 5), and a chip must not look as if it did. */
export function chipState(result: KiwiRefResult | undefined, seen?: boolean): { tone: ChipTone; tip: string } {
  if (!result) return { tone: 'soft', tip: 'Not checked yet' }
  if (!result.ok) return { tone: 'fail', tip: result.error || 'Not found' }
  if (seen === false) return { tone: 'fail', tip: 'Cited without being looked at this turn' }
  if (result.check === 'live') return { tone: 'soft', tip: 'Open now — gone when it closes' }
  if (result.check === 'format') return { tone: 'soft', tip: 'A place in the app — checked when shown' }
  if (result.check === 'proposal') {
    return onBoard(result) ? { tone: 'ok', tip: `${result.detail!.replace(/^already on/, 'On')} — click to open it` }
                           : { tone: 'ok', tip: 'Not plotted yet — click to plot it' }
  }
  return { tone: 'ok', tip: 'Exists — not checked against the claim' }
}

// ── What a click does ──────────────────────────────────────────────────────────────────────────────

/** A task category's page, and the TaskRunner `module` key that remembers its selected function
 *  (`cc-fn:<module>` in localStorage). Categories not listed open under `/custom/<category>`. */
export const TASK_PAGES: Record<string, { path: string; module: string }> = {
  importImages:    { path: '/manage-images',  module: 'manageImages' },
  exportImages:    { path: '/manage-images',  module: 'manageImages' },
  editImages:      { path: '/preprocess',     module: 'preprocess' },
  cleanupImages:   { path: '/cleanup',        module: 'cleanup' },
  opticalFlow:     { path: '/model-training', module: 'opticalFlow' },
  segment:         { path: '/segment',        module: 'segment' },
  tracking:        { path: '/track',          module: 'tracking' },
  behaviour:       { path: '/behaviour',      module: 'behaviour' },
  clustPops:       { path: '/clust-cells',    module: 'clustPops' },
  clustTracks:     { path: '/clust-tracks',   module: 'clustTracks' },
  clustRegions:    { path: '/regions',        module: 'clustRegions' },
  spatialAnalysis: { path: '/spatial',        module: 'spatialAnalysis' },
}

export type PointTarget =
  | { action: 'viewer'; imageUid: string; t?: number; z?: number;
      tracks?: { valueName: string; ids: number[] }; cells?: { valueName: string; ids: number[] } }
  | { action: 'set'; setUid: string }
  | { action: 'route'; path: string; query?: Record<string, string>; rememberFn?: { module: string; task: string } }
  | { action: 'capture'; captureId: string }
  | { action: 'plot'; plotId: string; u?: number; v?: number }
  | { action: 'ui'; anchor: string }
  // a plot nobody has made: open the board that holds it, else add one (`POST /api/kiwi/plot/open`)
  | { action: 'proposedPlot' }
  | { action: 'none'; why: string }

/** What clicking this ref should do. Pure: the composable performs it. */
export function pointTarget(ref: KiwiRef): PointTarget {
  switch (ref.kind) {
    case 'project':    return { action: 'route', path: '/manage-images' }
    case 'set':        return { action: 'set', setUid: ref.setUid }
    case 'image':      return { action: 'viewer', imageUid: ref.imageUid }
    case 'viewer':     return { action: 'viewer', imageUid: ref.imageUid,
                                ...(ref.t != null ? { t: ref.t } : {}), ...(ref.z != null ? { z: ref.z } : {}) }
    case 'tracks':     return { action: 'viewer', imageUid: ref.imageUid, tracks: { valueName: ref.valueName, ids: ref.trackIds } }
    case 'cells':      return { action: 'viewer', imageUid: ref.imageUid, cells: { valueName: ref.valueName, ids: ref.labelIds } }
    // A population reference is too general to point at — the "right" landing depends on why you'd
    // want it (gate definition, plots that use it, or its cells in the viewer). Opening the viewer
    // with cells highlighted was misleading: it looked like "here is that population" when it was
    // really a transient PickHighlight over the raw cells, not the gate. No-op instead. `why` is
    // shown as the chip's tooltip (KiwiRefChip disables + tips off `pointTarget.why`).
    case 'population': return { action: 'none', why: 'Reference only — no single place to open' }
    case 'plot':       return { action: 'plot', plotId: ref.plotId,
                                ...(ref.u != null ? { u: ref.u } : {}), ...(ref.v != null ? { v: ref.v } : {}) }
    case 'capture':    return { action: 'capture', captureId: ref.captureId }
    case 'ui':         return { action: 'ui', anchor: ref.anchor }
    case 'blackboard': return { action: 'route', path: '/blackboard', query: { entry: ref.entryId } }
    case 'task': {
      const [category, ...rest] = ref.funName.split('.')
      const task = rest.join('.')
      const page = TASK_PAGES[category]
      if (page) return { action: 'route', path: page.path, rememberFn: { module: page.module, task } }
      return task ? { action: 'route', path: `/custom/${category}` } : { action: 'none', why: 'Unknown task' }
    }
    case 'tile':       return { action: 'none', why: 'Tile — nothing to open yet' }
    case 'proposedPlot': return { action: 'proposedPlot' }
  }
}

// ── The draft (attached refs) ──────────────────────────────────────────────────────────────────────

export interface KiwiDraft { projectUid: string; refs: KiwiRef[] }

/** Attach `ref` unless it's already there (by `refKey`). A draft for another project starts over. */
export function draftAdd(d: KiwiDraft, projectUid: string, ref: KiwiRef): KiwiDraft {
  const base = d.projectUid === projectUid ? d.refs : []
  const k = refKey(ref)
  return { projectUid, refs: base.some(r => refKey(r) === k) ? base : [...base, ref] }
}

export function draftRemove(d: KiwiDraft, ref: KiwiRef): KiwiDraft {
  const k = refKey(ref)
  return { ...d, refs: d.refs.filter(r => refKey(r) !== k) }
}

/** Parse a stored draft; anything malformed is an empty draft, never a throw. */
export function parseDraft(raw: string | null): KiwiDraft {
  try {
    const v = JSON.parse(raw ?? '')
    if (v && typeof v.projectUid === 'string' && Array.isArray(v.refs)) return { projectUid: v.projectUid, refs: v.refs }
  } catch { /* fall through */ }
  return { projectUid: '', refs: [] }
}

// ── Turn record helpers ────────────────────────────────────────────────────────────────────────────

/** Replace-or-append a turn by id, keeping the list in start order (the feed renders newest first). */
export function upsertTurn(turns: KiwiTurn[], t: KiwiTurn): KiwiTurn[] {
  const i = turns.findIndex(x => x.turnId === t.turnId)
  if (i < 0) return [...turns, t]
  const next = turns.slice(); next[i] = t; return next
}

/** A step as the user reads it: "get_populations" → "get populations". */
export function stepLabel(step: string): string {
  return step.replace(/_/g, ' ')
}

/** `10468` → `10.5k`. */
function kTokens(n: number): string {
  return n < 1000 ? String(n) : `${(n / 1000).toFixed(1)}k`
}

/** One muted line under a finished turn: time, tool calls, a re-ask, the model and what it wrote.
 *  Output tokens only — the CLI reports input net of its cache, so that number reads as ~0. */
export function turnMeta(t: KiwiTurn): string {
  const r = t.reply
  const parts: string[] = []
  if (r) {
    parts.push(formatTaskDuration(r.seconds * 1000))
    parts.push(`${r.toolCalls} look${r.toolCalls === 1 ? '' : 's'}`)
    if (r.reasked) parts.push('re-asked')
  }
  if (t.model) parts.push(t.model)
  if (r?.usage?.output) parts.push(`${kTokens(r.usage.output)} tokens`)
  if (t.reasoning) parts.push('thought first')
  return parts.join(' · ')
}

/** A claim's text as shown. The flag says "I think"; a reply stored before the backend trimmed a
 *  written-out one would otherwise read "I think I think". */
export function claimText(c: KiwiClaim): string {
  return c.kind === 'interpretation' ? c.text.replace(/^I think(?: that)?,?\s+/i, '').replace(/^./, m => m.toUpperCase()) : c.text
}

/** A validation error as the user reads it: the claim number and the reason, not the raw ref JSON
 *  the re-ask prompt needed. */
export function plainError(e: string): string {
  return e.replace(/^claim (\d+)/, 'Claim $1')
          .replace(/: ref \{.*?\} — /, ': ')
          .replace(/: ref \{.*?\}(?= was )/, ':')
          .replace(/ was not in any tool result or attachment this turn$/, ' cites something Kiwi didn’t look at')
          .replace(/ — split it, one fact per claim$/, '')
}

// ── Claims as table rows ───────────────────────────────────────────────────────────────────────────

export interface ClaimRow { id: string; n: number; kind: KiwiClaimKind; text: string; refs: KiwiClaimRef[]
                          failed: boolean; tip: string }

/** One row per claim, numbered as the validation errors number them (1-based). `failed` when one of
 *  its refs doesn't resolve or wasn't looked at; `tip` says which, in plain words. How Kiwi WROTE the
 *  claim (`shapeErrors` — too long, two facts) is not shown: it says nothing about what it cites. */
export function claimRows(reply: KiwiReply): ClaimRow[] {
  const shape = new Set(reply.shapeErrors ?? [])
  return reply.claims.map((c, i) => {
    const n = i + 1
    const failed = c.refs.some(r => chipState(r.result, r.seen).tone === 'fail')
    const own = new RegExp(`^claim ${n}\\b`)
    const tip = failed ? reply.errors.filter(e => !shape.has(e) && own.test(e)).map(plainError).join('\n') : ''
    return { id: String(n), n, kind: c.kind, text: claimText(c), refs: c.refs, failed, tip }
  })
}

// ── Attachments (the rows above the prompt box) ────────────────────────────────────────────────────

export interface AttachmentRow { id: string; ref: KiwiRef; kind: string; label: string; detail: string; tip: string }

/** One row per attached ref: the resolver's label and detail once it has answered, the ref's own label
 *  until then. The tip carries how far it was checked (Decision 10). */
export function attachmentRows(refs: KiwiRef[], results: Record<string, KiwiRefResult | undefined>): AttachmentRow[] {
  return refs.map(ref => {
    const id = refKey(ref)
    const res = results[id]
    return { id, ref, kind: kindLabel(ref.kind, res),
             label: res?.ok && res.label ? res.label : refLabel(ref),
             detail: res?.ok ? (res.detail ?? '') : (res?.error ?? ''),
             tip: chipState(res).tip }
  })
}

// ── API ────────────────────────────────────────────────────────────────────────────────────────────

export function startKiwiTurn(body: { projectUid: string; prompt: string; refs: KiwiRef[];
                                      reasoning: boolean; model?: string; followUp?: string }): Promise<KiwiTurn> {
  return svcPost('/api/kiwi/turn', body, 15_000)
}

/** "Plot this": the board that holds the proposed plot, added if none does. */
export function openProposedPlot(projectUid: string, ref: KiwiRef): Promise<{ ok: boolean; board: string; created: boolean }> {
  return svcPost('/api/kiwi/plot/open', { projectUid, ref }, 30_000)
}

export async function resolveKiwiRefs(projectUid: string, refs: KiwiRef[]): Promise<KiwiRefResult[]> {
  if (!refs.length) return []
  const r = await svcPost('/api/kiwi/refs/resolve', { projectUid, refs }, 15_000) as { results?: KiwiRefResult[] }
  return r.results ?? []
}

export function cancelKiwiTurn(turnId: string): Promise<unknown> {
  return svcPost('/api/kiwi/turn/cancel', { turnId }, 10_000)
}

export function clearKiwiTurns(projectUid: string): Promise<unknown> {
  return svcPost('/api/kiwi/turns/clear', { projectUid }, 10_000)
}

export async function fetchKiwiTurns(projectUid: string): Promise<{ turns: KiwiTurn[]; running: KiwiTurn | null }> {
  const res = await fetch(`/api/kiwi/turns?projectUid=${encodeURIComponent(projectUid)}`)
  if (!res.ok) return { turns: [], running: null }
  const data = await res.json() as { turns?: KiwiTurn[]; running?: KiwiTurn | null }
  return { turns: data.turns ?? [], running: data.running ?? null }
}

// ── Typed search (the "+" in the prompt box) ───────────────────────────────────────────────────────

export interface RefCandidate { ref: KiwiRef; label: string; hint: string }

/** Rank this project's sets, images and task functions against `query` (case-insensitive substring
 *  of the name or uid; name-prefix matches first). Empty query → nothing. At most `limit`. */
export function searchRefs(query: string,
                           src: { sets: { uid: string; name: string; images: { uid: string; name: string }[] }[];
                                  tasks: { fun_name: string; label: string }[] },
                           limit = 8): RefCandidate[] {
  const q = query.trim().toLowerCase()
  if (!q) return []
  const scored: { c: RefCandidate; score: number }[] = []
  const score = (name: string, id: string): number => {
    const n = name.toLowerCase(), i = id.toLowerCase()
    if (n.startsWith(q) || i === q) return 0
    if (n.includes(q) || i.includes(q)) return 1
    return -1
  }
  for (const s of src.sets) {
    const sc = score(s.name, s.uid)
    if (sc >= 0) scored.push({ score: sc, c: { ref: { kind: 'set', setUid: s.uid }, label: s.name, hint: 'set' } })
    for (const im of s.images) {
      const si = score(im.name, im.uid)
      if (si >= 0) scored.push({ score: si, c: { ref: { kind: 'image', imageUid: im.uid }, label: im.name, hint: `image · ${s.name}` } })
    }
  }
  for (const t of src.tasks) {
    const st = score(t.label, t.fun_name)
    if (st >= 0) scored.push({ score: st, c: { ref: { kind: 'task', funName: t.fun_name }, label: t.label, hint: t.fun_name } })
  }
  return scored.sort((a, b) => a.score - b.score).slice(0, limit).map(x => x.c)
}

// ── What the viewer's "Add to Kiwi" adds ───────────────────────────────────────────────────────────

interface HL { imageUid: string; valueName: string; origin?: 'user' | 'claude' }

/** The viewer's own selection wins over the view: selected tracks → a `tracks` ref, picked cells → a
 *  `cells` ref, else the view itself (`viewer`, with z only in the plane view, where it means the
 *  shown plane). A highlight the assistant painted (`origin: 'claude'`) is not the user's selection,
 *  and one for another image is not this view's. */
export function viewerRefFor(v: { imageUid: string; t: number; z: number; plane: boolean;
                                  tracks?: (HL & { trackIds: number[] }) | null;
                                  cells?: (HL & { labels: number[] }) | null }): { ref: KiwiRef; tip: string } {
  const mine = (h: HL | null | undefined) => !!h && h.imageUid === v.imageUid && h.origin !== 'claude'
  if (mine(v.tracks) && v.tracks!.trackIds.length)
    return { ref: { kind: 'tracks', imageUid: v.imageUid, valueName: v.tracks!.valueName, trackIds: v.tracks!.trackIds },
             tip: `Add the ${v.tracks!.trackIds.length} selected track${v.tracks!.trackIds.length === 1 ? '' : 's'} to Kiwi` }
  if (mine(v.cells) && v.cells!.labels.length)
    return { ref: { kind: 'cells', imageUid: v.imageUid, valueName: v.cells!.valueName, labelIds: v.cells!.labels },
             tip: `Add the ${v.cells!.labels.length} selected cell${v.cells!.labels.length === 1 ? '' : 's'} to Kiwi` }
  return { ref: { kind: 'viewer', imageUid: v.imageUid, t: v.t, ...(v.plane ? { z: v.z } : {}) },
           tip: 'Add this view to Kiwi' }
}
