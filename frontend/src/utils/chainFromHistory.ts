// The whiteboard's "From history…" dialog: what the backend suggests (`GET /api/chains/suggest`,
// app/src/tasks/chain/from_history.jl) and the pure steps between that and a chain on the canvas.
// Design: docs/todo/CHAIN_FROM_HISTORY_PLAN.md, Part B. Also the dialog's Methods parameter-table export.

import type { ChainTemplate } from '../tasks/types'
import { rowsToCsv } from '../plots/export'
import type { RailKind } from '../components/canvas/canvasManager'
import { tkey } from '../plots/series'

export interface HistoryFinal {
  id: string; fn: string; output: string; imageUids: string[]; kept: boolean
  detail: string    // what tells this version from its siblings ('' = the only one)
  multi: boolean    // several versions can run side by side (segmentation, per-population tracking)
  at: string        // its latest run, ISO
}
export interface HistoryConflict { nodeId: string; param: string; values: Record<string, unknown> }
export interface HistorySuggestion {
  template: ChainTemplate
  finalOutputs: HistoryFinal[]
  conflicts: HistoryConflict[]
  notReplayed: { fn: string; imageUid: string; why: string }[]
  reusedModels: { fn: string; model: string }[]
  gateSteps: string[]
  valid: boolean
  error: string
}

/** `keep` undefined = every final output (the backend default); `[]` keeps nothing. */
export function suggestUrl(projectUid: string, imageUids: string[],
                           opts: { keep?: string[]; retrain?: boolean; name?: string } = {}): string {
  const q = new URLSearchParams({ projectUid, imageUids: imageUids.join(',') })
  if (opts.keep) q.set('keep', opts.keep.join(','))
  if (opts.retrain) q.set('retrain', '1')
  if (opts.name) q.set('name', opts.name)
  return `/api/chains/suggest?${q.toString()}`
}

/** One option per DISTINCT value a conflict saw, labelled with the exemplars that used it. */
export function conflictOptions(c: HistoryConflict, nameOf: (uid: string) => string = u => u) {
  const byValue = new Map<string, { value: unknown; images: string[] }>()
  for (const [uid, v] of Object.entries(c.values)) {
    const k = JSON.stringify(v)
    const e = byValue.get(k) ?? byValue.set(k, { value: v, images: [] }).get(k)!
    e.images.push(nameOf(uid))
  }
  return [...byValue.entries()].map(([key, e]) => ({ key, value: e.value, label: `${key} — ${e.images.join(', ')}` }))
}

/**
 * The template with each picked conflict value written into its node. `choices` is
 * nodeId → param → the JSON key from `conflictOptions`; an unpicked conflict keeps the template's value
 * (the latest run). Returns a new template; the input is untouched.
 */
export function applyChoices(tpl: ChainTemplate, choices: Record<string, Record<string, string>>): ChainTemplate {
  return {
    ...tpl,
    nodes: tpl.nodes.map(n => {
      const picked = choices[n.id]
      if (!picked) return n
      const params = { ...(n.params ?? {}) }
      for (const [param, key] of Object.entries(picked)) params[param] = JSON.parse(key)
      return { ...n, params }
    }),
  }
}

/** One step of the dialog: a task, and the versions of it the exemplars ran, newest first. */
export interface StepGroup { fn: string; label: string; multi: boolean; versions: HistoryFinal[] }

export function groupSteps(finals: readonly HistoryFinal[], labelOf: (fn: string) => string = f => f): StepGroup[] {
  const by = new Map<string, HistoryFinal[]>()
  for (const f of finals) (by.get(f.fn) ?? by.set(f.fn, []).get(f.fn)!).push(f)
  return [...by.entries()].map(([fn, vs]) => ({
    fn, label: labelOf(fn), multi: vs.every(v => v.multi),
    versions: [...vs].sort((a, b) => b.at.localeCompare(a.at)),
  }))
}

/**
 * What to keep when the user has not chosen yet: every version of a step that forks (both
 * segmentations, tracking per population), and only the NEWEST of a step whose versions are
 * alternatives — the latest is the one the user ended on.
 */
export function defaultKeep(groups: readonly StepGroup[]): string[] {
  return groups.flatMap(g => g.multi ? g.versions.map(v => v.id) : g.versions.slice(0, 1).map(v => v.id))
}

/** Left-out runs folded per reason: which tasks, on how many images. */
export function groupLeftOut(rows: readonly { fn: string; imageUid: string; why: string }[],
                             labelOf: (fn: string) => string = f => f) {
  const by = new Map<string, { tasks: Set<string>; images: Set<string> }>()
  for (const r of rows) {
    const e = by.get(r.why) ?? by.set(r.why, { tasks: new Set(), images: new Set() }).get(r.why)!
    e.tasks.add(labelOf(r.fn)); e.images.add(r.imageUid)
  }
  return [...by.entries()].map(([why, e]) => ({ why, tasks: [...e.tasks].sort(), images: e.images.size }))
}

/** `base`, or `base 2`, `base 3`… — the first name no existing chain has. */
export function uniqueChainName(base: string, existing: readonly string[]): string {
  const taken = new Set(existing)
  if (!taken.has(base)) return base
  let i = 2
  while (taken.has(`${base} ${i}`)) i++
  return `${base} ${i}`
}

// ── Methods parameter table (`GET /api/chains/history-params`, app/src/tasks/chain/history_params.jl) ──

/** One step as it ran on the selected images — only params that differ from the task's defaults. */
/** A board-scoped row: 'made' = wrote what is shown (or fed a step that did); 'ran on' = the shown cells. */
export type HistoryRelation = 'made' | 'ran on' | ''

export interface HistoryParamsRow {
  step: string; task: string; output: string
  images: string             // "n/N"
  lastRun: string            // ISO, '' when unknown
  changedFromDefault: string // "key=value; …"
  conflicts: string          // "key: a (n) | b (m); …" — the images disagree
  note: string               // why it is not something a chain repeats, or why its params are unknown
  relation?: HistoryRelation // board-scoped only
}

/** What an analysis board shows, for cutting the table to it: population keys + its cluster runs. */
export interface BoardParamsScope { pops: string[]; clusterRuns: string[] }

export function historyParamsUrl(projectUid: string, imageUids: string[], scope?: BoardParamsScope): string {
  const q = new URLSearchParams({ projectUid, imageUids: imageUids.join(',') })
  if (scope) {   // JSON, not a comma list: population names may hold commas
    q.set('pops', JSON.stringify(scope.pops))
    q.set('clusterRuns', JSON.stringify(scope.clusterRuns))
  }
  return `/api/chains/history-params?${q}`
}

/**
 * A board's scope from its filled slots: each population-rail slot's selection, a self-contained view's
 * label set (its root population), and — when a cluster slot is on the board — the board's one cluster
 * run (`popType:suffix`). The host resolves each slot's rail and selection (global vs local scope).
 */
export function boardParamsScope(
  slots: { rail: RailKind; sel: readonly string[]; valueName?: string }[],
  cluster: { popType: string; suffix: string } | null,
): BoardParamsScope {
  const pops = new Set<string>()
  for (const s of slots) {
    if (s.rail === 'pops') s.sel.forEach(k => pops.add(k))
    else if (s.valueName) pops.add(tkey('live', s.valueName, '/'))
  }
  const hasCluster = slots.some(s => s.rail === 'clusterPops')
  return { pops: [...pops], clusterRuns: hasCluster && cluster ? [`${cluster.popType}:${cluster.suffix}`] : [] }
}

/**
 * The table as CSV, columns in a fixed order (the backend's JSON objects carry no key order). A
 * board-scoped table adds `relation` after `output`; the whole-history one has none to say.
 */
export function historyParamsCsv(rows: HistoryParamsRow[]): string {
  const scoped = rows.some(r => r.relation)
  return rowsToCsv(rows.map(r => ({
    step: r.step, task: r.task, output: r.output, ...(scoped ? { relation: r.relation ?? '' } : {}),
    images: r.images, last_run: r.lastRun,
    changed_from_default: r.changedFromDefault, differs_between_images: r.conflicts, note: r.note,
  })))
}
