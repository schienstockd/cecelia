// The whiteboard's "From history…" dialog: what the backend suggests (`GET /api/chains/suggest`,
// app/src/tasks/chain/from_history.jl) and the pure steps between that and a chain on the canvas.
// Design: docs/todo/CHAIN_FROM_HISTORY_PLAN.md, Part B.

import type { ChainTemplate } from '../tasks/types'

export interface HistoryFinal { id: string; fn: string; output: string; imageUids: string[]; kept: boolean }
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
