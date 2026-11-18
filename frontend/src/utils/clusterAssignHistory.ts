// Undo / redo for putting clusters into populations (the population manager's shared cluster strip).
//
// NOT the server's gating history: that one is per image and restores the whole tree, while a cluster
// edit is written to every image of the run (the store's mirrorUids) — undoing it on one image would
// leave the others behind, and a whole-tree restore would also revert renames/additions made since.
// So this history holds only what a click changed — the cluster lists of the 1–2 populations it
// touched — and undo writes those lists back through the same set-wide update.
//
// A step is only replayed if the populations still hold exactly what the step left them with. If
// something else changed them since (Claude's set_cluster_pops, another window), the step is dropped
// rather than overwriting that later change.
//
// Held at module scope, keyed by run, so it survives the manager being closed and reopened; it dies
// with the page, as the gating history dies with the backend.
import { reactive } from 'vue'

/** population path → its cluster IDs (sorted) */
export type Assign = Record<string, number[]>
export interface AssignStep { before: Assign; after: Assign }
export interface AssignHistory { undo: AssignStep[]; redo: AssignStep[] }

export const ASSIGN_HISTORY_LIMIT = 50
const histories = reactive(new Map<string, AssignHistory>())

export function assignHistoryFor(key: string): AssignHistory {
  if (!histories.has(key)) histories.set(key, { undo: [], redo: [] })
  return histories.get(key)!
}

const sorted = (ids: number[]) => [...ids].sort((a, b) => a - b)
export const sameIds = (a: number[] | undefined, b: number[] | undefined) =>
  !!a && !!b && a.length === b.length && sorted(a).every((x, i) => x === sorted(b)[i])

/**
 * The step a strip click makes: `id` goes into `path`, moved off whichever population holds it —
 * or comes out of `path` if it is already there. `current` = every population of the run.
 */
export function toggleStep(current: Assign, path: string, id: number): AssignStep {
  const mine = current[path] ?? []
  if (mine.includes(id))
    return { before: { [path]: mine }, after: { [path]: mine.filter(x => x !== id) } }
  const owner = Object.keys(current).find(p => p !== path && current[p].includes(id))
  const before: Assign = { [path]: mine }, after: Assign = { [path]: sorted([...mine, id]) }
  if (owner) { before[owner] = current[owner]; after[owner] = current[owner].filter(x => x !== id) }
  return { before, after }
}

export function recordStep(h: AssignHistory, step: AssignStep) {
  h.undo.push(step)
  if (h.undo.length > ASSIGN_HISTORY_LIMIT) h.undo.shift()
  h.redo.length = 0
}

/**
 * Pop the next undo (or redo) step. Returns the lists to write, or `null` when there is nothing to
 * step / the populations changed since (the stale step is discarded, and so is the rest of that
 * stack — older steps were made on top of the state that no longer exists).
 */
export function takeStep(h: AssignHistory, dir: 'undo' | 'redo', current: Assign): Assign | null {
  const from = dir === 'undo' ? h.undo : h.redo, to = dir === 'undo' ? h.redo : h.undo
  const step = from.pop()
  if (!step) return null
  const expect = dir === 'undo' ? step.after : step.before
  if (!Object.entries(expect).every(([p, ids]) => sameIds(current[p], ids))) {
    from.length = 0
    return null
  }
  to.push(step)
  return dir === 'undo' ? step.before : step.after
}

/**
 * Order the writes so a cluster is never in two populations at once (the server refuses that):
 * first every population drops what it is losing, then each takes its target list.
 */
export function writePlan(current: Assign, target: Assign): Array<[string, number[]]> {
  const plan: Array<[string, number[]]> = []
  const kept: Assign = {}
  for (const [p, ids] of Object.entries(target)) {
    const now = current[p] ?? []
    kept[p] = now.filter(x => ids.includes(x))
    if (kept[p].length < now.length) plan.push([p, kept[p]])        // drops first
  }
  for (const [p, ids] of Object.entries(target))
    if (!sameIds(kept[p], ids)) plan.push([p, sorted(ids)])          // then gains
  return plan
}
