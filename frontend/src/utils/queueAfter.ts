// "Queue after" — start a run when one already going finishes, without waiting by the screen to press
// Run. The runner sends a two-node chain built on the spot (never saved): an `after_live` node for the
// running function, which waits per image for the runs of it already queued or running there, then the
// follow-up. Barrier, failure propagation, cancel and the run record are the chain executor's own.
// Backend: app/src/tasks/chain/after_live.jl; docs/SCHEDULER.md → *Queue after*.

import type { TaskEntry } from '../stores/tasks'
import type { ChainTemplate } from '../tasks/types'
import { isLiveStatus } from '../lib/taskStatus'

/** One function running on some of the selected images — what a follow-up can queue after. */
export interface LiveGroup {
  funName: string
  label: string
  /** the selected images it is queued or running on, each with the scheduler task ids live there */
  waitFor: Record<string, string[]>
}

/**
 * The functions queued or running on any of `selected`, in this project — one group per function, in
 * the task list's order (newest first, mostly: rows adopted after a reload come in snapshot order). History rows are a record of a run, not a run, so they never count. A row of a chain's
 * own node counts like any other: the backend waits for whatever run of the function is live there.
 */
export function liveGroups(tasks: readonly TaskEntry[], selected: readonly string[],
                           projectUid: string): LiveGroup[] {
  const sel = new Set(selected)
  const out = new Map<string, LiveGroup>()
  for (const t of tasks) {
    if (t.history || t.projectUid !== projectUid || !isLiveStatus(t.status)) continue
    // a set-scope run covers all its members, not just the representative its row is keyed by
    const on = (t.imageUids ?? [t.imageUid]).filter(u => sel.has(u))
    if (!on.length) continue
    // A chain row is keyed by a synthetic id; its scheduler id comes off its frames (`backendTaskId`).
    // Not `backendTaskId || id`: a synthetic id is never banked, so the wait would hold forever.
    const id = t.chainRunId ? t.backendTaskId : t.id
    const g = out.get(t.funName) ?? { funName: t.funName, label: t.label, waitFor: {} }
    for (const u of on) {
      const ids = (g.waitFor[u] ??= [])
      if (id && !ids.includes(id)) ids.push(id)
    }
    out.set(t.funName, g)
  }
  return [...out.values()]
}

/**
 * Which live function the ONE "Queue after" control queues behind. With one live, that one. With
 * several, only an explicit pick that is still live — never a guess: the list's order is not a
 * reliable dispatch order, and waiting for the wrong run starts the follow-up on missing outputs.
 */
export function queueAfterTarget(groups: readonly LiveGroup[], picked: string | null): LiveGroup | null {
  if (groups.length === 1) return groups[0]!
  return groups.find(g => g.funName === picked) ?? null
}

/** The follow-up as the runner would run it. */
export interface FollowUp {
  funName: string
  label: string
  scope: 'image' | 'set'
  params: Record<string, unknown>
  pool: string
}

/**
 * The inline chain template: wait for `live`, then run `next`. A set-scope follow-up runs once over the
 * images whose run finished well (`successful_only`); an image whose run failed or was cancelled sits it
 * out rather than going into, say, a clustering with no cells.
 */
export function queueAfterTemplate(live: LiveGroup, next: FollowUp): ChainTemplate {
  return {
    name: `${live.label} → ${next.label}`,
    nodes: [
      // `waitFor`: the runs this tab saw live, which the chain waits out even when its process can't see
      // them (the task on the runner, the chain in the backend), or they ended before it looked
      { id: 'wait', fn: live.funName, scope: 'image', params: { waitFor: live.waitFor },
        barrier_policy: 'all', resource_pool: '', after_live: true },
      { id: 'next', fn: next.funName, scope: next.scope, params: next.params,
        barrier_policy: 'successful_only', resource_pool: next.pool },
    ],
    edges: [{ from: 'wait', to: 'next' }],
  }
}

/**
 * Throws if `tmpl` has a Queue-after wait step. The whiteboard has no field for `after_live`: its node
 * mapping and its save both drop it, so a wait step opened there and saved would come back as an ordinary
 * run of the waited-for function — starting segmentation again instead of waiting for the run going.
 * Nothing brings one to the editor today (the Queue-after template is never saved); this makes the day
 * something does fail loudly instead.
 */
export function assertNoAfterLive(tmpl: Pick<ChainTemplate, 'nodes'>): void {
  const waits = (tmpl.nodes ?? []).filter(n => n.after_live).map(n => n.id)
  if (waits.length)
    throw new Error(`a Queue-after wait step (${waits.join(', ')}) can't be opened in the chain editor`)
}
