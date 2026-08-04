// `GET /api/tasks` — the scheduler's live in-flight set, and the ONE place the frontend asks for it.
//
// It answers two questions, which is why they share a module: *how many* tasks are running (before a
// destructive action) and *which* ones (so this tab can show work it didn't launch). Endpoint ownership is
// a rule, not a habit — `taskReconcile.ts` owns `/api/tasks/recent`, this owns `/api/tasks`, and a third
// asker is how two callers start disagreeing about what's in flight (docs/UI.md).
//
// **Why a tab has to ask at all.** The `tasks` store is built purely from WS events received by *this*
// tab. Nothing ever fetched the current set, so a page reload — or a second tab, or the app opened on
// another machine mid-run — showed an EMPTY task list while the backend segmented 20 images, and the
// terminal frames then landed on rows that didn't exist (`setStatus` matches by id and returns early), so
// the tasks never appeared even as they finished. Meanwhile the plots refreshed, because
// `bumpDataVersion` keys off the frame's `imageUid` rather than a row. Adopting the snapshot on connect
// is what closes that.
//
// **This is the browser half of what the task console does in Julia** (`_reconcile_snapshot!` in
// `api/task_console.jl`) — same route, same fields, same purpose, mirrored because the two clients share
// no runtime. Keep them recognisable as twins. One rule deliberately does NOT cross over: the console
// retires rows that vanish from the snapshot and tallies them "ended", because it may never see the
// terminal frame; the browser has the outcome poll (`taskReconcile.ts`) and recovers the REAL outcome
// instead of guessing, so it must not copy the retire half.
//
// Chain nodes are adopted too, keyed the way the chain frames key them so the two can't double up. The
// chain BOARD doesn't need this — it rebuilds a reloaded run from the run's own persisted state
// (`/api/chains/run`), which has the whole graph rather than just what is in flight. This is the task
// list's copy of the same work.

/** One row of `GET /api/tasks` (snake_case, like `/api/tasks/recent`). Shaped by `list_tasks()`. */
export interface InFlightTaskRow {
  id: string
  fun_name?: string
  pool_name?: string
  image_uid?: string
  chain_run_id?: string
  /** which node of that run — needed to rebuild the store's `runId::nodeId::imageUid` key */
  chain_node_id?: string
  status?: string
  /** ISO-8601 UTC; `started_at` is `''` until a pool slot admits the task */
  queued_at?: string
  started_at?: string
}

/** What a row needs from the app to become a task-list entry. */
export interface AdoptContext {
  /** the loaded project — a row for an image outside it can't be named, so it is skipped */
  projectUid: string
  /** imageUid → display name, from the project store */
  imageNames: Record<string, string>
  /** fun_name → human label, from the task-defs store (falls back to the fun's last segment) */
  labelFor?: (fun: string) => string
}

/** A task-list entry rebuilt from the snapshot. Deliberately a plain object — the store owns `TaskEntry`. */
export interface AdoptedTask {
  id: string
  module: string
  label: string
  funName: string
  taskName: string
  imageUid: string
  imageName: string
  projectUid: string
  status: 'queued' | 'running'
  startedAt?: Date
  chainRunId?: string
  chainNodeId?: string
  /** the scheduler's id — how a later frame or a recovered outcome matches this row */
  backendTaskId: string
}

const ACTIVE = new Set(['queued', 'running'])

/**
 * Which module page a `fun_name` belongs to — `'importImages.omezarr'` → `'import'`.
 *
 * Same derivation `addFromChainEvent` uses for a chain node, because both are answering the same question
 * about the same string.
 */
function moduleFromFun(fun: string): string {
  const category = fun.split('.')[0] ?? ''
  return category.replace(/Images$/i, '').replace(/Tasks$/i, '').toLowerCase() || 'chain'
}

/**
 * Snapshot rows → task-list entries, dropping what this tab can't or shouldn't show.
 *
 * Skipped, each for a reason:
 * - **a terminal row** — the snapshot only ever lists in-flight work, but a row that has just flipped is
 *   the outcome poll's business, not ours.
 * - **an image the loaded project doesn't have** — the snapshot carries no `projectUid` (only
 *   `image_uid`), so a row we can't resolve might belong to another project entirely; showing it under
 *   this one would attribute someone else's run to the wrong place.
 * - **a row this tab already has** — it launched it, so its entry is richer (params, log, seq). Matched on
 *   the SCHEDULER id, which for a chain row lives on `backendTaskId` rather than `id`.
 * - **a chain node with no `chain_node_id`** — a set-scope node (it bypasses `run_task`, so it has no
 *   record to report one) or a backend too old to send it. Without the node id the row can't take the
 *   store's `runId::nodeId::imageUid` key, and adopting it under its scheduler id would leave a SECOND row
 *   behind as soon as the next `chain:node:*` frame arrived.
 *
 * A chain node WITH its node id is adopted under exactly that key, so the next chain frame updates this
 * row instead of adding one. (The chain *board* recovers a reloaded run separately and more completely,
 * from the run's own persisted state — `/api/chains/run`. This is only about the task list.)
 */
export function adoptableTasks(
  rows: InFlightTaskRow[],
  ctx: AdoptContext,
  known: (id: string) => boolean,
): AdoptedTask[] {
  if (!Array.isArray(rows) || !ctx.projectUid) return []
  const out: AdoptedTask[] = []
  for (const r of rows) {
    const id = String(r?.id ?? '')
    const status = String(r?.status ?? '')
    const fun = String(r?.fun_name ?? '')
    const imageUid = String(r?.image_uid ?? '')
    const runId  = String(r?.chain_run_id ?? '')
    const nodeId = String(r?.chain_node_id ?? '')
    if (!id || !fun || !ACTIVE.has(status) || known(id)) continue
    if (runId && !nodeId) continue
    const imageName = ctx.imageNames[imageUid]
    if (!imageName) continue
    out.push({
      // a chain row is addressed by the synthetic key the chain frames will use, so the next one updates
      // THIS row; a plain task is addressed by the scheduler id it was launched with
      id:         runId ? `${runId}::${nodeId}::${imageUid}` : id,
      backendTaskId: id,
      ...(runId ? { chainRunId: runId, chainNodeId: nodeId } : {}),
      module:     moduleFromFun(fun),
      label:      ctx.labelFor?.(fun) || fun.split('.').pop() || fun,
      funName:    fun,
      taskName:   fun,
      imageUid,
      imageName,
      projectUid: ctx.projectUid,
      status:     status as 'queued' | 'running',
      // The scheduler's own start, so the elapsed is right from the first render rather than counting
      // from when this tab noticed. `''`/absent = queued, or a backend too old to send it.
      startedAt:  r.started_at ? new Date(r.started_at) : undefined,
    })
  }
  return out
}

/**
 * The raw snapshot, or `[]` if the check fails.
 *
 * Fails CLOSED to an empty list for the same reason `fetchRecentOutcomes` does: this feeds a display
 * backstop, so a transient error must mean "learned nothing", never an exception in the connect path.
 */
export async function fetchInFlightTasks(): Promise<InFlightTaskRow[]> {
  try {
    const r = await fetch('/api/tasks')
    if (!r.ok) return []
    const rows = await r.json()
    return Array.isArray(rows) ? rows as InFlightTaskRow[] : []
  } catch {
    return []
  }
}

/**
 * Count of in-flight scheduler tasks, or `0` if the check fails.
 *
 * Failing OPEN (0 = idle) is deliberate: the count gates a *warning*, not the action itself, so a
 * transient fetch error must not block a user from quitting or exporting. The cost of a missed
 * warning is lower than the cost of an unusable button.
 *
 * Counts the WHOLE snapshot, including the rows `adoptableTasks` drops — "is the backend busy?" is a
 * different question from "what can this tab show?", and a chain node still writing to disk is exactly
 * the kind of work a quit must warn about.
 */
export async function runningTaskCount(): Promise<number> {
  return (await fetchInFlightTasks()).length
}
