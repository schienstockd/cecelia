import { moduleKeyFromFun } from '../utils/taskModule'
import { defineStore } from 'pinia'
import { ref } from 'vue'
import { shortId } from '../utils/id'

export type TaskStatus = 'queued' | 'running' | 'done' | 'failed' | 'cancelled'

export interface TaskEntry {
  id: string
  seq: number             // monotonically increasing task number (#1, #2, …)
  module: string          // 'manageImages' | 'segment' | … — matches a page's `module=` prop
  label: string           // human-readable description
  imageUid: string
  imageName: string
  status: TaskStatus
  startedAt?: Date
  finishedAt?: Date
  log: string[]           // live log lines
  progress?: number       // 0–1 when the task emits [PROGRESS] lines; undefined otherwise
  taskName: string        // backend task key, e.g. 'cellposeCorrect'
  funName: string         // canonical "category.task" identifier sent to backend
  params: Record<string, unknown>
  projectUid: string
  // Chain provenance — set when task originated from a chain run
  chainRunId?:   string
  chainNodeId?:  string
  chainName?:    string
  // Rebuilt from `GET /api/tasks` rather than watched live (utils/runningTasks.ts) — this tab did not
  // launch it, so it has no earlier log lines (backfilled from disk on first open) and its params come
  // from the snapshot rather than from the dispatch that created it.
  adopted?: boolean
  // Set only when `params` is a PLACEHOLDER rather than what the run was submitted with — an adopted row
  // whose snapshot carried none (a backend predating `list_tasks().params`). `rerun()` sends `params`, so
  // this withholds Re-run: an empty dict would silently relaunch with the JSON spec's defaults. Absent on
  // every row this tab dispatched, so the common path needs no flag.
  paramsUnknown?: boolean
  // The SCHEDULER task id this row ran as. Same as `id` for a client-dispatched task (we mint the id and
  // send it), but a chain row is keyed by a synthetic `runId::nodeId::imageUid`, so its backend id is
  // only knowable from the `taskId` the chain frames carry. Needed to match a row against a backend
  // outcome (`utils/taskReconcile.ts`); undefined = no correlation available, never assume `id`.
  backendTaskId?: string
}

export const useTaskStore = defineStore('tasks', () => {
  const tasks   = ref<TaskEntry[]>([])
  const _seqRef = ref(0)

  // Signal for a UNIVERSAL "started in background" confirmation. `add()` is the client-dispatch
  // entry point (crop, copy, project export/import, generic task:run) — incoming server events go
  // through setStatus/setProgress by id, and chain steps through addFromChainEvent, so bumping this
  // only on add() fires one toast per user-initiated background job. App.vue watches it (component
  // context needed for useToast). Avoids each dialog rolling its own "it's running" feedback.
  const lastStarted = ref<TaskEntry | null>(null)

  function add(t: Omit<TaskEntry, 'id' | 'log' | 'seq'>): TaskEntry {
    const entry: TaskEntry = { ...t, id: shortId(), log: [], seq: ++_seqRef.value }
    tasks.value.unshift(entry)
    lastStarted.value = entry
    return entry
  }

  /**
   * Dispatch a BATCH as one user action: N real task entries, but ONE toast. `add()` bumps
   * `lastStarted` every call, so a bulk dispatch (Copy across a 20-image selection) would stack 20
   * identical "running in the background" toasts — use this instead whenever one click fans out.
   * `toastLabel` is what that single toast says (e.g. "Copy 20 images"); the entries keep their own
   * per-image labels in the task console.
   */
  function addMany(items: Array<Omit<TaskEntry, 'id' | 'log' | 'seq'>>, toastLabel?: string): TaskEntry[] {
    const entries: TaskEntry[] = items.map(t =>
      ({ ...t, id: shortId(), log: [], seq: ++_seqRef.value }))
    // reversed so the highest seq ends up at the head, exactly as N successive add() calls would leave it
    tasks.value.unshift(...[...entries].reverse())
    const last = entries[entries.length - 1]
    lastStarted.value = last ? { ...last, label: toastLabel ?? last.label } : null
    return entries
  }

  function appendLog(id: string, line: string) {
    const t = tasks.value.find(t => t.id === id)
    if (t) t.log.push(line)
  }

  /**
   * `at` carries the BACKEND's own timestamps for this transition (`startedAt`/`finishedAt` on the frame,
   * parsed by the ws store). They take precedence over stamping `new Date()` here, because "when this tab
   * received the frame" is not when the task ran: a terminal frame recovered by polling arrives seconds
   * late, and a frame for a task that started before this tab connected has no local equivalent at all.
   * Falling back to `new Date()` keeps a producer whose start the backend never noted working as before.
   */
  /**
   * Replace a row's log wholesale — for backfilling an adopted row from the on-disk log
   * (`utils/taskLogBackfill.ts`). Replaces rather than appends so a second open can't duplicate what the
   * first one fetched, and PREPENDS nothing: the fetched slice already covers the run up to now, and any
   * lines that arrived live since are re-included by the same slice.
   */
  function setLog(id: string, lines: string[]) {
    const t = tasks.value.find(t => t.id === id)
    if (t) t.log = lines
  }

  function setStatus(id: string, status: TaskStatus, at: { startedAt?: Date; finishedAt?: Date } = {}) {
    const t = tasks.value.find(t => t.id === id)
    if (!t) return
    // Terminal states set by the user (cancelled) are sticky — don't let a late
    // backend "done" or "running" overwrite a cancel the user explicitly requested.
    if (t.status === 'cancelled' && status !== 'cancelled') return
    t.status = status
    // A backend start is adopted even if we already stamped one locally — it's the real instant, and it
    // only ever moves the number closer to the truth.
    if (at.startedAt) t.startedAt = at.startedAt
    else if (status === 'running' && !t.startedAt) t.startedAt = new Date()
    if (status === 'done' || status === 'failed' || status === 'cancelled')
      t.finishedAt = at.finishedAt ?? new Date()
  }

  function setProgress(id: string, progress: number) {
    const t = tasks.value.find(t => t.id === id)
    if (t) t.progress = progress
  }

  function restart(id: string) {
    const t = tasks.value.find(t => t.id === id)
    if (!t) return
    t.status      = 'queued'
    t.log         = []
    t.progress    = undefined
    t.startedAt   = undefined
    t.finishedAt  = undefined
  }

  function cancel(id: string) {
    setStatus(id, 'cancelled')
  }

  function cancelChainRun(runId: string) {
    for (const t of tasks.value) {
      if (t.chainRunId === runId && (t.status === 'running' || t.status === 'queued'))
        t.status = 'cancelled'
    }
  }

  function remove(id: string) {
    const idx = tasks.value.findIndex(t => t.id === id)
    if (idx !== -1) tasks.value.splice(idx, 1)
  }

  function clearFinished(module: string, projectUid?: string) {
    const done = new Set<TaskStatus>(['done', 'failed', 'cancelled'])
    for (let i = tasks.value.length - 1; i >= 0; i--) {
      const t = tasks.value[i]
      if (t.module === module && done.has(t.status) && (!projectUid || t.projectUid === projectUid))
        tasks.value.splice(i, 1)
    }
  }

  // projectUid is optional so callers that genuinely want the cross-project view (the /tasks
  // manager) can still get everything — the per-module sidebar (TaskList/TaskRunner) always
  // passes the current project so switching projects doesn't leave a stale task list visible.
  function forModule(module: string, projectUid?: string) {
    return tasks.value.filter(t => t.module === module && (!projectUid || t.projectUid === projectUid))
  }

  const running = () => tasks.value.filter(t => t.status === 'running' || t.status === 'queued')

  // Used by TaskList jump button to tell TasksModule which task to highlight
  const jumpToId = ref<string | null>(null)

  /**
   * Add rows for work already in flight on the backend (`utils/runningTasks.ts` → `adoptableTasks`).
   *
   * The store is otherwise built purely from WS events THIS tab received, so a reload mid-run left it
   * empty while the backend kept working — and the terminal frames then had no row to land on. These
   * entries are marked `adopted` (the log then backfills from disk on first open) and get a `seq` like
   * any other row so the numbering stays monotonic.
   *
   * The snapshot carries the submitted `params`, so an adopted row supports Re-run like any other. A row
   * that arrives WITHOUT them is flagged `paramsUnknown` and keeps Re-run withheld — see the field.
   *
   * Idempotent by id: it runs on every (re)connect, and a row this tab launched always wins.
   */
  function adopt(rows: Array<Omit<TaskEntry, 'seq' | 'log' | 'adopted' | 'params' | 'taskName'> &
                             { taskName?: string; params?: Record<string, unknown> }>) {
    for (const r of rows) {
      if (tasks.value.some(t => t.id === r.id)) continue
      tasks.value.unshift({
        ...r,
        taskName: r.taskName ?? r.funName,
        params:   r.params ?? {},
        log:      [],
        seq:      ++_seqRef.value,
        adopted:  true,
        ...(r.params ? {} : { paramsUnknown: true }),
      })
    }
  }

  // Upsert a task entry from a chain WS event. Creates on first event (usually :queued),
  // updates status thereafter. startedAt is only set on :running (real pool-slot start), so a node
  // waiting for a GPU slot shows :queued with no elapsed time — and it comes from the frame
  // (`chain:node:*` carries the scheduler's own `startedAt`), falling back to now only if absent.
  function addFromChainEvent(opts: {
    runId: string
    nodeId: string
    imageUid: string
    imageName?: string
    fn: string
    label?: string
    chainName?: string
    status: TaskStatus
    projectUid: string
    taskId?: string
    startedAt?: Date
    finishedAt?: Date
  }) {
    const syntheticId = `${opts.runId}::${opts.nodeId}::${opts.imageUid}`
    const existing = tasks.value.find(t => t.id === syntheticId)
    if (existing) {
      // Update label/imageName on later events if they're now resolved
      if (opts.label    && existing.label     === (opts.fn.split('.').pop() ?? opts.fn)) existing.label     = opts.label
      if (opts.imageName && existing.imageName === opts.imageUid) existing.imageName = opts.imageName
      // …same for the scheduler task id: :queued may arrive before the node has one
      if (opts.taskId) existing.backendTaskId = opts.taskId
      setStatus(syntheticId, opts.status, { startedAt: opts.startedAt, finishedAt: opts.finishedAt })
      return existing
    }
    // The shared derivation (`utils/taskModule`) — this was a third inline copy of the same rule.
    const module = moduleKeyFromFun(opts.fn)
    const entry: TaskEntry = {
      id:          syntheticId,
      seq:         ++_seqRef.value,
      module,
      label:       opts.label ?? opts.fn.split('.').pop() ?? opts.fn,
      imageUid:    opts.imageUid,
      imageName:   opts.imageName ?? opts.imageUid,
      status:      opts.status,
      startedAt:   opts.startedAt ?? (opts.status === 'running' ? new Date() : undefined),
      finishedAt:  opts.finishedAt,
      log:         [],
      taskName:    opts.fn,
      funName:     opts.fn,
      params:      {},
      projectUid:  opts.projectUid,
      chainRunId:  opts.runId,
      chainNodeId: opts.nodeId,
      chainName:   opts.chainName,
      backendTaskId: opts.taskId || undefined,
    }
    tasks.value.unshift(entry)
    return entry
  }

  return { tasks, lastStarted, add, addMany, adopt, addFromChainEvent, appendLog, setLog, setStatus, setProgress, restart, cancel, cancelChainRun, remove, clearFinished, forModule, running, jumpToId }
})
