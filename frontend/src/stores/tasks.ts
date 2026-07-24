import { defineStore } from 'pinia'
import { ref } from 'vue'
import { shortId } from '../utils/id'

export type TaskStatus = 'queued' | 'running' | 'done' | 'failed' | 'cancelled'

export interface TaskEntry {
  id: string
  seq: number             // monotonically increasing task number (#1, #2, …)
  module: string          // 'import' | 'segment' | ...
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

  function appendLog(id: string, line: string) {
    const t = tasks.value.find(t => t.id === id)
    if (t) t.log.push(line)
  }

  function setStatus(id: string, status: TaskStatus) {
    const t = tasks.value.find(t => t.id === id)
    if (!t) return
    // Terminal states set by the user (cancelled) are sticky — don't let a late
    // backend "done" or "running" overwrite a cancel the user explicitly requested.
    if (t.status === 'cancelled' && status !== 'cancelled') return
    t.status = status
    if (status === 'running' && !t.startedAt) t.startedAt = new Date()
    if (status === 'done' || status === 'failed' || status === 'cancelled')
      t.finishedAt = new Date()
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

  // Upsert a task entry from a chain WS event. Creates on first event (usually :queued),
  // updates status thereafter. startedAt is only stamped on :running (real pool-slot start),
  // so a node waiting for a GPU slot shows :queued with no elapsed time.
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
  }) {
    const syntheticId = `${opts.runId}::${opts.nodeId}::${opts.imageUid}`
    const existing = tasks.value.find(t => t.id === syntheticId)
    if (existing) {
      // Update label/imageName on later events if they're now resolved
      if (opts.label    && existing.label     === (opts.fn.split('.').pop() ?? opts.fn)) existing.label     = opts.label
      if (opts.imageName && existing.imageName === opts.imageUid) existing.imageName = opts.imageName
      setStatus(syntheticId, opts.status)
      return existing
    }
    // Derive module from fn category: 'cleanupImages' → 'cleanup', 'importImages' → 'import'
    const fnCategory = opts.fn.split('.')[0] ?? ''
    const module = fnCategory.replace(/Images$/i, '').replace(/Tasks$/i, '').toLowerCase() || 'chain'
    const entry: TaskEntry = {
      id:          syntheticId,
      seq:         ++_seqRef.value,
      module,
      label:       opts.label ?? opts.fn.split('.').pop() ?? opts.fn,
      imageUid:    opts.imageUid,
      imageName:   opts.imageName ?? opts.imageUid,
      status:      opts.status,
      startedAt:   opts.status === 'running' ? new Date() : undefined,
      log:         [],
      taskName:    opts.fn,
      funName:     opts.fn,
      params:      {},
      projectUid:  opts.projectUid,
      chainRunId:  opts.runId,
      chainNodeId: opts.nodeId,
      chainName:   opts.chainName,
    }
    tasks.value.unshift(entry)
    return entry
  }

  return { tasks, lastStarted, add, addFromChainEvent, appendLog, setStatus, setProgress, restart, cancel, cancelChainRun, remove, clearFinished, forModule, running, jumpToId }
})
