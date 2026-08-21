/**
 * How wide ONE task may go — the CPU sibling of the pool limits, and the other half of "how hard is
 * this machine allowed to work". A pool limit rations how many tasks run at once; this rations the
 * width of one. Backed by `[tasks].workerThreads`, delivered as `CECELIA_TASK_WORKERS` (a task's own
 * thread pools) and `LOKY_MAX_CPU_COUNT` (coastal's flow stage, which parallelises over processes).
 * docs/SCHEDULER.md → *Thread budgets*.
 *
 * The logic worth pinning is the DERIVED distinction, not the fetch: an effective 16 means something
 * different when it came from the box than when someone typed it, because the derived value follows
 * the hardware and a written one does not. A readout that shows a bare "16" for both loses that.
 */
export interface ThreadBudget {
  /** the number in effect now */
  workers: number
  /** what the machine would derive if nothing were configured */
  default: number
  /** slider ceiling */
  max: number
  /** true when `workers` came from the machine, not from `custom.toml` */
  derived: boolean
  /** cores on the box, for the tooltip */
  cores?: number
}

/** The value shown beside the label. Says WHERE the number came from, not just what it is. */
export function threadReadout(b: ThreadBudget | null): string {
  if (!b) return '—'
  return b.derived ? `auto · ${b.workers}` : String(b.workers)
}

/**
 * The tooltip. Names the two things a user cannot see: that it applies to the next task rather than
 * the running one (the value reaches a task as an env var read at spawn), and what "auto" resolved to
 * on this machine.
 */
export function threadTip(b: ThreadBudget | null): string {
  if (!b) return 'Threads one task may use for its own work.'
  const box = b.cores ? `${b.cores} cores` : 'this machine'
  return b.derived
    ? `Auto: ${b.workers} threads, derived from ${box}. Applies to the next task started.`
    : `${b.workers} threads per task (auto would be ${b.default} on ${box}). Applies to the next task started.`
}

/** Clamp a slider value the way the backend will, so the UI can't show a number it won't get. */
export function clampWorkers(n: number, max: number): number {
  return Math.min(Math.max(Math.round(n), 1), max)
}
