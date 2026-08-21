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
  /** CPUs this PROCESS may use — affinity mask + cgroup quota, not the machine's count */
  cores?: number
  /** what the box has. Differs from `cores` under a cpuset or `--cpus`; equal on a workstation */
  machineCores?: number
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
  if (!b) return 'How wide one task may go.'
  const box = cpuPhrase(b)
  return b.derived
    ? `Auto: ${b.workers}, derived from ${box}. Applies to the next task started.`
    : `${b.workers} per task (auto would be ${b.default} on ${box}). Applies to the next task started.`
}

/**
 * How the machine is described. Says "usable" and names both numbers only when they DIFFER — on a
 * cluster node or in a container the process may use a fraction of the box, and a budget sized from
 * the box hands out workers for CPUs it cannot touch. On an ordinary workstation they are equal and
 * saying it twice would be noise.
 */
export function cpuPhrase(b: ThreadBudget): string {
  if (!b.cores) return 'this machine'
  if (b.machineCores && b.machineCores !== b.cores) {
    return `${b.cores} of ${b.machineCores} CPUs usable here`
  }
  return `${b.cores} CPUs`
}

/** Clamp a slider value the way the backend will, so the UI can't show a number it won't get. */
export function clampWorkers(n: number, max: number): number {
  return Math.min(Math.max(Math.round(n), 1), max)
}
