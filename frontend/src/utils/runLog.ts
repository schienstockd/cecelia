// Pure helpers over an image's automatic run log (the per-image provenance trail shipped from the
// backend as `CciaImage.runLog`, one entry per finished task — see app/src/run_log.jl). The run log
// is the single source of truth for "what has been done to this image": we DERIVE the last-run
// display and the "processed with" filter from it rather than persisting a separate mutable status
// attribute (which would drift out of sync). Entries arrive oldest→newest.

// One run-log entry. `status`/`params` are absent on legacy entries (readers treat a missing status
// as "done" — see runStatus). `at` is a local `yyyy-mm-ddTHH:MM:SS` timestamp, chosen so that
// lexicographic order == chronological order (matches the backend sort in api/src/routes.jl).
export interface RunLogEntry {
  fun: string
  valueName?: string
  status?: string                       // "done" | "failed" (missing ⇒ done, on legacy entries)
  params?: Record<string, unknown>
  at: string
}

export type ProcMode = 'ever' | 'last'

// Per-run outcome, treating a legacy entry with no status as a success ("done").
export function runStatus(e: RunLogEntry): string {
  return e.status && e.status.length ? e.status : 'done'
}

// The most recent run-log entry (the log is stored oldest→newest), or undefined when there is none.
export function lastRun(runLog?: RunLogEntry[] | null): RunLogEntry | undefined {
  if (!runLog || runLog.length === 0) return undefined
  return runLog[runLog.length - 1]
}

// The most recent SUCCESSFUL run-log entry (status not "failed"), or undefined when none succeeded.
// This is what the UI surfaces as "what has actually been done to this image" — a failed run left no
// output, so it shouldn't be presented as the image's state.
export function lastSuccessfulRun(runLog?: RunLogEntry[] | null): RunLogEntry | undefined {
  if (!runLog) return undefined
  for (let i = runLog.length - 1; i >= 0; i--) {
    if (runStatus(runLog[i]) !== 'failed') return runLog[i]
  }
  return undefined
}

// The set of fun_names this image has been processed with. `succeededOnly` (default) drops runs that
// ended in failure — "processed" means the output actually landed, not merely that a run was attempted.
export function funsRun(runLog?: RunLogEntry[] | null, succeededOnly = true): Set<string> {
  const out = new Set<string>()
  for (const e of runLog ?? []) {
    if (!e.fun) continue
    if (succeededOnly && runStatus(e) === 'failed') continue
    out.add(e.fun)
  }
  return out
}

// The union of fun_names across a set of images' run logs, sorted — the candidate list for the
// "processed with" filter (only funs that have actually been run are offered).
export function funsRunAcross(runLogs: (RunLogEntry[] | null | undefined)[], succeededOnly = true): string[] {
  const out = new Set<string>()
  for (const rl of runLogs) for (const f of funsRun(rl, succeededOnly)) out.add(f)
  return [...out].sort()
}

// Was this image processed with `fun`? `mode` 'ever' = any (non-failed, when succeededOnly) run of it;
// 'last' = the most recent run was that fun (and did not fail, when succeededOnly).
export function wasProcessedWith(
  runLog: RunLogEntry[] | null | undefined,
  fun: string,
  mode: ProcMode,
  succeededOnly = true,
): boolean {
  if (!fun) return false
  if (mode === 'last') {
    const e = lastRun(runLog)
    if (!e || e.fun !== fun) return false
    return !(succeededOnly && runStatus(e) === 'failed')
  }
  return funsRun(runLog, succeededOnly).has(fun)
}

// The module (task category) part of a fun_name: 'cleanupImages.cellposeCorrect' → 'cleanupImages'.
export function funCategory(fun: string): string {
  return (fun ?? '').split('.')[0] ?? ''
}

// A human module-page label for a fun_name's category — mirrors AppSidebar's prettifyCategory and the
// tasks store's category→module derivation ('cleanupImages' → 'Cleanup', 'clustPops' → 'Clust Pops').
// Generic (title-cased, de-camel-cased); not guaranteed to equal the exact nav label for every page.
export function funModuleLabel(fun: string): string {
  const cat = funCategory(fun).replace(/Images$/, '').replace(/Tasks$/, '')
  const spaced = cat.replace(/([a-z0-9])([A-Z])/g, '$1 $2').replace(/[_-]+/g, ' ').trim()
  return spaced ? spaced.charAt(0).toUpperCase() + spaced.slice(1) : ''
}
