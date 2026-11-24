// The draft Methods section (`GET /api/analysis/methods`, app/src/ai/methods_report.jl). One generator,
// two scopes — a chain run, or a set (by uid, or the images a board shows, which read as their set) —
// and this builds the query for each. The text is the server's; nothing here rewrites it.
// docs/ANALYSIS.md → *Methods report*.
import type { HistoryParamsRow } from './chainFromHistory'
import { safeNamePart } from './batchMovie'
import { downloadBlob } from '../plots/export'
import { useToast } from '../composables/useToast'
import type { Ref } from 'vue'

/** What to write the Methods for: one chain run, one set, or a list of images (a board's — the server
 *  reads it as their set when it is one). A Methods section is per dataset, not per figure: a board's
 *  panels get their legends, not their own Methods. */
export type MethodsScope =
  | { kind: 'chainRun'; runId: string }
  | { kind: 'set'; setUid: string }
  | { kind: 'images'; imageUids: string[] }

export interface MethodsReport {
  markdown: string
  table: HistoryParamsRow[]
  /** The table as CSV, from the server — the same bytes the run's RO-Crate holds. */
  csv: string
  scope: { kind: 'chainRun' | 'set'; label: string; imageUids: string[] }
}

export function methodsReportUrl(projectUid: string, s: MethodsScope): string {
  let q: URLSearchParams
  if (s.kind === 'chainRun') q = new URLSearchParams({ projectUid, runId: s.runId })
  else if (s.kind === 'set') q = new URLSearchParams({ projectUid, setUid: s.setUid })
  else q = new URLSearchParams({ projectUid, imageUids: s.imageUids.join(',') })
  return `/api/analysis/methods?${q}`
}

/** Base file name for a download: `<what>_methods`, safe on every OS. */
export function methodsFileBase(what: string): string {
  return `${safeNamePart(what) || 'cecelia'}_methods`
}

/** A chain run's RO-Crate (`GET /api/chains/runs/ro-crate`, app/src/ai/ro_crate.jl): one `.zip`. */
export function roCrateUrl(projectUid: string, runId: string): string {
  return `/api/chains/runs/ro-crate?${new URLSearchParams({ projectUid, runId })}`
}

/** The crate's download name: `<run>_ro-crate.zip`, safe on every OS. */
export function roCrateFileName(runId: string): string {
  return `${safeNamePart(`run_${runId}`) || 'cecelia'}_ro-crate.zip`
}

/** Fetch a chain run's RO-Crate and save it; a failure is a toast. `busy` is true while it runs. */
export async function downloadRoCrate(projectUid: string, runId: string, busy: Ref<boolean>): Promise<void> {
  busy.value = true
  try {
    const res = await fetch(roCrateUrl(projectUid, runId))
    if (!res.ok) {
      const d = await res.json().catch(() => ({})) as { error?: string }
      throw new Error(d.error ?? res.statusText)
    }
    downloadBlob(roCrateFileName(runId), await res.blob())
  } catch (e) {
    useToast().add({ severity: 'error', life: 5000, summary: 'Couldn’t export the RO-Crate', detail: (e as Error).message })
  } finally { busy.value = false }
}
