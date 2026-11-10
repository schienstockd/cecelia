// Outputs made stale by an upstream re-run, in the GUI (docs/todo/GUI_CLARITY_AUDIT.md §8 item 12).
//
// The backend already answers it: `stale_outputs_for` (app/src/correction_staleness.jl) reads the run
// log, and `GET /api/analysis/lineage` carries the answer per image as `stale` / `staleUnknown`. This
// module turns those entries into the two things the user acts on:
//   * a QC finding on the image table's QC dot (`staleFindings`) — same channel as `qc_finding`s;
//   * a one-line advisory in the Run panel when the picked task READS a stale output (`staleReadNote`).
// `staleUnknown` (a maker from before inputs were recorded) is deliberately not shown: on projects
// older than input recording nearly every output is "unknown", and a dot that is always on says nothing.
import type { SourcedFinding } from './qc'

export interface StaleRun { fun: string; taskId?: string; at?: string }
export interface StaleRead { name: string; ns?: string; version?: string }
/** One link of why: the input was overwritten by a later run, or its maker is itself stale. */
export interface StaleLink { read: StaleRead; replacedBy?: StaleRun; madeBy?: StaleRun }
/** One stale output as `stale_outputs_for` reports it. `into` = written INTO a label set (tracks). */
export interface StaleOutput {
  name: string
  ns?: string
  into?: boolean
  run: StaleRun
  because: StaleLink[]
  summary?: string
}
export interface LineageImage { uid: string; stale?: StaleOutput[]; staleUnknown?: unknown[] }
export interface Lineage { images?: LineageImage[] }

/** Lineage → uid → its stale outputs (images with none are left out). */
export function staleByImage(lin: Lineage | null | undefined): Record<string, StaleOutput[]> {
  const out: Record<string, StaleOutput[]> = {}
  for (const im of lin?.images ?? []) if (im.stale?.length) out[im.uid] = im.stale
  return out
}

export const STALE_CODE = 'lineage.stale_output'

// what kind of thing a read name is, in the user's words
function kindOf(ns: string | undefined): string {
  return !ns ? 'image' : ns === 'labels' ? 'segmentation' : ns
}

/**
 * One QC finding per stale output, tagged with the task that made it (so the QC tooltip's module pill
 * names the step to re-run). Short = the problem, long = the action (docs/ui/COPY.md).
 */
export function staleFindings(stale: readonly StaleOutput[] | undefined,
                              labelFor: (fn: string) => string): SourcedFinding[] {
  return (stale ?? []).flatMap(s => {
    const first = s.because?.[0]
    if (!first?.read) return []
    const what = `${kindOf(first.read.ns)} '${first.read.name}'`
    const label = labelFor(s.run.fun)
    const short = first.madeBy ? `Stale: ${what} is stale too` : `Stale: ${what} was re-run`
    const long = first.madeBy
      ? `Re-run ${labelFor(first.madeBy.fun)} on '${first.read.name}', then ${label} on '${s.name}'.`
      : `Re-run ${label} on '${s.name}'.`
    return [{ level: 'warn' as const, code: STALE_CODE, short, long, funName: s.run.fun,
              detail: { name: s.name, ns: s.ns ?? '', into: !!s.into, summary: s.summary ?? '' } }]
  })
}

/** A name a task reads: `{name, ns}` with ns "" = an image version, "labels" = a label set. */
export interface TaskRead { name: string; ns: string }

/**
 * The Run panel line: the picked task reads, on some selected image, an output that is stale. Only
 * plain outputs count — an `into` entry (tracks written into a label set) leaves the label set itself
 * current, and the record says nothing about which readers read the tracks. Null = nothing to say.
 */
export function staleReadNote(
  reads: readonly TaskRead[],
  imageUids: readonly string[],
  byImage: Readonly<Record<string, readonly StaleOutput[]>>,
  labelFor: (fn: string) => string,
): { short: string; detail: string } | null {
  const want = new Set(reads.map(r => `${r.ns}\u0000${r.name}`))
  const hits: { uid: string; s: StaleOutput }[] = []
  for (const uid of imageUids)
    for (const s of byImage[uid] ?? [])
      if (!s.into && want.has(`${s.ns ?? ''}\u0000${s.name}`)) hits.push({ uid, s })
  if (!hits.length) return null
  const names = [...new Set(hits.map(h => h.s.name))]
  const nImg = new Set(hits.map(h => h.uid)).size
  const what = names.length === 1 ? `'${names[0]}'` : `${names.length} inputs`
  const where = imageUids.length === 1 ? '' : ` on ${nImg} of ${imageUids.length} images`
  const fixes = [...new Set(hits.map(h => `${labelFor(h.s.run.fun)} on '${h.s.name}'`))]
  return { short: `Reads stale ${what}${where}`, detail: `Re-run ${fixes.join(', ')} first` }
}
