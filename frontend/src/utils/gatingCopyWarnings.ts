// The wording of ONE finding — `missing_gate_columns`: a gate/filter reads a column an image's table
// lacks, so populations are empty there — on its two surfaces. The gating copy's per-target warnings
// (`POST /api/gating/copy` → `warnings`, #1737) as the copy dialog shows them: one short line per
// target naming the absent columns and how many populations end up empty, the population names for
// the hover, and one action line for the lot. And the Population Manager's per-pop reason
// (`missingColumns` beside every pop tree, #1769). Pure, so the wording is tested rather than eyeballed.
// INVENTORY-EXEMPT: callers are GatingCopyDialog + PopulationManager; the wording is specific to this finding.

import { clusterMeasurePrefix } from './clusterMeasure'

export interface MissingColumn { column: string; pops: string[] }
export type CopyWarnings = Record<string, MissingColumn[]>

// `info` — a note, not a problem (the copy went through as meant): rendered without a severity.
export interface CopyWarningLine { uid: string; text: string; pops: string[]; info?: boolean }

const popLeaf = (path: string) => path.slice(path.lastIndexOf('/') + 1) || path

export function copyWarningLines(warnings: CopyWarnings | undefined,
                                 nameOf: (uid: string) => string): CopyWarningLine[] {
  return Object.entries(warnings ?? {})
    .filter(([, cols]) => cols?.length)
    .map(([uid, cols]) => {
      const pops = [...new Set(cols.flatMap(c => c.pops))]
      const n = pops.length
      return {
        uid,
        text: `${nameOf(uid)}: no ${cols.map(c => c.column).join(', ')} — ` +
              `${n} population${n === 1 ? '' : 's'} will be empty`,
        pops: pops.map(popLeaf),
      }
    })
}

const CLUSTER_PREFIXES = [clusterMeasurePrefix('clust'), clusterMeasurePrefix('region')]

// What to do about it. A cluster/region column comes from a run the target wasn't in.
export function copyWarningAction(warnings: CopyWarnings | undefined): string {
  const cols = Object.values(warnings ?? {}).flat().map(c => c.column)
  const clustered = cols.length > 0 && cols.every(c => CLUSTER_PREFIXES.some(p => c.startsWith(p)))
  return clustered
    ? 'Run the clustering on those images, or delete those populations there.'
    : 'Add those measures to those images, or delete those populations there.'
}

// The Population Manager's tooltip on a pop that reads 0 because its (or an ancestor's) gate/filter
// column is absent here — server `missingColumns[path]`, through the display label. '' ⇒ show nothing.
export function missingColumnsReason(cols: readonly string[] | undefined,
                                     label: (col: string) => string = c => c): string {
  return cols?.length ? `Empty: no ${cols.map(label).join(', ')} on this image` : ''
}

// Targets the copy refused (`POST /api/gating/copy` → `skipped`: uid → reason, e.g. "channel order
// differs — mean_intensity_0 is CD8 here, CD4 in the source"). Short line = image + the reason's head;
// the hover carries the rest. Without these, a refused image just goes missing from "Copied to N".
export function copySkippedLines(skipped: Record<string, string> | undefined,
                                 nameOf: (uid: string) => string): CopyWarningLine[] {
  return Object.entries(skipped ?? {}).map(([uid, reason]) => {
    const [head, ...rest] = reason.split(' — ')
    return { uid, text: `${nameOf(uid)}: not copied, ${head}`, pops: rest.length ? [rest.join(' — ')] : [] }
  })
}

// Targets copied with their gates pointed at another column (`POST /api/gating/copy` → `remapped`:
// uid → [{from, to, channel}]) — the target orders its channels differently, so each gated channel was
// matched by NAME. A note, not a warning: the copy reads the same markers. The hover names the channels.
export interface ChannelRemap { from: string; to: string; channel: string }
export function copyRemappedLines(remapped: Record<string, ChannelRemap[]> | undefined,
                                  nameOf: (uid: string) => string): CopyWarningLine[] {
  return Object.entries(remapped ?? {})
    .filter(([, rs]) => rs?.length)
    .map(([uid, rs]) => ({ uid, text: `${nameOf(uid)}: channels matched by name`,
                           pops: [...new Set(rs.map(r => r.channel))], info: true }))
}

// The copied plot layout (`canvasPanels.copyEntry`) carries each gate plot's axes as raw columns
// (`x`/`y`/`z`, the pairs panel's `channels`). On a remapped target, point them at the target's
// columns for the same channels too, so the axes show the markers the copied gates read. In place.
export function renamePanelColumns(states: Record<string, unknown>[], remaps: ChannelRemap[] | undefined): void {
  const to = Object.fromEntries((remaps ?? []).map(r => [r.from, r.to])) as Record<string, string>
  if (!Object.keys(to).length) return
  for (const s of states) {
    for (const k of ['x', 'y', 'z']) { const v = s[k]; if (typeof v === 'string' && to[v]) s[k] = to[v] }
    if (Array.isArray(s.channels)) s.channels = s.channels.map(c => (typeof c === 'string' && to[c]) || c)
  }
}
