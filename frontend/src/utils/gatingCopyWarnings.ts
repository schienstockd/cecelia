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

export interface CopyWarningLine { uid: string; text: string; pops: string[] }

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
