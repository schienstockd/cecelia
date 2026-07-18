// QC rendering — the frontend half of the QC framework (docs/todo/QC_PLAN.md). The BACKEND computes
// the findings (thresholds live in Julia); this module only aggregates and formats them into a badge
// + tooltip, mirroring imageMetadataWarnings.ts. Reused by ImageTable, MetadataPanel, and (later) the
// chain whiteboard so all surfaces agree about the same image.
import type { CciaImage } from '../stores/project'

export interface QcFinding {
  level: 'info' | 'warn'
  code: string           // stable slug, e.g. "drift.canvas_expansion"
  short: string
  long: string
  detail?: Record<string, unknown>
}

// One QC sidecar's parsed contents (1/{uid}/qc/{funName}/{valueName}.json). Only `findings` is relied
// on here; producer-specific extras (source/output/trajectory) are ignored by the renderer.
export interface QcDoc {
  funName?: string
  valueName?: string
  findings?: QcFinding[]
}

export interface QcSummary {
  level: 'info' | 'warn'
  count: number
  short: string   // one-line badge tooltip
  long: string    // full detail (all findings)
}

// `metadata.*` findings are calibration warnings (missing physical size / frame interval). They have
// their OWN image-table affordance (the click-to-fix warn icon → PhysicalSizeDialog, via
// imageMetadataWarnings.ts), so the general QC badge excludes them to avoid a double indicator — both
// now read the same qc.jl source, just partitioned by this predicate.
export const isMetadataCode = (code?: string): boolean => !!code && code.startsWith('metadata.')

// Every finding across all of an image's QC docs.
export function qcFindings(img: CciaImage): QcFinding[] {
  const qc = img.qc
  if (!qc) return []
  return Object.values(qc).flatMap(d => d?.findings ?? [])
}

// Worst-level summary for the badge (calibration `metadata.*` findings excluded — see isMetadataCode),
// or null when the image has no non-metadata QC findings. `warn` outranks `info`.
export function qcSummary(img: CciaImage): QcSummary | null {
  const fs = qcFindings(img).filter(f => !isMetadataCode(f.code))
  if (!fs.length) return null
  const level = fs.some(f => f.level === 'warn') ? 'warn' : 'info'
  const short = fs.length === 1 ? fs[0].short : `${fs.length} QC issues — hover for detail`
  // Tooltip: each finding as problem then the action (→). Brief; text convention in docs/todo/QC_PLAN.md.
  const long = fs.map(f => `${f.short}\n→ ${f.long}`).join('\n\n')
  return { level, count: fs.length, short, long }
}
