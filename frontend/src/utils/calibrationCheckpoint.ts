// The pre-run checkpoint (docs/todo/GUI_CLARITY_AUDIT.md §4, calibration at Run): when the picked
// task measures in physical units (`requires.scale`), the selected images' calibration findings (`metadata.*`, banked by qc.jl)
// that bear on THOSE scales, one line per finding with the images it covers. TaskRunner shows each as
// an InlineNote under "When to use" with a Fix that opens the size & timing editor. ADVISORY: Run is
// never gated by this — the missing-scale gate is `utils/taskGating.ts`, a separate rule.
import type { CciaImage } from '../stores/project'
import type { TaskDef } from '../tasks/types'
import { qcFindings, isMetadataCode } from '../lib/qc'
import { imageAxes, missingScale, taskRequiresAxes, taskRequiresScale, type ScaleAxis } from './taskGating'

export interface CheckpointLine {
  code: string
  short: string     // the finding's problem, with "· n of m" when it covers part of the selection
  detail: string    // the finding's action
  uids: string[]    // the selected images it covers, selection order
}

// A finding's `detail.field` → the scale it is about. A finding without one is about the pixel size.
const FIELD_SCALE: Record<string, ScaleAxis> = { x: 'XY', y: 'XY', z: 'Z', t: 'T' }

/** Whether this image's finding on `scale` matters to a task needing `need`: a static image has no
 *  frame interval to get wrong and a single plane no z spacing (the `missingScale` rule). */
function applies(scale: ScaleAxis, need: Set<ScaleAxis>, img: CciaImage): boolean {
  if (!need.has(scale)) return false
  if (scale === 'Z') return imageAxes(img).has('Z')
  if (scale === 'T') return imageAxes(img).has('T')
  return true
}

export function calibrationCheckpoint(def: TaskDef | undefined, imgs: CciaImage[]): CheckpointLine[] {
  if (!def) return []
  const need = taskRequiresScale(def)
  if (!need.size || !imgs.length) return []
  const byCode = new Map<string, { short: string; detail: string; uids: string[] }>()
  for (const img of imgs) {
    for (const f of qcFindings(img)) {
      if (f.level !== 'warn' || !isMetadataCode(f.code)) continue
      const field = typeof f.detail?.field === 'string' ? f.detail.field : 'x'
      if (!applies(FIELD_SCALE[field] ?? 'XY', need, img)) continue
      const line = byCode.get(f.code) ?? { short: f.short, detail: f.long, uids: [] }
      if (!line.uids.includes(img.uid)) line.uids.push(img.uid)
      byCode.set(f.code, line)
    }
  }
  return [...byCode].map(([code, l]) => ({
    code, detail: l.detail, uids: l.uids,
    short: imgs.length > 1 ? `${l.short} · ${l.uids.length} of ${imgs.length}` : l.short,
  }))
}

/** True when the gate is ONLY a missing scale (no missing axis, not coming soon) AND the checkpoint's
 *  lines name every image that scale is missing on — then they say it image by image, each with its
 *  Fix, and the runner drops the generic gate note rather than saying it twice. An image missing a
 *  scale with no banked finding (QC from before that finding existed) keeps the gate note, so nothing
 *  that blocks Run goes unsaid. Run stays disabled either way. */
export function checkpointCoversGate(def: TaskDef | undefined, imgs: CciaImage[], lines: CheckpointLine[]): boolean {
  if (!def || def.comingSoon || !imgs.length || !lines.length) return false
  const need = taskRequiresAxes(def)
  if (imgs.some(img => [...need].some(ax => !imageAxes(img).has(ax)))) return false
  const blocked = imgs.filter(img => missingScale(def, img).size > 0)
  if (!blocked.length) return false
  const named = new Set(lines.flatMap(l => l.uids))
  return blocked.every(img => named.has(img.uid))
}
