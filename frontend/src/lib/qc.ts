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
  short: string      // one-line badge tooltip
  long: string       // full detail (all findings), plain text
  groups: QcGroup[]  // the same findings, per producing task — see qcTooltipHtml
}

// `metadata.*` findings are calibration warnings (missing physical size / frame interval). They have
// their OWN image-table affordance (the click-to-fix warn icon → PhysicalSizeDialog, via
// imageMetadataWarnings.ts), so the general QC badge excludes them to avoid a double indicator — both
// now read the same qc.jl source, just partitioned by this predicate.
export const isMetadataCode = (code?: string): boolean => !!code && code.startsWith('metadata.')

// A finding plus the task that raised it. The sidecar key is `funName/valueName`, so which step
// produced a finding is already in the payload — the flatten below used to drop it, which left the
// badge listing three problems on one image with nothing to say that one came from import, one from
// drift and one from AF. Provenance is the first thing you need to act on a finding.
export interface SourcedFinding extends QcFinding {
  funName: string
}

// One task's findings, in the order the docs arrived. Rendering groups so a task that raises two
// findings is badged once rather than twice.
export interface QcGroup {
  funName: string
  findings: QcFinding[]
}

// Every finding across all of an image's QC docs, each tagged with its producing task.
export function qcFindings(img: CciaImage): SourcedFinding[] {
  const qc = img.qc
  if (!qc) return []
  // `funName` is written into the doc by qc.jl; the key is the fallback for a sidecar banked before
  // that field existed. Both spell the same thing, so either is enough to badge with.
  return Object.entries(qc).flatMap(([key, d]) =>
    (d?.findings ?? []).map(f => ({ ...f, funName: d?.funName || key.split('/')[0] })))
}

// Findings collapsed to one entry per task, first-seen order preserved.
export function groupByTask(fs: readonly SourcedFinding[]): QcGroup[] {
  const out: QcGroup[] = []
  const at = new Map<string, QcGroup>()
  for (const f of fs) {
    let g = at.get(f.funName)
    if (!g) { g = { funName: f.funName, findings: [] }; at.set(f.funName, g); out.push(g) }
    g.findings.push(f)
  }
  return out
}

/**
 * The image table's QC slot, as one of four states — the distinction `qcSummary` cannot make, because
 * it returns null both for "QC has never run" and for "QC ran and found nothing".
 *
 *  * `none`  — no QC sidecar at all. Nothing has been processed, so there is nothing to vouch for.
 *  * `clean` — QC ran and raised nothing. This is the one worth SHOWING (a green tick): "checked, fine"
 *              is information, and it is what makes a blank slot mean "not checked" rather than "fine".
 *  * `info` / `warn` — findings, worst level wins.
 *
 * Calibration (`metadata.*`) findings are excluded throughout, exactly as in `qcSummary`: they have
 * their own click-to-fix affordance and would otherwise show up twice.
 */
export type QcState = 'none' | 'clean' | 'info' | 'warn'
export function qcState(img: CciaImage): QcState {
  const docs = Object.values(img.qc ?? {})
  if (!docs.length) return 'none'
  const fs = qcFindings(img).filter(f => !isMetadataCode(f.code))
  if (!fs.length) return 'clean'
  return fs.some(f => f.level === 'warn') ? 'warn' : 'info'
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
  return { level, count: fs.length, short, long, groups: groupByTask(fs) }
}

const HTML_ESCAPES: Record<string, string> = {
  '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;',
}
const esc = (s: string) => s.replace(/[&<>"']/g, c => HTML_ESCAPES[c])

// Only `#rrggbb`/`#rrggbbaa` from the MODULE_COLORS palette ever reaches the style attribute below.
// The palette is a fixed literal so this cannot currently fail — it is here so that the ONE place this
// module writes unescaped markup stays safe if the tint ever becomes user- or backend-supplied.
const HEX = /^#[0-9a-f]{6}([0-9a-f]{2})?$/i
const styleAttr = (s: Record<string, string>) => Object.entries(s)
  .filter(([, v]) => HEX.test(v))
  .map(([k, v]) => `${k.replace(/[A-Z]/g, c => '-' + c.toLowerCase())}:${v}`)
  .join(';')

/**
 * The QC badge's tooltip as HTML — each task's findings under the **shared module tag**
 * (`.cc-module-tag` + `taskModule.moduleTagStyle`), the same pill the task manager and the image
 * table's run tag use, so "which step raised this" reads identically wherever it appears. Only the
 * task label is spelled out; the module is already carried by the pill's colour, and a tooltip this
 * narrow cannot afford a word restating it.
 *
 * Rendered with PrimeVue's `escape: false`, so **every interpolated string goes through `esc`**; the
 * text is app-authored (qc.jl's `QC_TEXT` catalog + a task's own `label`), but escaping it costs
 * nothing and means a future finding interpolating a filename or channel name cannot break out.
 *
 * HTML rather than the plain `long` string because a tooltip cannot show a pill otherwise — and
 * because the plain version was worse than it looked: `.p-tooltip-text` has no `white-space` rule, so
 * the `\n`s in `long` collapsed and three findings rendered as one run-on paragraph in a 280px box.
 * `labelFor`/`tagStyle` are injected (rather than importing the store) to keep this pure and testable.
 */
export function qcTooltipHtml(
  groups: readonly QcGroup[],
  labelFor: (fn: string) => string,
  tagStyle: (fn: string) => Record<string, string>,
): string {
  return groups.map(g => {
    const rows = g.findings.map((f, i) => {
      // the tag rides the group's FIRST finding, so a task raising two findings is named once
      // NO `-mod`/`-fun` part class here, and that is load-bearing: `-fun` means "secondary to the
      // bold module id" and carries `opacity: .85`, which is right in the image table's run tag and
      // wrong here, where this label is the whole point. Wearing it measured 3.70:1 against the pill
      // fill — back under AA, undoing exactly what `moduleTagStyle` lifted it for.
      const tag = i === 0
        ? `<span class="cc-module-tag" style="${styleAttr(tagStyle(g.funName))}">`
          + `${esc(labelFor(g.funName))}</span>`
        : ''
      return `<div class="qcf-p">${tag}${esc(f.short)}</div>`
           + `<div class="qcf-a">${esc(f.long)}</div>`
    }).join('')
    return `<div class="qcf">${rows}</div>`
  }).join('')
}
