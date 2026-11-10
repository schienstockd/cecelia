// The Checks cluster in the module action bar (docs/todo/GUI_CLARITY_PLAN.md, Decision 15; audit
// slice 7): Acquisition, QC and Cohort, each drawn by one `CheckButton`. This file is the pure half —
// each check's state from what the page already holds, and how a state is drawn.
//
// A check's STATE is a severity (lib/severity.ts: its icon and `--cc-sev-*` colour, never colour
// alone) or one of two non-verdicts: `none` (nothing to say yet — never run, no plan) and `busy`.
import { SEVERITY, type Severity } from './severity'
import type { AcqCheckResult } from '../utils/acqCheckApi'
import { badgeOf } from '../utils/acqCheck'
import { qcFindings, type QcDoc } from './qc'

export type CheckState = Severity | 'none' | 'busy'

export interface CheckLook { icon: string; color?: string }

/** How a state is drawn. `none` is the image table's "nothing to report" glyph, uncoloured. */
export function checkLook(state: CheckState): CheckLook {
  if (state === 'busy') return { icon: 'pi-spin pi-spinner' }
  if (state === 'none') return { icon: 'pi-minus' }
  return { icon: SEVERITY[state].icon, color: SEVERITY[state].color }
}

export interface CheckView { state: CheckState; count: number; tip: string }

/** Acquisition: warn while findings are open; ok once a plan has been checked clean; else none. */
export function acqCheckView(result: AcqCheckResult | null): CheckView {
  const { n, tip } = badgeOf(result)
  return { state: n ? 'warn' : result?.plan ? 'ok' : 'none', count: n, tip }
}

// ── QC: images with a warn finding ─────────────────────────────────────────────────────────────────

/** The image shape the QC check reads — a structural subset of the store's CciaImage. */
export interface QcImage { uid: string; included?: boolean | null; qc?: Record<string, QcDoc> }

/** Any warn-level QC finding, calibration (`metadata.*`) included: both draw a warn mark on the row. */
export const isQcFlagged = (img: QcImage): boolean =>
  qcFindings(img).some(f => f.level === 'warn')

const hasQc = (img: QcImage) => Object.keys(img.qc ?? {}).length > 0
const counted = (imgs: QcImage[]) => imgs.filter(i => i.included !== false)

/** Included images with a warn finding. An excluded image is out of the analysis, so out of the count. */
export const qcFlaggedCount = (imgs: QcImage[]): number => counted(imgs).filter(isQcFlagged).length

const s = (n: number) => (n === 1 ? '' : 's')

/** QC: warn with the flagged count; ok when QC ran and flagged nothing; none before any QC. */
export function qcCheckView(imgs: QcImage[], filterOn: boolean): CheckView {
  const n = qcFlaggedCount(imgs)
  if (filterOn) return { state: n ? 'warn' : 'none', count: n, tip: 'Show all images' }
  if (n) return { state: 'warn', count: n, tip: `Show only the ${n} QC-flagged image${s(n)}` }
  return counted(imgs).some(hasQc)
    ? { state: 'ok', count: 0, tip: 'No QC flags' }
    : { state: 'none', count: 0, tip: 'No QC yet' }
}

// ── Cohort: the last check, read back from the server ──────────────────────────────────────────────

/** GET /api/qc/cohort/state — the last cohort check over a page's funs (docs/API.md). */
export interface CohortState {
  checked: boolean
  checkedAt: number | null      // unix seconds
  flaggedUids: string[]
  nIncluded: number
  funs: { funName: string; checkedAt: number | null; flagged: number }[]
}

/** Cohort: warn with the flagged count; ok when the last check was clean; none when never checked. */
export function cohortCheckView(st: CohortState | null, busy = false): CheckView {
  if (busy) return { state: 'busy', count: 0, tip: 'Checking…' }
  if (!st?.checked) return { state: 'none', count: 0, tip: 'Not checked yet — flag outliers across the set' }
  const n = st.flaggedUids.length
  return n
    ? { state: 'warn', count: n, tip: `${n} image${s(n)} unlike the rest of the set` }
    : { state: 'ok', count: 0, tip: `All ${st.nIncluded} images within range` }
}

/** The per-image docs a cohort fun's findings live under: gating QC writes its own `gating.*` docs,
 *  every other check `cohort.{fun}` (mirrors `_cohort_image_fun`, app/src/qc_cohort.jl). */
export const cohortImageFun = (fun: string): string => fun.startsWith('gating.') ? fun : `cohort.${fun}`

/** An image's warn findings from the last cohort check over `funs` — what the Cohort popover lists. */
export function cohortFindingsOf(img: QcImage, funs: string[]) {
  const want = new Set(funs.map(cohortImageFun))
  return qcFindings(img).filter(f => f.level === 'warn' && want.has(f.funName))
}
