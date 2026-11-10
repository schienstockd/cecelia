// Task discovery — what each task is for, and when it is the wrong step (docs/todo/TASK_DISCOVERY_PLAN.md).
// The text lives in the task specs (`purpose` / `useWhen` / `notWhen`, docs/MODULES.md); this shapes
// it for TaskRunner, under the function picker. Pure, so it is unit-tested; no copy is written here.
//
// A spec line is either plain text or `{text, check}`. A check is ADVISORY: it looks at the selected
// images and says whether the line applies to them. It never blocks Run — a hard prerequisite is a
// `required` param (`missingRequired`), not this. A line with no check stays neutral: static guidance
// must not borrow a severity (InlineNote's rule — `ok` draws a green tick nobody earned).
import type { CciaImage } from '../stores/project'
import type { Severity } from '../lib/severity'
import {
  hasMeasuredTracks, hasNeighbourGraph, hasPixelSize, hasZStep,
  isDriftCorrected, isSegmented, isTimeSeries, isZStack,
} from './imageState'

export type DiscoveryLine = string | { text: string; check?: string }

export const lineText = (l: DiscoveryLine): string => typeof l === 'string' ? l : l.text
export const lineCheck = (l: DiscoveryLine): string | undefined => typeof l === 'string' ? undefined : l.check

interface CheckDef {
  test: (img: CciaImage) => boolean
  yes: string     // reads after "2 of 3 images …" when the test holds
  no: string      // …and when it does not
}

/** The checks a spec line may name. Only state the store already holds (no request). A spec names
 *  one by id; `!id` means the line's condition is the negation ("Pixel sizes missing" = `!pixelSizeSet`).
 *  An unknown id fails `taskDiscovery.test.ts`. */
export const TASK_CHECKS: Record<string, CheckDef> = {
  driftCorrected: { test: isDriftCorrected,  yes: 'drift-corrected',            no: 'not drift-corrected' },
  segmented:      { test: isSegmented,       yes: 'segmented',                  no: 'not segmented' },
  trackMeasured:  { test: hasMeasuredTracks, yes: 'with track measures',        no: 'without track measures' },
  neighbourGraph: { test: hasNeighbourGraph, yes: 'with a neighbour graph',     no: 'without a neighbour graph' },
  timeSeries:     { test: isTimeSeries,      yes: 'with several timepoints',    no: 'with one timepoint' },
  zStack:         { test: isZStack,          yes: 'with several Z planes',      no: 'with one Z plane' },
  pixelSizeSet:   { test: hasPixelSize,      yes: 'with a pixel size',          no: 'without a pixel size' },
  zStepSet:       { test: hasZStep,          yes: 'with a Z step',              no: 'without a Z step' },
}

/** The bare id of a check reference (`!pixelSizeSet` → `pixelSizeSet`). */
export const checkId = (ref: string) => ref.replace(/^!/, '')

export interface CheckedLine {
  text: string
  severity?: Severity   // only when a check reached a verdict worth showing
  summary?: string      // what the check found, e.g. "2 of 3 images not drift-corrected"
}

function count(n: number, total: number, phrase: string): string {
  return total === 1 ? `Selected image ${phrase}` : `${n} of ${total} images ${phrase}`
}

/** One spec line, checked against the selected images.
 *  - `use` line: holds for every image → ok; misses any → warn, naming how many it misses (a use line
 *    an image fails is as much a reason to look twice as a not line it meets).
 *  - `not` line: its condition holds for any image → warn, naming how many; otherwise neutral.
 *  No check, an unknown one, or no images → just the text. */
export function checkLine(line: DiscoveryLine, kind: 'use' | 'not', images: readonly CciaImage[]): CheckedLine {
  const text = lineText(line)
  const ref = lineCheck(line)
  const def = ref ? TASK_CHECKS[checkId(ref)] : undefined
  if (!ref || !def || !images.length) return { text }
  const neg = ref.startsWith('!')
  const holds = (i: CciaImage) => def.test(i) !== neg
  const [yes, no] = neg ? [def.no, def.yes] : [def.yes, def.no]
  const n = images.filter(holds).length
  const total = images.length
  if (kind === 'use') {
    return n === total ? { text, severity: 'ok', summary: count(n, total, yes) }
                       : { text, severity: 'warn', summary: count(total - n, total, no) }
  }
  return n > 0 ? { text, severity: 'warn', summary: count(n, total, yes) } : { text }
}

// ── Cross-page mentions ────────────────────────────────────────────────────────────────────────────
//
// A "When to use" line often names a task that lives on another page ("After HMM states…" on Cluster
// tracks; HMM states is on Behaviour). `linkTaskMentions` splits such a line into plain text and
// mentions, so the picker can render each mention as a link to its page, named in the line itself —
// no spec has to spell out where a task lives, and a new task gets the link for free.

/** A task the line may mention, already resolved to its page (`taskLanding`). */
export interface MentionTarget {
  label: string               // the task's label, as written in other tasks' lines
  path: string                // its page
  page: string                // that page's sidebar name
  module: string              // TaskRunner `cc-fn:<module>` key that pre-selects it there
  task: string                // the value to pre-select
}

export type LineSegment = { text: string; to?: MentionTarget }

const escapeRe = (s: string) => s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')

/** `text` split into plain runs and mentions of tasks on OTHER pages than `selfPath`. The longest
 *  label wins where two overlap ("HMM states" inside a longer label), and a label only matches as a
 *  whole word. */
export function linkTaskMentions(text: string, selfPath: string, targets: readonly MentionTarget[]): LineSegment[] {
  const others = targets.filter(t => t.path !== selfPath && t.label)
    .sort((a, b) => b.label.length - a.label.length)
  if (!others.length) return [{ text }]
  const byLabel = new Map(others.map(t => [t.label, t]))
  const re = new RegExp(`(?<![\\w-])(${others.map(t => escapeRe(t.label)).join('|')})(?![\\w-])`, 'g')
  const out: LineSegment[] = []
  let last = 0
  for (const m of text.matchAll(re)) {
    if (m.index! > last) out.push({ text: text.slice(last, m.index) })
    out.push({ text: m[0], to: byLabel.get(m[0]) })
    last = m.index! + m[0].length
  }
  if (last < text.length) out.push({ text: text.slice(last) })
  return out
}
