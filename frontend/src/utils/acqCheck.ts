// Pure helpers for the acquisition check modal (AcqCheckDialog / AcqPlanForm): the plan sentence, a
// fresh plan from the suggested designs, what keeps a plan from saving, and the per-movie table grouped
// by design. Types and calls: acqCheckApi.ts.
import type { AcqCheckResult, AcqDesign, AcqFinding, AcqPlan, AcqRow, AcqStage } from './acqCheckApi'
import type { Severity } from '../lib/severity'
import { isLiveStatus } from '../lib/taskStatus'
import { AI_OFF_COPY, aiOffLine, type AiState } from './aiSwitch'
import type { TaskStatus } from '../stores/tasks'

/** What the plan says it will measure, in the order the form offers it. */
export const ACQ_MEASURES = ['movement', 'contacts', 'uptake', 'counts', 'shape'] as const

/** "A, B and C" */
export function listWords(xs: string[]): string {
  const w = xs.filter(Boolean)
  return w.length <= 1 ? (w[0] ?? '') : `${w.slice(0, -1).join(', ')} and ${w[w.length - 1]}`
}

/** The plan as one sentence — what the modal echoes and the entry stores: "Compare movement of P14,
 *  OT-I and gBT (naive — expected the same)". Falls back to the objects when nothing is compared. */
export function planSentence(plan: AcqPlan | null): string {
  if (!plan || !plan.objects.length) return ''
  const what = plan.measure.length ? listWords(plan.measure) : 'measurements'
  const between = plan.compare.length ? plan.compare : plan.objects.map(o => o.name)
  const verb = between.length > 1 ? `Compare ${what} of ${listWords(between)}` : `Measure ${what} of ${between[0]}`
  return plan.expectation.trim() ? `${verb} (${plan.expectation.trim()})` : verb
}

/** A plan to start the form from: the suggested designs, no objects yet. */
export function emptyPlan(designs: AcqDesign[]): AcqPlan {
  return {
    stage: 'test',
    designs: designs.map(d => ({ id: d.id, key: d.key, label: d.label ?? d.key })),
    objects: [], references: [], measure: [], compare: [], expectation: '',
  }
}

/** Why the plan can't be saved yet (empty = it can): an object needs a name and a channel in every
 *  design, and two objects can't share a name. */
export function planProblems(plan: AcqPlan): string[] {
  const out: string[] = []
  const named = plan.objects.filter(o => o.name.trim())
  if (!named.length) out.push('Add at least one object')
  const names = named.map(o => o.name.trim())
  if (new Set(names).size !== names.length) out.push('Two objects share a name')
  for (const o of named) {
    const missing = plan.designs.filter(d => !(d.id in o.channels))
    if (missing.length) out.push(`${o.name}: pick a channel for ${listWords(missing.map(d => d.label ?? d.key))}`)
  }
  return out
}

/** The plan as saved: names trimmed, unnamed rows dropped, `compare` limited to existing objects. */
export function cleanPlan(plan: AcqPlan): AcqPlan {
  const objects = plan.objects.filter(o => o.name.trim()).map(o => ({ ...o, name: o.name.trim() }))
  const names = new Set(objects.map(o => o.name))
  return {
    ...plan,
    objects,
    references: plan.references.filter(o => o.name.trim()).map(o => ({ ...o, name: o.name.trim() })),
    compare: plan.compare.filter(c => names.has(c)),
    expectation: plan.expectation.trim(),
    text: planSentence({ ...plan, objects, compare: plan.compare.filter(c => names.has(c)) }),
  }
}

export interface AcqRowGroup { design: string | null; label: string; rows: AcqRow[] }

/** The per-movie rows grouped by design, in the plan's (or suggestion's) design order. */
export function rowsByDesign(result: AcqCheckResult): AcqRowGroup[] {
  const designs = result.plan?.designs ?? result.suggestedDesigns
  const label = (id: string | null) => {
    const d = designs.find(x => x.id === id || x.key === id)
    return d ? (d.label || d.key || d.id) : (id ?? 'No design')
  }
  const order = designs.map(d => d.id)
  const ids = [...new Set(result.rows.map(r => r.design))]
    .sort((a, b) => (order.indexOf(a ?? '') + 1 || 999) - (order.indexOf(b ?? '') + 1 || 999))
  return ids.map(id => ({ design: id, label: label(id), rows: result.rows.filter(r => r.design === id) }))
}

/** The findings for one scope ("set" or an image uid), problems first. */
export function findingsFor(result: AcqCheckResult, scope: string): AcqFinding[] {
  const rank = { warn: 0, info: 1, ok: 2 } as Record<string, number>
  return result.findings.filter(f => f.scope === scope).sort((a, b) => (rank[a.level] ?? 3) - (rank[b.level] ?? 3))
}

/** The Bright Z cell: on, off, or off with a ramp saved (the ramp in the tooltip). */
export function brightZText(r: Pick<AcqRow, 'brightZ' | 'brightZRamp'>): string {
  if (r.brightZ === null) return '—'
  return r.brightZ ? 'on' : r.brightZRamp ? 'off · ramp saved' : 'off'
}

/** The movies a finding is about (`detail.imageUids`) — the "Measure N movies" targets. */
export function findingImageUids(f: AcqFinding | undefined): string[] {
  const d = f?.detail as { imageUids?: unknown } | undefined
  return Array.isArray(d?.imageUids) ? d!.imageUids.filter((u): u is string => typeof u === 'string') : []
}

/** How many tasks of `funName` on these images are still queued or running — the batch is done at 0. */
export function measuringCount(tasks: { funName?: string; imageUid?: string; status: TaskStatus }[],
                               funName: string, imageUids: string[]): number {
  return tasks.filter(t => t.funName === funName && imageUids.includes(t.imageUid ?? '') && isLiveStatus(t.status)).length
}

/** A number or a dash, rounded to `digits`. */
export const fmt = (v: number | null | undefined, digits = 0): string =>
  v === null || v === undefined || !Number.isFinite(v) ? '—' : v.toFixed(digits)

/** The badge: warnings still open, and what the button says. */
export function badgeOf(result: AcqCheckResult | null): { n: number; tip: string } {
  if (!result) return { n: 0, tip: 'Check these movies against what you plan to do with them' }
  if (!result.plan) return { n: result.nOpen, tip: result.nOpen ? `${result.nOpen} to look at — set a plan for the full check` : 'Set a plan for the full check' }
  return { n: result.nOpen, tip: result.nOpen ? `${result.nOpen} to look at` : 'Nothing to look at' }
}

/** A finding's InlineNote severity: `info` is a note with none (the info icon, no verdict). */
export const findingSeverity = (f: AcqFinding): Severity | undefined =>
  f.level === 'warn' ? 'warn' : f.level === 'ok' ? 'ok' : undefined

/** The stage picker's options, and the heading over the review's verdict for each stage. */
export const ACQ_STAGES: { value: AcqStage; label: string }[] = [
  { value: 'test', label: 'Test acquisition' }, { value: 'final', label: 'Final data' }]
export const verdictHeading = (stage: AcqStage) => stage === 'final' ? 'Before analysing' : 'Before acquiring the rest'

/** A finding's lines under its short text, each labelled by what it holds: which movies (or designs)
 *  have which value, what to do next, and what it means for the analysis. On final data the action IS
 *  the analysis one (the check sends no `thisData` then). */
export function findingLines(f: AcqFinding, stage: AcqStage = 'test'): { label: string; text: string }[] {
  return [
    ...(f.where ? [{ label: f.whereLabel ?? 'Movies', text: f.where }] : []),
    { label: stage === 'final' ? 'Analysis' : 'Next', text: f.long },
    ...(f.thisData ? [{ label: 'Analysis', text: f.thisData }] : []),
  ]
}

/** A plan with every channel as its name — an older plan stored the index, which the form's channel
 *  picker (by name) would show as blank. An index the design doesn't have is left as is. */
export function planByName(plan: AcqPlan, designs: AcqDesign[]): AcqPlan {
  const conv = (o: AcqPlan['objects'][number]) => ({
    ...o,
    channels: Object.fromEntries(Object.entries(o.channels).map(([d, c]) =>
      [d, typeof c === 'number' ? (designs.find(x => x.id === d)?.channels?.find(ch => ch.index === c)?.name ?? c) : c])),
  })
  return { ...plan, objects: plan.objects.map(conv), references: plan.references.map(conv) }
}

/** Why the Review button can't run ('' = it can): AI off (and by whom), no saved plan, or a turn
 *  already running. */
export function reviewBlocked(o: { ai: AiState; hasPlan: boolean; running: boolean }): string {
  if (!o.ai.enabled) return o.ai.setBy === 'workstation' ? `${AI_OFF_COPY.title} — ${aiOffLine(o.ai).toLowerCase()}` : AI_OFF_COPY.title
  if (!o.hasPlan) return 'Save a plan first'
  if (o.running) return 'Reviewing…'
  return ''
}

