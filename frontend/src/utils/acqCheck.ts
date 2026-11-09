// Pure helpers for the acquisition check modal (AcqCheckDialog / AcqPlanForm): the plan sentence, a
// fresh plan from the suggested designs, what keeps a plan from saving, and the per-movie table grouped
// by design. Types and calls: acqCheckApi.ts.
import type { AcqCheckResult, AcqDesign, AcqFinding, AcqPlan, AcqRow } from './acqCheckApi'

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

/** A depth range as "−3 to −17 %", top plane to bottom, with real minus signs. */
export function formatDepth(d: [number, number] | null): string {
  if (!d) return '—'
  const s = (v: number) => (Math.round(v) < 0 ? '−' : Math.round(v) > 0 ? '+' : '') + Math.abs(Math.round(v))
  const [lo, hi] = d
  return Math.round(lo) === Math.round(hi) ? `${s(lo)} %` : `${s(hi)} to ${s(lo)} %`
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
