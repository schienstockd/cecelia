// A board panel's figure legend: what it plots, of which populations, what a point is, n and the test —
// written from the panel's own state, so it says what the panel shows now. The dataset's processing is
// the Methods report's (docs/ANALYSIS.md → *Methods report*); a legend is per figure. The line under an
// exported plot (`plotCaption`) is the short form of the same facts.
import { popDisplayLabels } from './popName'
import { listWords } from './acqCheck'
import type { StatUnit, ImageAgg } from './statUnitState'

/** What a summary panel tells its legend (`SummaryPanel` → `legendFacts()`). */
export interface SummaryLegendFacts {
  /** The plot type's label ("Track measures") and the panel's own title, when it has one. */
  plot: string
  title?: string
  /** The measure column (`live.track.straightness`), or its spec label when the spec names it. */
  measure: string
  chart: string
  /** One row per drawn series: its population (manager path), source image ('' = pooled) and n. */
  series: { pop: string; uID?: string; n: number }[]
  /** 'track' | 'cell' — what one point is at the individual level. */
  pointKind?: 'track' | 'cell'
  statUnit: StatUnit
  imageAgg: ImageAgg
  /** The replicate noun ("mouse"/"mice") at the replicate level, else image/images. */
  unitNoun: { one: string; many: string }
  /** The test the plot drew (`comparisons.methodNote`), or '' — none drawn. */
  test: string
}

const CHART_WORDS: Record<string, string> = {
  boxplot: 'Boxes show the median and interquartile range, whiskers 1.5 × IQR',
  bar: 'Bars show the mean',
  violin: 'Violins show the distribution',
  histogram: 'Histogram',
}

// Measure columns a reader recognises: the few whose key is not already their name. The Julia twin is
// `methods_measure_phrase` (app/src/ai/methods_text.jl) — the Methods report says the same words.
const MEASURE_WORDS: Record<string, string> = {
  volume_mesh: 'volume', surface_to_volume: 'surface-to-volume ratio', surface_area_mesh: 'surface area',
  displacementRatio: 'displacement ratio', outreachRatio: 'outreach ratio', meanTurningAngle: 'mean turning angle',
}

/** A measure column as words: `live.track.meanTurningAngle` → "mean turning angle". */
export function measureWords(col: string): string {
  const hmm = /^live\.cell\.hmm\.(state|transitions)\.(.+)$/.exec(col)
  if (hmm) return `HMM ${hmm[1] === 'state' ? 'states' : 'transitions'} (${hmm[2]})`
  const s = col.replace(/^live\.(track|cell)\./, '').replace(/^live\./, '')
  if (MEASURE_WORDS[s]) return MEASURE_WORDS[s]
  return s.replace(/([a-z0-9])([A-Z])/g, '$1 $2').replace(/_/g, ' ').toLowerCase()
}

/** One summary panel's legend, prefixed with its panel letter. */
export function summaryLegend(letter: string, f: SummaryLegendFacts): string {
  const pops = [...new Set(f.series.map(s => s.pop))]
  const labels = popDisplayLabels(pops, undefined, { tracks: f.pointKind === 'track' })
  const name = (p: string) => labels.get(p) ?? p
  const images = new Set(f.series.map(s => s.uID ?? '').filter(Boolean)).size
  const m = f.measure.includes('.') || f.measure.includes('_') ? measureWords(f.measure) : f.measure
  const objects = f.pointKind === 'track' ? ' tracks' : f.pointKind === 'cell' ? ' cells' : ''
  const head = `**${letter} — ${f.title || f.plot}.** ${m[0]?.toUpperCase() ?? ''}${m.slice(1)} of ${listWords(pops.map(name))}${objects}` +
    (images > 1 ? `, ${images} images pooled` : '') + '.'
  const parts = [head]
  const chart = CHART_WORDS[f.chart]
  if (chart) parts.push(`${chart}.`)
  // what a point is and n per population
  const noun = f.statUnit === 'individual'
    ? (f.pointKind === 'track' ? { one: 'track', many: 'tracks' } : { one: 'cell', many: 'cells' })
    : f.unitNoun
  const nOf = (p: string) => f.series.filter(s => s.pop === p).reduce((a, s) => a + s.n, 0)
  const ns = pops.map(p => `${name(p)} ${nOf(p)}`)
  const a = (w: string) => (/^[aeiou]/i.test(w) ? 'an ' : 'a ') + w
  const point = f.statUnit === 'individual' ? `Each point is ${a(noun.one)}`
    : `Each point is the ${f.imageAgg} of ${a(noun.one)}`
  parts.push(`${point} (n = ${listWords(ns)} ${noun.many}).`)
  parts.push(f.test ? `${f.test}.` : 'No statistical test.')
  return parts.join(' ')
}

/** A view with no summary facts (gating strategy, a viewer, a heatmap): what it is, of what. */
export function viewLegend(letter: string, o: { label: string; title?: string; valueName?: string; pops?: readonly string[] }): string {
  const pops = o.pops?.length ? [...popDisplayLabels([...o.pops]).values()] : []
  const of = [o.valueName ? `the ${o.valueName} segmentation` : '', pops.length ? listWords(pops) : '']
    .filter(Boolean).join(', ')
  return `**${letter} — ${o.title || o.label}.**` + (of ? ` ${o.label} of ${of}.` : '')
}

/** The board's legends as Markdown, one paragraph per filled panel. */
export function boardLegendsMarkdown(board: string, legends: readonly string[]): string {
  return [`## ${board} — legends`, '', ...legends.flatMap(l => [l, ''])].join('\n')
}
