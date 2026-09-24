// What a summary plot SHOWS, as a small text table — the numbers on screen, for an assistant handed the
// plot (Kiwi's "Add to Kiwi"). The panel builds it from the `/api/plot_data` response it just drew and
// ships it with its registry entry (`usePlotRegistry` → `summary`); the backend puts it in Kiwi's
// prompt when the plot is attached.
//
// Why from the response and not recomputed: the first real Kiwi turns (4kS67f, 2026-09-24) had no way
// to read an attached plot, so Kiwi rebuilt its own numbers image by image — 2 of the 7 images, per-image
// medians the plot never showed — and the replies didn't match the picture. This is the picture.
//
// One row per series (a population, per image when the plot is per image, per group when grouped).
// The statistic is whatever the chart drew: quartiles for box / violin / strip, mean ± sd for bars,
// category shares for a frequency chart, the count otherwise. A stats test the panel ran rides along.
// Capped (`SUMMARY_MAX_CHARS`) — a board of 40 images × 3 states stays a readable prompt, and says so
// when it was cut. Pure ⇒ tested.

import type { PlotDataResponse, PlotSeries } from '../plots/types'

export const SUMMARY_MAX_CHARS = 12_000

const num = (v: number | undefined): string =>
  v == null || !Number.isFinite(v) ? '' : Math.abs(v) >= 100 ? v.toFixed(0) : Math.abs(v) >= 1 ? v.toFixed(2) : v.toPrecision(3)

function statOf(s: PlotSeries, categories: string[] | undefined): string {
  if (s.median != null) return `n=${s.n ?? ''} median=${num(s.median)} q1=${num(s.q1)} q3=${num(s.q3)}`
  if (s.value != null)  return `n=${s.n ?? ''} mean=${num(s.value)}${s.sd != null ? ` sd=${num(s.sd)}` : ''}`
  if (s.values && categories?.length) {
    return categories.map((c, i) => `${c}=${num(s.values![i])}`).join(' ')
  }
  if (s.counts) return `n=${s.counts.reduce((a, b) => a + b, 0)}`
  return s.n != null ? `n=${s.n}` : ''
}

/** The plot as text. `imageName(uid)` names an image (the uid rides along — refs cite it). */
export function plotSummaryText(r: PlotDataResponse, imageName: (uid: string) => string = u => u): string {
  if (r.chartType === 'matrix' || !r.series?.length) return ''
  const head = [`measure: ${r.measure || '(population counts)'}`, `chart: ${r.chartType}`,
                r.scope ? `scope: ${r.scope === 'summarised' ? 'pooled across images' : 'one series per image'}` : '',
                r.groupBy ? `grouped by: ${r.groupBy}` : ''].filter(Boolean)
  const lines = [head.join(' · '), 'population | image | group | values']
  for (const s of r.series) {
    const img = s.uID ? `${imageName(s.uID)} (${s.uID})` : 'all'
    const pop = s.pop.startsWith(s.value_name) ? s.pop : `${s.value_name}${s.pop}`   // `pop` is manager-form (vn + path)
    lines.push(`${pop} | ${img} | ${s.group || '-'} | ${statOf(s, r.categories)}`)
  }
  const c = r.comparisons
  if (c) {
    lines.push(`stats: ${c.methodNote || c.test} across ${c.groups.join(', ')} → p=${num(c.pValue)} (${c.significance})`)
    for (const p of c.comparisonPairs ?? []) lines.push(`  ${p.a} vs ${p.b}: p_adj=${num(p.pAdj)} (${p.significance})`)
  }
  let out = lines.join('\n')
  if (out.length > SUMMARY_MAX_CHARS) {
    const cut = out.lastIndexOf('\n', SUMMARY_MAX_CHARS)
    out = `${out.slice(0, cut)}\n… cut: ${r.series.length} series in all`
  }
  return out
}
