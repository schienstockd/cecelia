// The legend overlay in a real browser: how many rows it wraps to is CSS (style.css
// `.plot-legend-overlay`), so only a laid-out page can say. Four short labels in a ~500 px panel fit on
// one row; the legend used to be capped at 58% of the panel whether or not a title sat beside it, so
// they broke into two rows with half the panel empty.
import { describe, expect, it } from 'vitest'
import * as Plot from '@observablehq/plot'
import { legendOverlay, titleOverlay } from './overlays'
import { frames } from '../test/browser/mount'

const LABELS = ['long meandering', 'fast high-turning', 'straight short slow', 'fast straight']
const COLOURS = ['#4477aa', '#cc8800', '#aa99ee', '#cc33cc']

function panel(width: number, title?: string) {
  const host = document.createElement('div')
  host.style.cssText = `position:relative;width:${width}px;height:300px`
  document.body.appendChild(host)
  const legend = legendOverlay(Plot, { domain: LABELS, range: COLOURS }, '#111')!
  host.append(legend)                                   // PlotChart's order: legend, then title
  if (title) host.append(titleOverlay(title, '#111'))
  return { host, legend }
}

function rows(legend: HTMLElement): number {
  const swatches = [...legend.children].filter(c => c.tagName.toLowerCase() !== 'style')
  return new Set(swatches.map(s => Math.round(s.getBoundingClientRect().top))).size
}

describe('plot legend overlay', () => {
  it('stays on one row when the panel has room and there is no title', async () => {
    const { host, legend } = panel(500)
    await frames()
    expect(rows(legend)).toBe(1)
    host.remove()
  })

  it('still wraps when the panel is too narrow', async () => {
    const { host, legend } = panel(260)
    await frames()
    expect(rows(legend)).toBeGreaterThan(1)
    host.remove()
  })

  it('keeps clear of a title overlay', async () => {
    const { host, legend } = panel(510, 'Population summary')
    await frames()
    expect(legend.getBoundingClientRect().width).toBeLessThanOrEqual(510 * 0.58 + 1)
    host.remove()
  })
})
