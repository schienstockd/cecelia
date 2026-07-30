import { describe, it, expect } from 'vitest'
import { legendTopPad, LEGEND_GAP, LEGEND_ROW, TITLE_PAD } from './plot'

// The legend is absolutely-positioned HTML, so it consumes no layout height and the plot has to leave
// it room explicitly. The old rule ESTIMATED that room as "3 entries per row, at most 3 rows" — and
// that is what made legends look arbitrary: three long labels wrap to two rows, `ceil(3/3)` reserved
// one, and the second row sat on the frame. How many rows a legend wraps to depends on the label texts
// and the panel width, neither of which the option builder can see, so the estimate is only the FIRST
// pass — PlotChart measures the rendered node and re-renders once with the real height.

describe('legendTopPad', () => {
  it('reserves the MEASURED height when it is known — no assumption about entries per row', () => {
    // 3 entries that happened to wrap to two rows: the measurement says 40px, so 40px is reserved,
    // regardless of what an entries-per-row guess would have said
    expect(legendTopPad(3, { legend: true, legendHeight: 40 })).toBe(40 + LEGEND_GAP)
    expect(legendTopPad(3, { legend: true, legendHeight: 20 })).toBe(20 + LEGEND_GAP)
    // …and it scales with however many rows the browser actually produced
    expect(legendTopPad(9, { legend: true, legendHeight: 96 })).toBe(96 + LEGEND_GAP)
  })

  it('rounds a fractional measurement UP — half a pixel short still clips', () => {
    expect(legendTopPad(2, { legend: true, legendHeight: 19.4 })).toBe(20 + LEGEND_GAP)
  })

  it('falls back to a generous estimate before the measurement exists', () => {
    const est = legendTopPad(3, { legend: true })
    expect(est).toBe(8 + LEGEND_ROW + LEGEND_GAP)
    // more entries → more rows, so the first frame is never wildly short
    expect(legendTopPad(7, { legend: true })).toBeGreaterThan(est)
  })

  it('reserves nothing for a legend that is not drawn', () => {
    // one series draws no legend (PlotChart requires domain.length > 1), and the toggle can be off
    expect(legendTopPad(1, { legend: true })).toBe(12)
    expect(legendTopPad(5, { legend: false })).toBe(12)
    expect(legendTopPad(5, { legend: false, legendHeight: 80 })).toBe(12)
  })

  it('a title always gets its line, and never shrinks the legend band', () => {
    expect(legendTopPad(1, { legend: true, title: 'x' })).toBe(TITLE_PAD)
    // a tall legend wins over the title's fixed line rather than being clipped by it
    expect(legendTopPad(4, { legend: true, title: 'x', legendHeight: 60 })).toBe(60 + LEGEND_GAP)
  })

  it('is monotonic in the measured height — a taller legend never reserves less', () => {
    let prev = 0
    for (const h of [10, 20, 40, 80, 160]) {
      const pad = legendTopPad(4, { legend: true, legendHeight: h })
      expect(pad).toBeGreaterThan(prev)
      prev = pad
    }
  })
})
