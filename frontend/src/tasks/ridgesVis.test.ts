/**
 * The two contracts a session cannot break silently:
 *   1. Ratios stay in a readable range on the schematic — no runaway 1e8× when the background
 *      is genuinely zero (the bug the second review round caught).
 *   2. σ actually reaches the response — sliding the min σ past every ridge width drops the
 *      ratio, which is the whole point of showing σ live.
 * If either regresses, the figure lies at the point of choosing.
 */
import { describe, expect, it } from 'vitest'
import { fibreBgRatio, meijeringResp, ridgeField, ridgesFigure } from './ridgesVis'

describe('ridgesVis', () => {
  it('fibreBgRatio caps at 50× on a spike frame with zero background', () => {
    // The schematic-shaped worst case: field is 0 almost everywhere, a few pixels at 1.
    // Without the floor, top/(bot+1e-9) → ~1e9. With the 2%-of-peak floor, capped at 50.
    const spike = ridgeField().map(r => r.map(() => 0))
    spike[0][0] = 1
    spike[0][1] = 1
    expect(fibreBgRatio(spike)).toBeLessThanOrEqual(50)
    expect(fibreBgRatio(spike)).toBeGreaterThan(0)
  })

  it('fibreBgRatio is 0 for a blank frame', () => {
    const blank = ridgeField().map(r => r.map(() => 0))
    expect(fibreBgRatio(blank)).toBe(0)
  })

  it('σ actually reaches the response — meijeringResp at σ=1 differs from σ=6', () => {
    // The bug this pins: the previous vis called a hardcoded preblur regardless of σ, so the
    // response frames were byte-identical for every σ range. Since σ is a live control on the
    // form now, changing it must change the response — anywhere.
    const f = ridgeField()
    const r1 = meijeringResp(f, 1)
    const r6 = meijeringResp(f, 6)
    let anyDelta = 0
    for (let y = 0; y < r1.length; y++) {
      for (let x = 0; x < r1[0].length; x++) {
        anyDelta += Math.abs(r1[y][x] - r6[y][x])
      }
    }
    expect(anyDelta).toBeGreaterThan(0)
  })

  it('all three columns produce a finite, non-runaway ratio at defaults', () => {
    const { vis } = ridgesFigure({ filter: 'meijering', sigmaMinPx: 1, sigmaMaxPx: 5 })
    const ratioRow = vis.rows.find(r => r.key === 'ratio')!
    for (let i = 1; i <= 3; i++) {
      const m = ratioRow.cells[i].text!.match(/(\d+(?:\.\d+)?)×/)
      const r = m ? Number(m[1]) : NaN
      expect(Number.isFinite(r)).toBe(true)
      expect(r).toBeLessThanOrEqual(50)
    }
  })
})
