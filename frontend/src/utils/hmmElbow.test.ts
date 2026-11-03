import { describe, it, expect } from 'vitest'
import { elbowRows, type HmmFitCurve } from './hmmElbow'

const curve: HmmFitCurve = {
  key: 'a', label: 'speed, angle · T/_tracked',
  points: [
    { k: 3, run: 'k3', logLik: -24106.6, bic: 48377.3, converged: true, gain: 964.2 },
    { k: 2, run: 'k2', logLik: -25070.8, bic: 50231.8, converged: true, gain: null },
    { k: 4, run: 'k4', logLik: -23570.7, bic: null, converged: false, gain: 535.9 },
  ],
}

describe('elbowRows', () => {
  it('orders by state count and reads the gain into the tooltip', () => {
    const r = elbowRows([curve], 'logLik')
    expect(r.map(x => x.k)).toEqual([2, 3, 4])
    expect(r[1].tip).toBe('k3: K=3 · Log-likelihood −24,107, +964 vs K−1')
    expect(r[0].tip).toBe('k2: K=2 · Log-likelihood −25,071')
    expect(r[2].converged).toBe(false)
    expect(r[2].tip).toMatch(/not converged$/)
  })
  it('drops points without the chosen metric and leaves the gain off BIC', () => {
    const r = elbowRows([curve], 'bic')
    expect(r.map(x => x.k)).toEqual([2, 3])
    expect(r[1].tip).toBe('k3: K=3 · BIC 48,377')
  })
  it('is empty without curves', () => {
    expect(elbowRows(undefined, 'logLik')).toEqual([])
  })
})
