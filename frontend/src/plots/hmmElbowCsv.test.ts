import { describe, it, expect } from 'vitest'
import { plotDataToCsv } from './plot'
import type { PlotDataResponse } from './types'

describe('plotDataToCsv — HMM state count (elbow)', () => {
  it('writes one row per recorded run with its family and fit numbers', () => {
    const r = {
      chartType: 'elbow', measure: 'logLik', granularity: 'cell', series: [],
      curves: [{ key: 'a', label: 'speed, angle · T/_tracked', pops: ['T/_tracked'],
                 modelMeasurements: ['live.cell.speed', 'live.cell.angle'],
                 points: [{ k: 2, run: 'k2', logLik: -10, aic: 20, bic: 25, gain: null, converged: true, nObs: 50 },
                          { k: 3, run: 'k3', logLik: -8, aic: 18, bic: 26, gain: 2, converged: false, nObs: 50 }] }],
    } as unknown as PlotDataResponse
    const lines = plotDataToCsv(r).split('\n')
    expect(lines[0]).toBe('family,pops,modelMeasurements,run,numStates,logLik,aic,bic,gainOverPrevK,converged,nObs')
    expect(lines[1]).toBe('"speed, angle · T/_tracked",T/_tracked,live.cell.speed live.cell.angle,k2,2,-10,20,25,,true,50')
    expect(lines[2]).toBe('"speed, angle · T/_tracked",T/_tracked,live.cell.speed live.cell.angle,k3,3,-8,18,26,2,false,50')
  })
})
