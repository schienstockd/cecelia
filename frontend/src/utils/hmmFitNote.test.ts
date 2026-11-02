import { describe, it, expect } from 'vitest'
import { hmmFitNote } from './hmmFitNote'

describe('hmmFitNote', () => {
  it('names the modelled measures and the state count', () => {
    const n = hmmFitNote({ modelMeasurements: ['live.cell.speed', 'live.cell.angle'], numStates: 3,
                           logLik: -277186.27, bic: 554600.97, converged: true })
    expect(n).toEqual({ text: 'Fitted on speed, angle · 3 states',
                        tip: 'Log-likelihood −277,186 · BIC 554,601', tone: 'muted' })
  })
  it('warns when the fit did not converge', () => {
    const n = hmmFitNote({ modelMeasurements: ['live.cell.speed'], numStates: 5, logLik: -1, bic: 2, converged: false })
    expect(n?.tone).toBe('warn')
    expect(n?.text).toBe('Not converged · Fitted on speed · 5 states')
    expect(n?.tip).toMatch(/fewer states/)
  })
  it('says nothing for a run with no fit record', () => {
    expect(hmmFitNote(undefined)).toBeNull()
    expect(hmmFitNote({})).toBeNull()
  })
  it('tolerates missing numbers', () => {
    expect(hmmFitNote({ numStates: 2 })?.tip).toBe('Log-likelihood – · BIC –')
  })
})
