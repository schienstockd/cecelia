import { describe, it, expect } from 'vitest'
import { axisFeatureLabel, axisTitle, type BehaviourAxis } from './behaviourAxes'

// the shape `clustering_utils.behaviour_axes` writes, copied from a real XcPcu8 run's log
const DC1: BehaviourAxis = { name: 'DC1', top: [
  { feature: 'live.track.speed', rho: 0.92 },
  { feature: 'live.cell.hmm.state.movement.3', rho: 0.86 },
  { feature: 'live.cell.hmm.transitions.movement.3_3', rho: 0.85 }] }
const DC2: BehaviourAxis = { name: 'DC2', top: [
  { feature: 'live.cell.hmm.state.movement.2', rho: 0.92 },
  { feature: 'live.cell.hmm.transitions.movement.2_2', rho: 0.89 },
  { feature: 'live.cell.hmm.transitions.movement.3_3', rho: -0.49 }] }

describe('behaviour axis titles', () => {
  it('shortens track and HMM columns', () => {
    expect(axisFeatureLabel('live.track.straightness')).toBe('straightness')
    expect(axisFeatureLabel('live.cell.hmm.state.movement.1')).toBe('HMM state 1')
    expect(axisFeatureLabel('live.cell.hmm.transitions.movement.2_3')).toBe('HMM 2→3')
    expect(axisFeatureLabel('mean_intensity_0.mean')).toBe('mean_intensity_0.mean')
    expect(axisFeatureLabel('mean_intensity_0', { mean_intensity_0: 'CD3' })).toBe('CD3')
  })

  it('names an axis by its top two features, marking a negative one', () => {
    expect(axisTitle(DC1, 'DC1')).toBe('DC1: speed, HMM state 3')
    expect(axisTitle(DC2, 'DC2', {}, 3)).toBe('DC2: HMM state 2, HMM 2→2, −HMM 3→3')
  })

  it('falls back when the run recorded no axes', () => {
    expect(axisTitle(undefined, 'DC1')).toBe('DC1')
    expect(axisTitle({ name: 'DC1', top: [] }, 'DC1')).toBe('DC1')
  })
})
