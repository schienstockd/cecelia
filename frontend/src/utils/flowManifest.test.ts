import { describe, it, expect } from 'vitest'
import { modelDetailGroups, type FlowManifest } from './flowManifest'

const groupNames = (m: FlowManifest | null) => modelDetailGroups(m).map(g => g.label)
const fieldsOf = (m: FlowManifest, label: string) =>
  Object.fromEntries((modelDetailGroups(m).find(g => g.label === label)?.fields ?? [])
    .map(f => [f.label, f.value]))

describe('modelDetailGroups', () => {
  it('is empty with no manifest, so the caller can say so in its own words', () => {
    expect(groupNames(null)).toEqual([])
    expect(groupNames({})).toEqual([])
  })

  it('drops a group with nothing in it rather than showing a header over dashes', () => {
    expect(groupNames({ epochs: 30 })).toEqual(['Training'])
  })

  // The one that matters: the manifest IS the contract inference configures itself from, so a key
  // this file has never heard of can still change what a model does. Dropping it would hide that.
  it('shows an unknown key under Other instead of dropping it', () => {
    const g = fieldsOf({ epochs: 30, someLaterField: 'v2' } as FlowManifest, 'Other')
    expect(g).toEqual({ someLaterField: 'v2' })
  })

  it('does not dump the loss curves into Other — they are the convergence plot', () => {
    expect(groupNames({ lossCurves: { total: [3, 2] } })).toEqual([])
  })

  it('spells the middle Z plane out', () => {
    expect(fieldsOf({ zSlice: -1 }, 'Input')['Z plane']).toBe('middle')
    expect(fieldsOf({ zSlice: 12 }, 'Input')['Z plane']).toBe('12')
  })

  it('says "none" when a model kept every metric, and lists them when it did not', () => {
    expect(fieldsOf({ metricKeys: ['mag_1', 'strain'] }, 'Flow metrics')).toEqual({
      'Planes read': '2', Set: 'mag_1, strain', Excluded: 'none',
    })
    expect(fieldsOf({ metricKeys: ['mag_1'], droppedMetrics: ['vorticity'] }, 'Flow metrics').Excluded)
      .toBe('vorticity')
  })

  it('lists a loss weight per term, including the ones switched off', () => {
    expect(fieldsOf({ lossWeights: { temporal: 2, variance: 0 } }, 'Training')).toEqual({
      'temporal weight': '2', 'variance weight': '0',
    })
  })

  it('counts the source images in the label so a long list is still readable', () => {
    expect(Object.keys(fieldsOf({ sourceImages: ['a', 'b', 'c'] }, 'Source')))
      .toEqual(['Images (3)'])
  })

  it('falls back to the channel indices when no channel name was recorded', () => {
    expect(fieldsOf({ trainChannels: [0, 2] }, 'Input').Channels).toBe('0, 2')
    expect(fieldsOf({ channelName: 'GFP+RFP', trainChannels: [0, 2] }, 'Input').Channels)
      .toBe('GFP+RFP')
  })
})
