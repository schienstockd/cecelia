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

  // Models trained before `zPlanes` are still in people's vaults, and the modal has to keep
  // describing them rather than dropping the row.
  it('spells the middle Z plane out for a pre-zPlanes model', () => {
    expect(fieldsOf({ zSlice: -1 }, 'Input')['Z plane']).toBe('middle')
    expect(fieldsOf({ zSlice: 12 }, 'Input')['Z plane']).toBe('12')
  })

  it('reports the plane count and the indices behind it', () => {
    const one = fieldsOf({ zPlanes: 1, zPlanesUsed: { a: [15], b: [15] } }, 'Input')
    expect(one['Z planes']).toBe('1 (middle)')
    // Every movie agreed, so one list — repeating it per uID would be noise.
    expect(one.Planes).toBe('[15]')
  })

  it('names the movies when they disagree about which planes — "3 planes" is not a depth', () => {
    expect(fieldsOf({ zPlanes: 3, zPlanesUsed: { deep: [5, 15, 25], shallow: [1, 4, 7] } },
                    'Input').Planes)
      .toBe('deep: [5, 15, 25]  shallow: [1, 4, 7]')
  })

  it('says nothing about planes for a 2D model rather than showing an empty row', () => {
    expect(fieldsOf({ zPlanes: 1, zPlanesUsed: {} }, 'Input')).toEqual({ 'Z planes': '1 (middle)' })
  })

  // The frame cap is invisible in `nFrames` — a pooled total cannot say whether a movie was cut or
  // simply short, and the window is seed-derived so it is not recoverable by inspection either.
  it('reports the frame cap, spelling out an uncapped run rather than showing 0', () => {
    expect(fieldsOf({ maxFrames: 0 }, 'Source')['Max frames/movie']).toBe('all')
    expect(fieldsOf({ maxFrames: 50 }, 'Source')['Max frames/movie']).toBe('50')
  })

  it('names the movies that were actually cut, and their windows', () => {
    expect(fieldsOf({ maxFrames: 50, frameWindows: { long: [40, 90] } }, 'Source')['Windows (1)'])
      .toBe('long: 40–89')
  })

  // The XY window and the Z interval are the other two axes of "what did this model actually see",
  // and both are seed-derived like the frame window — so the manifest is the only record, and the
  // modal is where anyone looks for it.
  it('reports the crop, spelling out a whole-frame run rather than showing 0', () => {
    expect(fieldsOf({ cropSize: 0 }, 'Source').Crop).toBe('whole frame')
    expect(fieldsOf({ cropSize: 512 }, 'Source').Crop).toBe('512×512')
  })

  it('counts the random crop windows across every movie and plane', () => {
    expect(fieldsOf({ cropSize: 512, cropWindows: { a: [[1, 2, 512, 512], [9, 9, 512, 512]],
                                                    b: [[3, 4, 512, 512]] } }, 'Source').Crop)
      .toBe('512×512 at random (3 windows)')
  })

  it('shows the Z interval only when one was asked for', () => {
    expect(fieldsOf({ zPlanes: 10, zSpacing: 2 }, 'Input')['Z spacing']).toBe('every 2')
    expect(fieldsOf({ zPlanes: 10, zSpacing: 0 }, 'Input')['Z spacing']).toBeUndefined()
    expect(fieldsOf({ zPlanes: 10 }, 'Input')['Z spacing']).toBeUndefined()
  })

  it('shows no window row when nothing was cut', () => {
    expect(fieldsOf({ maxFrames: 50, frameWindows: {} }, 'Source'))
      .toEqual({ 'Max frames/movie': '50' })
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
