import { describe, it, expect } from 'vitest'
import { RECORDED, type ViewerDrawSpec } from './drawSpec'
import { viewerLook } from './viewerLook'

// docs/todo/DRAW_SPEC_PLAN.md D4: every field of the draw spec is decided. A `recorded` field must reach
// the movie config (`viewerLook`); any other must not — so wiring a pending field without flipping it,
// or dropping a recorded one from the look, fails here.

const spec: ViewerDrawSpec = {
  viewState: {
    layers: { DAPI: { visible: true, colormap: 'blue' }, GFP: { visible: false, colormap: 'green' } },
    dims: { ndisplay: 2, current_step: [3, 7], point: [3, 7] },
  },
  channelNames: ['DAPI', 'GFP'], nZ: 0, version: 'corrected',
  maskValueName: 'cpSAM', labelFilesHidden: [],
  gating: { valueName: 'flowTom', popType: 'clust' },
  popVisible: {},
  trackVisible: {}, trackSourceColours: {}, showGatedTracks: false, hiddenTrackPops: {},
  popEyes: {}, ribbonEyes: {},
  pointSize: 8, pointBorder: 2, labelOpacity: 0.4, pointZTol: 1, trackZTol: 3, tailWidth: 3, tailLength: 12,
  labelContour: 2, maskColour: 'id', trackColourMode: 'speed', colourBy: '', colourOverrides: {},
  branchVisible: {}, scaleBar: true, timestamp: true, scaleBarPx: 20, timestampPx: 20, steps: 256,
  grid: false, gridDensity: 8, landscape: false, landscapeLabels: false, roiSpotlight: true,
}

const vol = (zRange: [number, number]) =>
  ({ layers: {}, dims: { ndisplay: 3 as const, current_step: [0, 0], point: [0, 0], zRange },
     camera: { center: [0, 0, 0] as [number, number, number], angles: [0, 0, 0] as [number, number, number],
               zoom: 1, perspective: 0 } })

/** Per field: the context it matters in, and a change to it. Typed over every key, so a new field
 *  needs a row here too. */
const PROBES: Record<keyof ViewerDrawSpec, [Partial<ViewerDrawSpec>, Partial<ViewerDrawSpec>]> = {
  viewState: [{}, { viewState: { ...spec.viewState!, layers: { DAPI: { visible: true, colormap: 'blue' },
                                                                GFP: { visible: true, colormap: 'green' } } } }],
  channelNames: [{}, { channelNames: ['Hoechst', 'GFP'] }],
  nZ: [{ viewState: vol([0, 9]) }, { nZ: 10 }],
  version: [{}, { version: 'raw' }],
  maskValueName: [{}, { maskValueName: 'flowTom' }],
  labelFilesHidden: [{}, { labelFilesHidden: ['halo'] }],
  gating: [{ popVisible: { flow: true } }, { gating: { valueName: 'cpSAM', popType: 'flow' } }],
  popVisible: [{}, { popVisible: { flow: true } }],
  trackVisible: [{}, { trackVisible: { flowTom: true } }],
  trackSourceColours: [{}, { trackSourceColours: { flowTom: '#ff0000' } }],
  showGatedTracks: [{}, { showGatedTracks: true }],
  hiddenTrackPops: [{}, { hiddenTrackPops: { flowTom: ['/a'] } }],
  popEyes: [{}, { popEyes: { flowTom: { 'flow::/a': false } } }],
  ribbonEyes: [{}, { ribbonEyes: { flowTom: { 'flow::/a': false } } }],
  pointSize: [{}, { pointSize: 9 }],
  pointBorder: [{}, { pointBorder: 3 }],
  labelOpacity: [{}, { labelOpacity: 0.5 }],
  pointZTol: [{}, { pointZTol: 2 }],
  trackZTol: [{}, { trackZTol: 4 }],
  tailWidth: [{}, { tailWidth: 4 }],
  tailLength: [{}, { tailLength: 13 }],
  labelContour: [{}, { labelContour: 1 }],
  maskColour: [{}, { maskColour: 'trackclust' }],
  trackColourMode: [{}, { trackColourMode: 'solid' }],
  colourBy: [{}, { colourBy: 'clusters' }],
  colourOverrides: [{ colourBy: 'clusters' }, { colourOverrides: { a: '#00ff00' } }],
  branchVisible: [{}, { branchVisible: { skel: false } }],
  scaleBar: [{}, { scaleBar: false }],
  timestamp: [{}, { timestamp: false }],
  scaleBarPx: [{}, { scaleBarPx: 30 }],
  timestampPx: [{}, { timestampPx: 30 }],
  steps: [{}, { steps: 512 }],
  grid: [{}, { grid: true }],
  gridDensity: [{ grid: true }, { gridDensity: 4 }],
  landscape: [{}, { landscape: true }],
  landscapeLabels: [{ landscape: true }, { landscapeLabels: true }],
  roiSpotlight: [{}, { roiSpotlight: false }],
}

describe('draw spec → recording (DRAW_SPEC_PLAN D4)', () => {
  for (const key of Object.keys(RECORDED) as (keyof ViewerDrawSpec)[]) {
    const status = RECORDED[key]
    const recorded = status === 'recorded'
    it(`${key}: ${recorded ? 'reaches' : 'stays out of'} the movie config`, () => {
      const [ctx, change] = PROBES[key]
      const before = viewerLook({ ...spec, ...ctx })
      const after = viewerLook({ ...spec, ...ctx, ...change })
      if (recorded) expect(after).not.toEqual(before)
      else expect(after).toEqual(before)
    })
  }

  it('a not-recorded field says why', () => {
    for (const s of Object.values(RECORDED))
      if (typeof s === 'object') expect(Object.values(s)[0].length).toBeGreaterThan(0)
  })
})

describe('the look’s keys — what the server must read (DRAW_SPEC_PLAN D4)', () => {
  it('match lookKeys.parity.json, which the Julia suite checks against api/src', async () => {
    const keys = new Set<string>()
    for (const [ctx, change] of Object.values(PROBES))
      for (const k of Object.keys(viewerLook({ ...spec, ...ctx, ...change }))) keys.add(k)
    const fx = (await import('./lookKeys.parity.json')).default as { keys: string[] }
    expect([...keys].sort()).toEqual(fx.keys)
  })
})
