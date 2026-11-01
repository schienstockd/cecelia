import { describe, it, expect } from 'vitest'
import { figureSpecFromView, niceFigureBarUm, parseFrames, applyFigureEdits } from './figureSpec'
import type { ViewerViewState } from './viewer/viewState'

const vs = (over: Partial<ViewerViewState> = {}): ViewerViewState => ({
  camera: { center: [9, 597.4, 361.2], zoom: 8, angles: [0, 0, 0], perspective: 0 },
  dims: { ndisplay: 2, current_step: [64, 9, 0, 0], point: [64, 9, 0, 0], zRange: [8, 10] },
  layers: {
    'mem-TOM': { visible: true, contrast_limits: [0, 68], colormap: 'magenta' },
    'CD169-Kat': { visible: true, contrast_limits: [0, 10], colormap: '#00ff00' },
    SHG: { visible: false, contrast_limits: [0, 1], colormap: 'gray' },
  },
  canvas: { width: 1000, height: 720 },
  ...over,
})
const NAMES = ['SHG', 'nuc-GFP', 'mem-TOM', 'CD169-Kat']

describe('figureSpecFromView', () => {
  it('turns the view into a crop, a z window, a frame and the visible channels', () => {
    const r = figureSpecFromView(vs(), NAMES, { valueName: 'denoised', scaleBarUm: 5 })
    expect('spec' in r).toBe(true)
    if (!('spec' in r)) return
    const s = r.spec
    expect(s.center).toEqual([597, 361])
    expect(s.sizePx).toBe(90)                       // 720 px canvas at zoom 8 → 90 image px (the shorter side)
    expect([s.z, s.zHalf, s.frames]).toEqual([9, 1, [64]])
    expect(s.rows[0].channels.map(c => c.c)).toEqual(['mem-TOM', 'CD169-Kat'])   // image order, hidden dropped
    expect(s.rows[0].channels[0].colour).toMatch(/^#[0-9a-f]{6}$/i)               // a name becomes a hex
    expect(s.display.ranges['CD169-Kat']).toEqual([0, 10])                        // the on-screen contrast
    expect(s.valueName).toBe('denoised')
  })
  it('refuses what has no single 2D crop', () => {
    expect(figureSpecFromView(null, NAMES)).toEqual({ error: 'Open the image in the viewer first' })
    expect(figureSpecFromView(vs({ dims: { ndisplay: 3, current_step: [0], point: [0] } }), NAMES))
      .toEqual({ error: 'Figures are 2D — switch the viewer to 2D' })
    const dark = vs(); for (const l of Object.values(dark.layers)) l.visible = false
    expect(figureSpecFromView(dark, NAMES)).toEqual({ error: 'No channel is visible' })
  })
})

describe('niceFigureBarUm', () => {
  it('picks a round length about a fifth of the crop', () => {
    expect(niceFigureBarUm(90, 0.3315)).toBe(5)     // 30 µm crop → 5 µm
    expect(niceFigureBarUm(1000, 0.5)).toBe(100)
    expect(niceFigureBarUm(90, null)).toBeUndefined()
  })
})

describe('parseFrames', () => {
  it('reads lists and stepped ranges', () => {
    expect(parseFrames('50, 54 57')).toEqual({ frames: [50, 54, 57] })
    expect(parseFrames('50-110:20, 60')).toEqual({ frames: [50, 60, 70, 90, 110] })
    expect(parseFrames('64')).toEqual({ frames: [64] })
  })
  it('names what it cannot read', () => {
    expect(parseFrames('50, t60')).toEqual({ error: '"t60" is not a frame or a range (50-110:10)' })
    expect(parseFrames('90-50')).toHaveProperty('error')
    expect(parseFrames(' ')).toEqual({ error: 'No frames' })
  })
})

describe('applyFigureEdits', () => {
  const spec = { frames: [64], rows: [{ channels: [{ c: 'mem-TOM', colour: '#ff00ff' }, { c: 'CD169-Kat', colour: '#00ff00', name: 'Kat' }] }],
                 scaleBarUm: 5 }
  it('changes only what was edited, never the stored spec', () => {
    const out = applyFigureEdits(spec, { frames: [50, 64], names: { 'mem-TOM': ' tdTom ', 'CD169-Kat': '' },
                                         tileMm: 40, interpolation: 'smooth' })
    expect(out.frames).toEqual([50, 64])
    expect(out.rows[0].channels.map(c => (c as { name?: string }).name)).toEqual(['tdTom', undefined])
    expect((out as Record<string, unknown>).style).toEqual({ tileMm: 40 })
    expect(spec.frames).toEqual([64])                                   // the input is untouched
    expect(applyFigureEdits(spec, { scaleBarUm: null })).not.toHaveProperty('scaleBarUm')
  })
})
