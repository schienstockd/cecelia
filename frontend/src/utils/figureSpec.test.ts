import { describe, it, expect } from 'vitest'
import { figureSpecFromView, niceFigureBarUm, framesAsRange, applyFigureEdits, figureFrames, figureName, figureTipText, figureViewState, splitRows, isSplit } from './figureSpec'
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
    expect(s.view?.camera.zoom).toBe(8)                   // the snapshot rides along, for open-at-source
  })
  it('refuses what has no single 2D crop', () => {
    expect(figureSpecFromView(null, NAMES)).toEqual({ error: 'Open the image in the viewer first' })
    expect(figureSpecFromView(vs({ dims: { ndisplay: 3, current_step: [0], point: [0] } }), NAMES))
      .toEqual({ error: 'Figures are 2D — switch the viewer to 2D' })
    // …unless the caller frames it itself (an ROI's figure): a 3D view still gives channels + contrast
    const own = figureSpecFromView(vs({ dims: { ndisplay: 3, current_step: [0], point: [0] } }), NAMES, { ownCrop: true })
    expect('spec' in own && own.spec.display.ranges['CD169-Kat']).toEqual([0, 10])
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

describe('framesAsRange', () => {
  it('reads a saved strip back as range + every', () => {
    expect(framesAsRange([60, 64, 68, 72])).toEqual({ lo: 60, hi: 72, every: 4, exact: true })
    expect(framesAsRange([64])).toEqual({ lo: 64, hi: 64, every: null, exact: true })
  })
  it('says auto when auto over that range would pick the same frames', () => {
    expect(framesAsRange(figureFrames(50, 58, null).frames)).toEqual({ lo: 50, hi: 58, every: null, exact: true })
    // an auto strip over 0–180 ends at 161; auto over 0–161 steps differently, so the step is shown
    expect(framesAsRange(figureFrames(0, 180, null).frames)).toEqual({ lo: 0, hi: 161, every: 23, exact: true })
  })
  it('flags a hand-picked list', () => {
    expect(framesAsRange([50, 54, 57, 60]).exact).toBe(false)
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
    // grid on writes the layout, off drops it (the renderer's default strip)
    const g = applyFigureEdits(spec, { grid: true })
    expect((g as Record<string, unknown>).layout).toBe('grid')
    expect(applyFigureEdits(g, { grid: false })).not.toHaveProperty('layout')
  })
})

describe('figureFrames', () => {
  it('takes every Nth frame of the range, starting at its first', () => {
    expect(figureFrames(50, 110, 10)).toEqual({ frames: [50, 60, 70, 80, 90, 100, 110], every: 10, auto: false })
    expect(figureFrames(50, 58, 4).frames).toEqual([50, 54, 58])
    expect(figureFrames(64, 64, 5).frames).toEqual([64])                      // a one-frame range = a still
  })
  it('auto picks the smallest step that keeps a page-width strip', () => {
    expect(figureFrames(0, 180, null)).toEqual({ frames: [0, 23, 46, 69, 92, 115, 138, 161], every: 23, auto: true })
    expect(figureFrames(50, 58, null).frames).toEqual([50, 51, 52, 53, 54, 55, 56, 57, 58])
  })
  it("the view's spec carries the chosen frames", () => {
    const r = figureSpecFromView(vs(), NAMES, { frames: [50, 60] })
    expect('spec' in r && r.spec.frames).toEqual([50, 60])
  })
})

describe('figureName / figureTipText', () => {
  it('names a still by its frame and a strip by its range', () => {
    expect(figureName('img', [64])).toBe('img_t64')
    expect(figureName('img', [50, 60, 110])).toBe('img_t50-110')
    // the viewer's name field, as a movie's suffix; blank adds nothing
    expect(figureName('img', [0, 175], ' ctrl ')).toBe('img_t0-175_ctrl')
    expect(figureName('img', [64], '')).toBe('img_t64')
  })
  it('says which frames the button will take, auto included', () => {
    expect(figureTipText(figureFrames(50, 110, 10))).toBe('Save t50–110, every 10: 7 frames → Movies')
    expect(figureTipText(figureFrames(0, 180, null))).toBe('Save t0–161, every 23 (auto): 8 frames → Movies')
    expect(figureTipText(figureFrames(64, 64, null))).toBe('Save this view as a figure (t64) → Movies')
    expect(figureTipText(null)).toBe('Save this view as a figure → Movies')
  })
})

describe('applyFigureEdits on the editor\'s reactive spec', () => {
  it('copies a Vue reactive proxy (structuredClone cannot)', async () => {
    const { reactive } = await import('vue')
    const spec = reactive({ frames: [64], rows: [{ channels: [{ c: 'mem-TOM', colour: '#ff00ff' }] }] })
    const out = applyFigureEdits(spec, { frames: [50, 64] })
    expect(out.frames).toEqual([50, 64])
    expect(spec.frames).toEqual([64])
  })
})

describe('figureViewState', () => {
  it('uses the snapshot a figure was saved with', () => {
    const v = vs()
    expect(figureViewState({ view: v, center: [1, 1], sizePx: 9 })).toBe(v)
  })
  it("rebuilds a view from a spec without one (Claude's)", () => {
    const r = figureViewState({ center: [597, 361], sizePx: 90, z: 9, zHalf: 1, frames: [64, 80],
      rows: [{ channels: [{ c: 'mem-TOM', colour: '#ff00ff' }] }], display: { ranges: { 'mem-TOM': [0, 68] } } })!
    expect(r.camera.center).toEqual([9, 597, 361])
    expect(r.camera.zoom * 90).toBe(r.canvas.height)          // the crop fills the canvas
    expect(r.dims.current_step.slice(0, 2)).toEqual([64, 9])
    expect(r.dims.zRange).toEqual([8, 10])
    expect(r.layers['mem-TOM']).toEqual({ visible: true, colormap: '#ff00ff', contrast_limits: [0, 68] })
    expect(figureViewState({ rows: [] })).toBeNull()
  })
})

describe('split channels', () => {
  const merge = [{ c: 'mem-TOM', colour: '#ff00ff', name: 'tdTom' }, { c: 'CD169-Kat', colour: '#00ff00' }]
  it('adds one grey row per channel under the merge, keeping legend names', () => {
    const rows = splitRows(merge, true)
    expect(rows.map(r => r.channels.map(c => `${c.c}:${c.colour}`))).toEqual([
      ['mem-TOM:#ff00ff', 'CD169-Kat:#00ff00'], ['mem-TOM:#ffffff'], ['CD169-Kat:#ffffff']])
    expect(rows[1].channels[0].name).toBe('tdTom')
    expect(splitRows(merge, false)).toHaveLength(1)
    expect(splitRows([merge[0]], true)).toHaveLength(1)        // one channel: nothing to split
  })
  it('recognises a split figure, and the editor toggles it', () => {
    expect(isSplit(splitRows(merge, true))).toBe(true)
    expect(isSplit(splitRows(merge, false))).toBe(false)
    const spec = { rows: splitRows(merge, false) }
    expect((applyFigureEdits(spec, { split: true }).rows as unknown[]).length).toBe(3)
    expect((applyFigureEdits(applyFigureEdits(spec, { split: true }), { split: false }).rows as unknown[]).length).toBe(1)
  })
  it('the view can save a split figure', () => {
    const r = figureSpecFromView(vs(), NAMES, { split: true })
    expect('spec' in r && r.spec.rows.length).toBe(3)
  })
})
