import { describe, expect, it } from 'vitest'
import {
  lensRadiusRangeUm, lensRadiusUm, lensRadiusWheel, lensFetchChannels, lensChipPops, lensChipCounts, lensCircleL0, lensForRecording, lensPopCounts, lensRadiusCss, lensScreenAt, lensUniform, pointerToolChips, lensFocusView,
  LENS_RADIUS_MIN, LENS_RIM_PX, LENS_UM_CAPS, lensCountLayer,
} from './channelLens'
import { VIEW_HALF_ANGLE } from '../volumeViewer'
import { isProxy, reactive } from 'vue'
import type { ViewerViewState } from './viewState'

describe('pointerToolChips', () => {
  it('switching the lens on while the readout is on drops the readout', () => {
    expect(pointerToolChips(['scaleBar', 'readout'], ['scaleBar', 'readout', 'lens'])).toEqual(['scaleBar', 'lens'])
  })
  it('switching the readout on while the lens is on drops the lens', () => {
    expect(pointerToolChips(['lens'], ['lens', 'readout'])).toEqual(['readout'])
  })
  it('switching a tool off, or another chip, passes through', () => {
    expect(pointerToolChips(['lens', 'scaleBar'], ['scaleBar'])).toEqual(['scaleBar'])
    expect(pointerToolChips(['lens'], ['lens', 'timestamp'])).toEqual(['lens', 'timestamp'])
  })
})

describe('lensUniform', () => {
  it('is device px: a pointer at CSS (100, 50) on a 2× screen sits at (200, 100)', () => {
    expect(lensUniform({ cx: 100, cy: 50 }, 120, 2)).toEqual({ x: 200, y: 100, r: 240, rimPx: LENS_RIM_PX * 2 })
  })
  it('no pointer yet = no lens', () => {
    expect(lensUniform(null, 120, 1)).toBeNull()
  })
  it('a nonsense dpr reads as 1', () => {
    expect(lensUniform({ cx: 10, cy: 20 }, 120, 0)?.x).toBe(10)
  })
})

describe('the lens radius, µm on the tissue', () => {
  // short side 1000 px × 1 µm = 1000 µm: steps of 5 (1000 / 400 = 2.5 → 5), 1% … 50%, default 10%
  const range = lensRadiusRangeUm({ nX: 2000, nY: 1000, voxelUm: [1, 1, 1] })
  it('the slider spans 1% to half the short side, starting at a tenth', () => {
    expect(range).toEqual({ min: 10, max: 500, step: 5, def: 100 })
  })
  it('a whole slide is capped at a tissue scale, not 1–50% of 5.8 mm', () => {
    // a 5760 µm short side gave 60–2880 µm before the caps
    expect(lensRadiusRangeUm({ nX: 16000, nY: 11520, voxelUm: [0.5, 0.5, 1] }))
      .toEqual({ min: LENS_UM_CAPS.min, max: LENS_UM_CAPS.max, step: 5, def: LENS_UM_CAPS.def })
  })
  it('follows the pixel size: the same pixels at 0.5 µm halve every bound', () => {
    expect(lensRadiusRangeUm({ nX: 2000, nY: 1000, voxelUm: [0.5, 0.5, 1] })).toMatchObject({ max: 250, def: 50 })
  })
  it('unset takes the image default; a setting from a bigger image is held in this one', () => {
    expect(lensRadiusUm(0, range)).toBe(100)
    expect(lensRadiusUm(NaN, range)).toBe(100)
    expect(lensRadiusUm(1e6, range)).toBe(500)
    expect(lensRadiusUm(3, range)).toBe(10)
    expect(lensRadiusUm(250, range)).toBe(250)
  })
  it('Alt+wheel moves 10% a notch on the slider steps, at least one step, inside the range', () => {
    expect(lensRadiusWheel(100, true, range)).toBe(110)
    expect(lensRadiusWheel(100, false, range)).toBe(90)
    expect(lensRadiusWheel(10, false, range)).toBe(10)
    expect(lensRadiusWheel(500, true, range)).toBe(500)
    expect(lensRadiusWheel(20, true, range)).toBe(25)
  })
})

describe('lensFetchChannels', () => {
  const chs = [
    { visible: true, lens: false },    // 0: context
    { visible: false, lens: true },    // 1: lens only
    { visible: true, lens: true },     // 2: both
    { visible: false, lens: false },   // 3: neither
  ]
  it('lens on: the visible channels and the lens ones, each once, in order', () => {
    expect(lensFetchChannels(chs, true)).toEqual([0, 1, 2])
  })
  it('lens off: only the visible ones — a lens-only channel costs nothing', () => {
    expect(lensFetchChannels(chs, false)).toEqual([0, 2])
  })
  it('stops at the cap', () => {
    expect(lensFetchChannels(chs, true, 2)).toEqual([0, 1])
  })
})

describe('lensForRecording', () => {
  const chs = [{ name: 'DAPI', lens: false }, { name: 'CD3', lens: true }, { name: 'CD20', lens: true }]
  // A 1000 × 500 µm image (1 µm px) on an 800 × 400 canvas, framed exactly: 1.25 image px per CSS px.
  const meta = { nX: 1000, nY: 500, voxelUm: [1, 1, 1] as [number, number, number] }
  const cam = { panX: 0, panY: 0, dist: 250 / VIEW_HALF_ANGLE, yaw: 0, pitch: 0 }
  it('pins to the tissue: level-0 image px for the centre and the radius; lens channels by name', () => {
    expect(lensForRecording({ cx: 200, cy: 100 }, 120, 800, 400, cam, meta, chs))
      .toEqual({ x: 250, y: 125, r: 150, channels: ['CD3', 'CD20'] })
  })
  it('a pan moves the image under the pointer, so the same screen spot names other tissue', () => {
    const panned = { ...cam, panX: 100 }   // the view centre moves 100 µm right
    expect(lensForRecording({ cx: 200, cy: 100 }, 120, 800, 400, panned, meta, chs)?.x).toBe(350)
  })
  it('nothing to record without a pointer position', () => {
    expect(lensForRecording(null, 120, 800, 400, cam, meta, chs)).toBeNull()
  })
  it('no channel picked records the spotlight lens: no channels, still placed', () => {
    expect(lensForRecording({ cx: 200, cy: 100 }, 120, 800, 400, cam, meta, [{ name: 'DAPI', lens: false }]))
      .toEqual({ x: 250, y: 125, r: 150, channels: [] })
  })
})

describe('lensPopCounts', () => {
  // 0.5 µm px: the circle at L0 (20, 20) r 10 is µm (10, 10) r 5.
  const vox: [number, number, number] = [0.5, 0.5, 2]
  const circle = { x: 20, y: 20, r: 10 }
  const layer = (hidden: string[] = []) => ({
    vn: 'cyto',
    payload: {
      cells: {
        label: [1, 2, 3, 4, 5],
        x: [10, 13, 10, 30, 10],       // 4 is far right
        y: [10, 10, 14, 10, 10],
        z: [0, 0, 0, 0, 8],            // 5 is on plane 4
        t: [0, 0, 0, 0, 0],
      },
      pops: [
        { path: '/T', name: 'T', colour: '#f00', labels: [1, 2, 4] },
        { path: '/B', name: 'B', colour: '#00f', labels: [3, 5] },
      ],
    },
    hidden: new Set(hidden),
  })
  it('counts each drawn population\'s cells inside the circle, on the plane shown', () => {
    expect(lensPopCounts([layer()], circle, vox, 0, [0, 1])).toEqual({
      cells: 3, members: 3,
      pops: [{ key: 'cyto|/T', name: 'T', colour: '#f00', n: 2 }, { key: 'cyto|/B', name: 'B', colour: '#00f', n: 1 }],
    })
  })
  it('a hidden population is left out; the cell total still counts its cells', () => {
    const got = lensPopCounts([layer(['/B'])], circle, vox, 0, [0, 1])
    expect(got.pops.map(p => p.name)).toEqual(['T'])
    expect(got.cells).toBe(3)
  })
  it('another frame, or another plane, holds none of them', () => {
    expect(lensPopCounts([layer()], circle, vox, 1, [0, 1]).cells).toBe(0)
    expect(lensPopCounts([layer()], circle, vox, 0, [4, 4]).pops.map(p => p.n)).toEqual([0, 1])
  })
  it('two pop types of one segmentation count its cells once', () => {
    expect(lensPopCounts([layer(), layer()], circle, vox, 0, [0, 1]).cells).toBe(3)
  })
})

describe('lensChipCounts — cell pops, track clusters and regions each their own group', () => {
  // 1 µm px, circle at (20, 20) r 10. Cells 1–4 inside, 5 outside; all on t 0, plane 0.
  const vox = [1, 1, 1]
  const circle = { x: 20, y: 20, r: 10 }
  const cells = { label: [1, 2, 3, 4, 5], x: [20, 22, 18, 20, 40], y: [20, 20, 20, 25, 20], z: [0, 0, 0, 0, 0], t: [0, 0, 0, 0, 0] }
  const lay = (pops: { path: string; labels: number[] }[], hidden: string[] = []) => ({
    vn: 'cyto', hidden: new Set(hidden),
    payload: { cells, pops: pops.map(p => ({ ...p, name: p.path.slice(1), colour: '#123' })) },
  })
  const src = {
    pops: [lay([{ path: '/T', labels: [1, 2, 5] }])],
    // every cell in a region: 1, 2 in A; 3, 4, 5 in B
    regions: [lay([{ path: '/A', labels: [1, 2] }, { path: '/B', labels: [3, 4, 5] }])],
    // tracks clustered: cells 1, 3 fast; 2 slow; 4 in no cluster; 5 fast but outside
    tracks: [lay([{ path: '/fast', labels: [1, 3, 5] }, { path: '/slow', labels: [2] }])],
  }
  it('cell pops are shares of the cells inside; regions too, in a group of their own', () => {
    const got = lensChipCounts(src, circle, vox, 0, [0, 0])
    expect(got.cells).toBe(4)
    expect(got.shown.map(p => [p.name, p.n, p.share])).toEqual([['T', 2, '50%']])
    const reg = got.groups.find(g => g.kind === 'regions')!
    expect(reg.of).toBe(4)
    expect(reg.shown.map(p => [p.name, p.n, p.share])).toEqual([['A', 2, '50%'], ['B', 2, '50%']])
  })
  it('track clusters are shares of the clustered tracks inside, a hidden cluster still in the total', () => {
    const tr = lensChipCounts(src, circle, vox, 0, [0, 0]).groups.find(g => g.kind === 'tracks')!
    expect(tr.of).toBe(3)   // cells 1, 2, 3 — cell 4 is in no cluster
    expect(tr.shown.map(p => [p.name, p.n, p.share])).toEqual([['fast', 2, '67%'], ['slow', 1, '33%']])
    const hid = lensChipCounts({ ...src, tracks: [lay(src.tracks[0].payload.pops, ['/slow'])] }, circle, vox, 0, [0, 0])
      .groups.find(g => g.kind === 'tracks')!
    expect([hid.of, hid.shown.map(p => p.name)]).toEqual([3, ['fast']])
  })
  it('groups in order tracks, regions; none without their layers; no cell layer = no cell count', () => {
    expect(lensChipCounts(src, circle, vox, 0, [0, 0]).groups.map(g => g.kind)).toEqual(['tracks', 'regions'])
    const only = lensChipCounts({ pops: [], regions: [], tracks: src.tracks }, circle, vox, 0, [0, 0])
    expect([only.cells, only.shown, only.groups.map(g => g.kind)]).toEqual([null, [], ['tracks']])
    const none = lensChipCounts({ ...src, tracks: [] }, circle, vox, 1, [0, 0])
    expect([none.cells, none.groups]).toEqual([0, []])   // nothing inside on t 1: no empty region heading
  })
  it('a segmentation in both the cell and region layers counts its cells once', () => {
    expect(lensChipCounts({ ...src, tracks: [] }, circle, vox, 0, [0, 0]).cells).toBe(4)
  })
})

describe('lensChipPops', () => {
  const pop = (name: string, n: number) => ({ key: name, name, colour: '#000', n })
  it('fullest first, empty ones left off, capped with the rest counted; each with its share', () => {
    expect(lensChipPops([pop('a', 1), pop('b', 0), pop('c', 5), pop('d', 3)], 10, 2))
      .toEqual({ shown: [{ ...pop('c', 5), share: '50%' }, { ...pop('d', 3), share: '30%' }], more: 1 })
  })
  it('a share rounds to a whole percent; a few cells of many read <1%, never 0%', () => {
    expect(lensChipPops([pop('a', 2)], 3).shown[0].share).toBe('67%')
    expect(lensChipPops([pop('a', 1)], 470).shown[0].share).toBe('<1%')
  })
})

describe('a pinned lens', () => {
  const meta = { nX: 1000, nY: 500, voxelUm: [1, 1, 1] as [number, number, number] }
  const cam = { panX: 0, panY: 0, dist: 250 / VIEW_HALF_ANGLE, yaw: 0, pitch: 0 }
  it('lands back where it was pinned: the pointer → tissue → canvas round trip', () => {
    const c = lensCircleL0({ cx: 200, cy: 100 }, 120, 800, 400, cam, meta)!
    expect(lensScreenAt(c, cam, meta, 800, 400)).toEqual({ cx: 200, cy: 100 })
  })
  it('moves with the tissue: a pan of 100 µm right shifts it 80 CSS px left (1.25 µm per px)', () => {
    const at = lensScreenAt({ x: 250, y: 125 }, { ...cam, panX: 100 }, meta, 800, 400)
    expect(at.cx).toBeCloseTo(120)
    expect(at.cy).toBeCloseTo(100)
  })
  it('a radius in tissue px reads back in CSS px, unbounded: it grows with the tissue it covers', () => {
    expect(lensRadiusCss(150, cam, meta, 800, 400)).toBe(120)
    expect(lensRadiusCss(1500, cam, meta, 800, 400)).toBe(1200)
  })
  it('zooming in 2× draws the same tissue radius twice as big — the lens keeps what it holds', () => {
    expect(lensRadiusCss(150, { ...cam, dist: cam.dist / 2 }, meta, 800, 400)).toBeCloseTo(240)
  })
})

describe('lensChipPops names', () => {
  it('a name two populations share takes its parent', () => {
    const p = (path: string, n: number) => ({ key: `cyto|${path}`, name: path.split('/').pop()!, colour: '#000', n })
    expect(lensChipPops([p('/T/CD4', 3), p('/NK/CD4', 2), p('/B', 1)], 6).shown.map(x => x.name))
      .toEqual(['T/CD4', 'NK/CD4', 'B'])
  })
})

describe('lensFocusView — a Kiwi ref reopening its pinned lens', () => {
  const view = (zoom: number): ViewerViewState => ({
    camera: { center: [0, 100, 200], zoom, angles: [0, 0, 0], perspective: 0 },
    dims: { ndisplay: 2, current_step: [7, 2], point: [7, 2] },
    layers: {}, canvas: { width: 800, height: 600 },
  })
  it('centres on the lens at the ref\'s t', () => {
    const v = lensFocusView(view(1), { x: 500, y: 300, r: 100 }, 3)!
    expect(v.camera.center.slice(1)).toEqual([300, 500])
    expect(v.dims.current_step[0]).toBe(3)
  })
  it('keeps a zoom at which the lens already fits the radius slider', () => {
    // r 100 level-0 px at zoom 1 = 100 canvas px: inside [MIN, 40% of 600]
    expect(lensFocusView(view(1), { x: 0, y: 0, r: 100 }, 0)!.camera.zoom).toBe(1)
  })
  it('zooms in until a lens too small to draw reaches the smallest radius', () => {
    // r 10 at zoom 1 = 10 px < MIN → zoom so 10 level-0 px span MIN canvas px
    expect(lensFocusView(view(1), { x: 0, y: 0, r: 10 }, 0)!.camera.zoom).toBeCloseTo(LENS_RADIUS_MIN / 10)
  })
  it('zooms out until a lens too big fits 40% of the canvas height', () => {
    // r 1000 at zoom 1 = 1000 px > min(MAX, 240) → zoom 240 / 1000
    expect(lensFocusView(view(1), { x: 0, y: 0, r: 1000 }, 0)!.camera.zoom).toBeCloseTo(0.4 * 600 / 1000)
  })
  it('is null with no view to start from', () => {
    expect(lensFocusView(null, { x: 0, y: 0, r: 10 }, 0)).toBeNull()
  })
})

describe('lensCountLayer', () => {
  it('hands the count plain arrays, never Vue proxies (a proxied read was ~25x slower per pointer move)', () => {
    const live = reactive({ vn: 'cells', payload: { cells: { label: [1, 2], x: [0, 5], y: [0, 5] },
      pops: [{ path: '/a', name: 'a', colour: '#fff', labels: [1] }] } })
    const l = lensCountLayer(live.vn, live.payload, new Set(), 'regions')
    expect(isProxy(l.payload)).toBe(false)
    expect(isProxy(l.payload.cells.x)).toBe(false)
    expect(isProxy(l.payload.pops[0].labels)).toBe(false)
    expect(l.group).toBe('regions')
    expect(lensChipCounts({ pops: [l], regions: [], tracks: [] }, { x: 0, y: 0, r: 1 }, [1, 1, 1], 0, [-1, -1]).cells).toBe(1)
  })
})
