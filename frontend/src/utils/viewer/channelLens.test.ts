import { describe, expect, it } from 'vitest'
import {
  clampLensRadius, lensFetchChannels, lensChipPops, lensCircleL0, lensForRecording, lensPopCounts, lensRadiusCss, lensScreenAt, lensUniform, pointerToolChips,
  LENS_RADIUS_DEFAULT, LENS_RADIUS_MAX, LENS_RADIUS_MIN, LENS_RIM_PX,
} from './channelLens'
import { VIEW_HALF_ANGLE } from '../volumeViewer'

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

describe('clampLensRadius', () => {
  it('holds the radius in its bounds, and a non-number falls back to the default', () => {
    expect(clampLensRadius(1)).toBe(LENS_RADIUS_MIN)
    expect(clampLensRadius(1e6)).toBe(LENS_RADIUS_MAX)
    expect(clampLensRadius(NaN)).toBe(LENS_RADIUS_DEFAULT)
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
      cells: 3,
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
  it('a radius in tissue px reads back in CSS px, held in bounds', () => {
    expect(lensRadiusCss(150, cam, meta, 800, 400)).toBe(120)
    expect(lensRadiusCss(1e6, cam, meta, 800, 400)).toBe(LENS_RADIUS_MAX)
  })
})

describe('lensChipPops names', () => {
  it('a name two populations share takes its parent', () => {
    const p = (path: string, n: number) => ({ key: `cyto|${path}`, name: path.split('/').pop()!, colour: '#000', n })
    expect(lensChipPops([p('/T/CD4', 3), p('/NK/CD4', 2), p('/B', 1)], 6).shown.map(x => x.name))
      .toEqual(['T/CD4', 'NK/CD4', 'B'])
  })
})
