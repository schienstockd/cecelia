import { describe, it, expect } from 'vitest'
import {
  captureViewState, applyViewState, channelHexFrom, propsUrl,
  type ApplyTarget, type ViewerViewState,
} from './viewerProps'
import type { ViewerChannel, ViewerMeta, OrbitCamera } from './volumeViewer'

function mkMeta(channels: ViewerChannel[]): ViewerMeta {
  return {
    nT: 3, nC: channels.length, nZ: 5, nX: 10, nY: 10,
    bytesPerVoxel: 2, slabBytes: 1000, contrastSource: 'viewer',
    voxelUm: [1, 1, 1], calibrated: { xy: true, z: true, t: true },
    spaceUnit: 'µm', frameIntervalMin: 1,
    channels,
  }
}

const CAM: OrbitCamera = { yaw: 0.1, pitch: 0.2, dist: 500, panX: 3, panY: 4 }

describe('channelHexFrom', () => {
  it('picks the last LUT stop and scales 0-1 → 0-255', () => {
    expect(channelHexFrom([[0, 0, 0], [1, 0, 0]])).toBe('#ff0000')
    expect(channelHexFrom([[0, 0, 0], [0, 1, 0]])).toBe('#00ff00')
  })
  it('empty LUT → empty string', () => {
    expect(channelHexFrom([])).toBe('')
  })
})

describe('captureViewState', () => {
  const ch: ViewerChannel[] = [
    { name: 'DAPI', lo: 100, hi: 4000, visible: true,  lut: [[0, 0, 0], [0, 0, 1]] },
    { name: 'GFP',  lo: 50,  hi: 3000, visible: false, lut: [[0, 0, 0], [0, 1, 0]] },
  ]
  const meta = mkMeta(ch)

  it('emits both webgpu + napari-shaped blocks', () => {
    const vs = captureViewState({
      meta, channels: ch, cam: CAM, mode: 'plane', zPlane: 12, zRange: [0, 30], t: 7, valueName: 'default',
    })
    expect(vs.webgpu?.channels).toHaveLength(2)
    expect(vs.webgpu?.channels[0]).toEqual({ hex: '#0000ff', lo: 100, hi: 4000, visible: true })
    expect(vs.webgpu?.mode).toBe('plane')
    expect(vs.webgpu?.t).toBe(7)
    expect(vs.webgpu?.zPlane).toBe(12)
    expect(vs.webgpu?.valueName).toBe('default')
    // napari-shaped: keyed by name, contrast + colormap + visible.
    expect(vs.layers?.['DAPI']?.contrast_limits).toEqual([100, 4000])
    expect(vs.layers?.['DAPI']?.colormap).toBe('#0000ff')
    expect(vs.layers?.['GFP']?.visible).toBe(false)
    expect(vs.dims?.current_step).toEqual([7, 12])
    expect(vs.dims?.ndisplay).toBe(2)
  })

  it('mode=volume → ndisplay=3', () => {
    const vs = captureViewState({
      meta, channels: ch, cam: CAM, mode: 'volume', zPlane: 0, zRange: [0, 30], t: 0, valueName: '',
    })
    expect(vs.dims?.ndisplay).toBe(3)
  })
})

function mkTarget() {
  const calls = { channel: [] as Array<{ c: number; patch: any }>, cam: null as OrbitCamera | null,
                  mode: '' as string, z: null as any, t: -1 }
  const target: ApplyTarget = {
    applyChannel: (c, patch) => { calls.channel.push({ c, patch }) },
    applyCamera:  cam => { calls.cam = cam },
    applyMode:    m => { calls.mode = m },
    applyZ:       (zPlane, zRange) => { calls.z = { zPlane, zRange } },
    applyT:       t => { calls.t = t },
  }
  return { target, calls }
}

describe('applyViewState', () => {
  const ch: ViewerChannel[] = [
    { name: 'DAPI', lo: 0, hi: 1, visible: true, lut: [[0, 0, 0], [0, 0, 1]] },
    { name: 'GFP',  lo: 0, hi: 1, visible: true, lut: [[0, 0, 0], [0, 1, 0]] },
  ]
  const meta = mkMeta(ch)

  it('prefers webgpu block over layers', () => {
    const vs: ViewerViewState = {
      webgpu: {
        channels: [{ hex: '#ff00ff', lo: 111, hi: 222, visible: false },
                   { hex: '#00ffff', lo: 333, hi: 444, visible: true }],
        cam: CAM, mode: 'volume', zPlane: 3, zRange: [0, 10], t: 5, valueName: 'v2',
      },
      layers: { DAPI: { contrast_limits: [9, 9], visible: true } },
    }
    const { target, calls } = mkTarget()
    applyViewState(vs, meta, target)
    expect(calls.channel[0]).toEqual({ c: 0, patch: { lo: 111, hi: 222, visible: false, hex: '#ff00ff' } })
    expect(calls.channel[1].patch.hex).toBe('#00ffff')
    expect(calls.cam).toEqual(CAM)
    expect(calls.mode).toBe('volume')
    expect(calls.z).toEqual({ zPlane: 3, zRange: [0, 10] })
    expect(calls.t).toBe(5)
  })

  it('falls back to napari layers matched by channel name', () => {
    const vs: ViewerViewState = {
      layers: {
        DAPI: { contrast_limits: [10, 100], visible: false, colormap: '#abcdef' },
        NOTFOUND: { contrast_limits: [1, 1] },
      },
      dims: { current_step: [8, 2] },
    }
    const { target, calls } = mkTarget()
    applyViewState(vs, meta, target)
    // Only DAPI matched. GFP has no entry → no call for it.
    const dapi = calls.channel.find(x => x.c === 0)!
    expect(dapi.patch.lo).toBe(10); expect(dapi.patch.hi).toBe(100)
    expect(dapi.patch.visible).toBe(false); expect(dapi.patch.hex).toBe('#abcdef')
    expect(calls.channel.find(x => x.c === 1)).toBeUndefined()   // GFP has no napari entry — skipped
    expect(calls.t).toBe(8)
  })

  it('null/undefined viewState is a no-op', () => {
    const { target, calls } = mkTarget()
    applyViewState(null, meta, target)
    applyViewState(undefined, meta, target)
    expect(calls.channel).toHaveLength(0)
    expect(calls.cam).toBeNull()
  })
})

describe('propsUrl', () => {
  it('omits valueName when empty', () => {
    expect(propsUrl({ projectUid: 'p', imageUid: 'i' })).toBe('/api/viewer/props?projectUid=p&imageUid=i')
  })
  it('includes valueName when set', () => {
    expect(propsUrl({ projectUid: 'p', imageUid: 'i', valueName: 'v' }))
      .toBe('/api/viewer/props?projectUid=p&imageUid=i&valueName=v')
  })
})
