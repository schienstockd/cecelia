import { describe, it, expect, vi, beforeAll, afterAll } from 'vitest'

// Both renderers' uploads are async — awaits (`popErrorScope`, `onSubmittedWorkDone`, a pending atlas
// allocation) between receiving the bytes and registering them as cached. A `setImage` landing in that gap is a NEW
// allocation, even at the same dims: the caller reallocated because what a frame holds changed (a mask
// switched on, the live store swapped for the registered one). The old frame must not be installed —
// installed, `hasTimepoint` says "cached", the caller never refetches, and the mask the user just
// ticked on is not drawn until the image is reopened. The tile atlas has the same hole for a store swap
// (a version switch or a same-store rewrite): the old store's pixels cached under the new store.
//
// No WebGPU in the test runner, so the device is a recursive no-op fake. Only the two awaits matter,
// and `onSubmittedWorkDone` is held open so the test decides what happens inside the gap.

let releaseQueue: (() => void) | null = null
/** Let the held `onSubmittedWorkDone` resolve — the upload resumes past its last await. */
const release = () => { const r = releaseQueue; releaseQueue = null; r?.() }

function noopProxy(): unknown {
  const fn = () => noopProxy()
  return new Proxy(fn, {
    get: (_t, prop) => (prop === 'then' ? undefined : noopProxy()),
    apply: () => noopProxy(),
  })
}

const fakeDevice = new Proxy({} as Record<string, unknown>, {
  get(_t, prop) {
    if (prop === 'then') return undefined
    if (prop === 'popErrorScope') return async () => null
    if (prop === 'createShaderModule') {
      return () => ({ getCompilationInfo: async () => ({ messages: [] }) })
    }
    if (prop === 'queue') {
      return new Proxy({}, {
        get: (_q, p) => p === 'onSubmittedWorkDone'
          ? () => new Promise<void>(res => { releaseQueue = res })
          : () => undefined,
      })
    }
    if (prop === 'lost') return new Promise(() => {})
    return noopProxy()
  },
})

vi.mock('../../utils/webgpuProbe', () => ({
  acquireGpuDevice: async () => ({
    device: fakeDevice, report: { maxTextureDimension3D: 2048, maxBufferSize: 1 << 28 },
  }),
  WebGpuUnavailable: class extends Error {},
}))

const GLOBALS = ['GPUTextureUsage', 'GPUBufferUsage', 'GPUShaderStage'] as const
const g = globalThis as unknown as Record<string, unknown>
let gpuBefore: PropertyDescriptor | undefined
beforeAll(() => {
  for (const k of GLOBALS) g[k] = new Proxy({}, { get: () => 1 })
  gpuBefore = Object.getOwnPropertyDescriptor(globalThis.navigator, 'gpu')
  Object.defineProperty(globalThis.navigator, 'gpu', {
    configurable: true, value: { getPreferredCanvasFormat: () => 'bgra8unorm' },
  })
})
afterAll(() => {
  for (const k of GLOBALS) delete g[k]
  if (gpuBefore) Object.defineProperty(globalThis.navigator, 'gpu', gpuBefore)
  else delete (globalThis.navigator as unknown as Record<string, unknown>).gpu
})

const meta = {
  nX: 4, nY: 4, nZ: 1, nT: 2, nC: 1, bytesPerVoxel: 2,
  voxelUm: [1, 1, 1], channels: [{ name: 'c0', lo: 0, hi: 100, lut: 'gray', visible: true }],
} as never

const canvas = { getContext: () => ({ configure: () => {}, unconfigure: () => {} }), width: 4, height: 4 }
async function renderer() {
  const { createVolumeRenderer } = await import('./volumeRenderer')
  return createVolumeRenderer(canvas as never)
}
async function tileRenderer() {
  const { createTileRenderer } = await import('./tileRenderer')
  return createTileRenderer(canvas as never)
}

const frame = () => [new ArrayBuffer(4 * 4 * 2)]
const tick = () => new Promise(res => setTimeout(res, 0))

describe('flat renderer — an upload that straddles a setImage', () => {
  it('installs a frame whose allocation is still current', async () => {
    const r = await renderer()
    r.setImage(meta, 1e9, 1, 0, false)
    const up = r.uploadFrame(0, frame(), 0)
    await tick()
    release()
    await up
    expect(r.hasTimepoint(0)).toBe(true)
  })

  it('drops a frame fetched for the previous allocation at the same dims', async () => {
    const r = await renderer()
    r.setImage(meta, 1e9, 1, 0, false)            // no mask yet
    const up = r.uploadFrame(0, frame(), 0)        // that allocation's frame, mid-upload
    await tick()
    r.setImage(meta, 1e9, 1, 0, true)             // the mask switches on — same dims
    release()
    await up
    // Cached, the caller would never refetch t=0 with its mask.
    expect(r.hasTimepoint(0)).toBe(false)
  })

  it('does not stamp the previous plane\'s bytes as the current plane', async () => {
    const r = await renderer()
    r.setImage(meta, 1e9, 1, 0, false)
    const up = r.uploadFrame(0, frame(), 0)        // bytes for plane 0
    await tick()
    r.setZPlane!(1)                                // the user moves to plane 1 mid-upload (the flat
                                                   // renderer implements the optional method)
    release()
    await up
    expect(r.hasTimepoint(0)).toBe(false)
  })
})

const tileMeta = {
  ...(meta as object), levels: [{ level: 0, nX: 4, nY: 4, chunkX: 4, chunkY: 4 }],
} as never
const tile = { t: 0, z: 0, level: 0, tx: 0, ty: 0 }
const tileBytes = () => [new ArrayBuffer(4 * 4 * 2)]

describe('tile atlas — an upload that straddles a store swap', () => {
  it('installs a tile whose store is still current', async () => {
    const tr = await tileRenderer()
    tr.setImage(tileMeta, 0, 1e9, 4, 4, 1, 'img/default/1')
    const up = tr.uploadTile(tile, tileBytes(), new Set(), [], 'img/default/1')
    await tick()
    release()
    expect(await up).toBeGreaterThanOrEqual(0)
    expect(tr.hasTile(tile)).toBe(true)
  })

  it('drops a tile when the store is swapped during its upload', async () => {
    const tr = await tileRenderer()
    tr.setImage(tileMeta, 0, 1e9, 4, 4, 1, 'img/default/1')
    const up = tr.uploadTile(tile, tileBytes(), new Set(), [], 'img/default/1')
    await tick()
    tr.setImage(tileMeta, 0, 1e9, 4, 4, 1, 'img/default/2')   // a re-run rewrote the store — same shape
    release()
    expect(await up).toBe(-1)
    expect(tr.hasTile(tile)).toBe(false)
  })

  it('refuses bytes fetched for the store before the swap', async () => {
    const tr = await tileRenderer()
    tr.setImage(tileMeta, 0, 1e9, 4, 4, 1, 'img/default/2')
    expect(await tr.uploadTile(tile, tileBytes(), new Set(), [], 'img/default/1')).toBe(-1)
    expect(tr.hasTile(tile)).toBe(false)
  })
})
