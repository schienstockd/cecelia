// The WebGPU device, textures and draw loop behind the volume viewer. Everything that needs a GPU is
// here; the logic that does not is in `utils/volumeViewer.ts` (and is unit-tested there).
//
// Lifecycle: `createVolumeRenderer(canvas)` → `setImage(meta, budgetBytes)` once per image →
// `uploadTimepoint(t, bufs)` per timepoint → `show(t)` + `draw()` per frame → `destroy()`. Nothing is
// allocated per frame.
//
// ONE TEXTURE PER CACHED TIMEPOINT, under a byte budget, evicted least-recently-used. That is what
// makes a timecourse usable and nothing about the rendering does: a cold timepoint is ~1.2 s and a
// resident one is sub-millisecond. The eviction policy itself is in `utils/volumeCache.ts` so it can be
// tested without a device; this owns the textures and the bind groups. The frame on screen is never
// evicted — its texture is what the next draw binds, and dropping it is an unattributable black frame.
//
// NOTHING TOUCHES THE GPU AFTER THE DEVICE IS GONE, and nothing binds a texture that has been
// destroyed. Both were reachable and both killed the browser rather than raising anything catchable:
// Firefox's main process crashed with `Queue[Id(4,2)] does not exist` / `Texture is not submitted`
// (Dominik, 2026-08-24). A lost device left `draw()` still submitting to a dead queue every frame, and
// the eviction policy protected the timepoint being LOADED while the bind group pointed at the
// different timepoint still on screen. `dead`/`destroyed` gate every GPU call, and `boundT` is
// protected at every eviction site as well as unbound before its texture can go.
//
// THE ADAPTER TRAP, and why it is asserted rather than trusted. `requestAdapter({})` returns the
// INTEGRATED GPU on this machine, and Firefox blanks every `adapter.info` field, so there is no name to
// check. `maxTextureDimension3D` is the usable tell: the discrete card reports 16384, integrated 2048.
// This is the browser-side twin of the PRIME trap in `app/src/napari.jl:55-59` — the difference is a
// 6x render cost, silently. WEB_VIEWER_PLAN.md decision 3.

import { MIP_WGSL } from './mipShader'
import {
  MAX_CHANNELS, LUT_STOPS, lutTextureBytes, extentUm,
  type ViewerMeta, type ViewerChannel, type OrbitCamera,
} from '../../utils/volumeViewer'
import { cacheCapacity, lruEvictions } from '../../utils/volumeCache'

/** Bytes in the uniform struct: 4 leading vec4s + one vec4 per channel slot. */
const UNIFORM_BYTES = 4 * 16 + MAX_CHANNELS * 16

export interface AdapterReport {
  maxTextureDimension3D: number
  /** Whether this looks like the discrete GPU. False means the browser handed us the integrated one. */
  looksDiscrete: boolean
  hasTimestamps: boolean
}

export interface VolumeRenderer {
  readonly adapter: AdapterReport
  /**
   * Allocate for one image under a VRAM budget. Frees every cached timepoint; they must be uploaded
   * again. `budgetBytes` decides how many fit — see `cacheCapacity`.
   *
   * `zDepth` is how many z planes each timepoint carries: `meta.nZ` for the volume view, `1` for the
   * 2D plane view. It drives the texture size, the physical extent AND the cache capacity, which is
   * why it is one argument rather than three settings that can disagree — at depth 1 a timepoint of
   * `Dml3RG` is 8.8 MB instead of 326 MB, so the same budget holds the whole movie instead of five
   * frames of it. Changing it necessarily drops the cache: the textures are a different shape.
   */
  setImage(meta: ViewerMeta, budgetBytes: number, zDepth?: number): void
  /**
   * Upload one timepoint — one raw little-endian slab per channel, each exactly
   * `nX*nY*nZ*bytesPerVoxel` long — and hold it. Resolves once the bytes are actually on the GPU, so
   * the caller can time the transfer rather than the staging copy. Evicts to stay inside the budget,
   * never evicting `keep`.
   */
  uploadTimepoint(t: number, channelBytes: ArrayBuffer[], keep: number): Promise<void>
  /**
   * Cap the cache at `n` timepoints. The effective `capacity` is the smallest of this, the byte ceiling
   * `setImage` was given, and anything the GPU turned out not to allow.
   */
  setCapacity(n: number): void
  /**
   * True once an allocation has actually run out of VRAM. The cache then holds at whatever fitted, so
   * this is informational rather than an error — and it is why there is no "how much VRAM do I have"
   * question for the user to get wrong.
   */
  readonly vramCapped: () => boolean
  /** Bind a cached timepoint for subsequent draws. False when it is not resident (nothing changes). */
  show(t: number): boolean
  hasTimepoint(t: number): boolean
  /** Cached timepoints, least recently used first — what the cache-state strip renders. */
  residentTimepoints(): number[]
  /** Mark a timepoint as recently used without binding it. */
  touch(t: number): void
  /** How many timepoints fit in the current budget, and what one costs. */
  readonly cache: { capacity: number; bytesPerTimepoint: number }
  setCamera(cam: OrbitCamera): void
  setChannels(channels: ViewerChannel[]): void
  setSteps(steps: number): void
  /** Orthographic projection. Required for the 2D view — under perspective a flat plane foreshortens
   *  towards the edges, which is wrong for a view people measure on. */
  setOrthographic(on: boolean): void
  /** Match the drawing buffer to the element's CSS size. Returns true when the size changed. */
  resize(): boolean
  draw(): void
  /** Rejects with the reason if the device is lost — VRAM pressure is the one to watch. */
  readonly lost: Promise<GPUDeviceLostInfo>
  destroy(): void
}

export class WebGpuUnavailable extends Error {}

export async function createVolumeRenderer(canvas: HTMLCanvasElement): Promise<VolumeRenderer> {
  if (!('gpu' in navigator)) throw new WebGpuUnavailable('This browser has no WebGPU')
  // 'high-performance' is not advice here — without it the browser picks the integrated GPU.
  const adapter = await navigator.gpu.requestAdapter({ powerPreference: 'high-performance' })
  if (!adapter) throw new WebGpuUnavailable('No WebGPU adapter available')

  const maxDim3D = adapter.limits.maxTextureDimension3D
  const hasTimestamps = adapter.features.has('timestamp-query')
  const report: AdapterReport = {
    maxTextureDimension3D: maxDim3D, looksDiscrete: maxDim3D > 2048, hasTimestamps,
  }

  const device = await adapter.requestDevice()
  const ctx = canvas.getContext('webgpu')
  if (!ctx) throw new WebGpuUnavailable('Canvas gave no WebGPU context')
  const format = navigator.gpu.getPreferredCanvasFormat()
  ctx.configure({ device, format, alphaMode: 'opaque' })

  const module = device.createShaderModule({ code: MIP_WGSL })
  // Compile errors are reported, not thrown, so an unchecked module fails later as a blank canvas —
  // indistinguishable from "the data is empty". Surface it here instead.
  const errs = (await module.getCompilationInfo()).messages.filter(m => m.type === 'error')
  if (errs.length) {
    throw new Error('Shader: ' + errs.map(m => `${m.lineNum}:${m.message}`).join(' | '))
  }

  const bindGroupLayout = device.createBindGroupLayout({
    entries: [
      { binding: 0, visibility: GPUShaderStage.FRAGMENT,
        buffer: { type: 'uniform', minBindingSize: UNIFORM_BYTES } },
      { binding: 1, visibility: GPUShaderStage.FRAGMENT,
        texture: { sampleType: 'uint', viewDimension: '3d' } },
      { binding: 2, visibility: GPUShaderStage.FRAGMENT,
        texture: { sampleType: 'float', viewDimension: '2d' } },
    ],
  })
  const pipeline = device.createRenderPipeline({
    layout: device.createPipelineLayout({ bindGroupLayouts: [bindGroupLayout] }),
    vertex: { module, entryPoint: 'vs' },
    fragment: { module, entryPoint: 'fs', targets: [{ format }] },
    primitive: { topology: 'triangle-list' },
  })

  const uniforms = device.createBuffer({
    size: UNIFORM_BYTES, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
  })
  const lutTex = device.createTexture({
    size: [LUT_STOPS, MAX_CHANNELS], format: 'rgba8unorm',
    usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST,
  })

  const u = new Float32Array(UNIFORM_BYTES / 4)
  /** t → its volume texture and the bind group that reads it. */
  const slots = new Map<number, { texture: GPUTexture; bindGroup: GPUBindGroup }>()
  /** LRU order, least recently used FIRST. */
  let order: number[] = []
  let bindGroup: GPUBindGroup | null = null
  let meta: ViewerMeta | null = null
  let capacity = 2
  /** What the GPU actually allowed, discovered by an allocation failing. `Infinity` until it does. */
  let allowed = Infinity
  /** From the BYTE ceiling — the one bound that means the same thing in 2D and 3D. */
  let byteCap = 2
  /** From the user's "keep N timepoints". */
  let requested = Infinity
  let bytesPerTimepoint = 0
  const recap = () => { capacity = Math.max(2, Math.min(byteCap, requested, allowed)) }
  /** z planes per timepoint actually loaded — `meta.nZ` in 3D, 1 in the 2D plane view. */
  let depth = 1
  let steps = 256
  let destroyed = false
  /** The device is GONE (lost, or destroyed by us). Every entry point below is a no-op afterwards: a
   *  queue that no longer exists is not an error you can catch, it is a browser crash. */
  let dead = false
  void device.lost.then(() => { dead = true })
  const usable = () => !destroyed && !dead
  /** The timepoint whose texture the current `bindGroup` reads. NOT the same as the timepoint being
   *  loaded, which is the mistake that crashed the browser — see `lruEvictions`. */
  let boundT = -1
  /** Everything an eviction must spare: the caller's timepoint AND whatever is actually on screen. */
  const spare = (keep: number) => [keep, boundT]

  const touch = (t: number) => {
    const i = order.indexOf(t)
    if (i >= 0) order.splice(i, 1)
    order.push(t)
  }
  const dropSlot = (t: number) => {
    // Unbind FIRST. `spare()` should mean this never fires, but a bind group outliving its texture is
    // a browser crash rather than a bad frame, so the invariant is enforced here too and not only
    // where the eviction list is computed.
    if (t === boundT) { bindGroup = null; boundT = -1 }
    if (!dead) slots.get(t)?.texture.destroy()
    slots.delete(t)
    const i = order.indexOf(t)
    if (i >= 0) order.splice(i, 1)
  }
  const dropAll = () => { for (const t of [...slots.keys()]) dropSlot(t) }

  const pushUniforms = () => { if (usable()) device.queue.writeBuffer(uniforms, 0, u) }

  // Standalone rather than a method, because `setImage` calls it and `this` inside a returned object
  // literal is not the object as far as the compiler is concerned.
  function setChannels(channels: ViewerChannel[]) {
    if (!usable()) return
    device.queue.writeTexture(
      { texture: lutTex }, lutTextureBytes(channels),
      { bytesPerRow: LUT_STOPS * 4, rowsPerImage: MAX_CHANNELS },
      [LUT_STOPS, MAX_CHANNELS],
    )
    for (let c = 0; c < MAX_CHANNELS; c++) {
      const ch = channels[c]
      const o = 16 + c * 4
      u[o] = ch ? ch.lo : 0
      u[o + 1] = ch ? ch.hi : 1
      u[o + 2] = ch && ch.visible ? 1 : 0
    }
  }

  return {
    adapter: report,
    lost: device.lost,

    setImage(m: ViewerMeta, budgetBytes: number, zd = m.nZ) {
      if (!usable()) return
      dropAll()
      bindGroup = null
      meta = m
      depth = Math.max(1, Math.min(zd, m.nZ))
      const nch = Math.min(m.nC, MAX_CHANNELS)
      bytesPerTimepoint = m.nX * m.nY * depth * m.bytesPerVoxel * nch
      allowed = Infinity                       // a new shape gets a fresh chance at the limit
      byteCap = cacheCapacity(budgetBytes, bytesPerTimepoint)
      recap()
      const [ex, ey, ez] = extentUm(m, depth)
      u[8] = ex; u[9] = ey; u[10] = ez
      // dims.z is ONE channel's own depth, not the stacked height: the ray marches one channel's box
      // and the shader offsets by `c * zpc` to reach the others. Using the stacked height here squashes
      // every channel into 1/nch of the volume — a render that looks like a thin slab of real data.
      u[12] = m.nX; u[13] = m.nY; u[14] = depth; u[15] = depth
      u[4] = nch
      setChannels(m.channels)
    },

    async uploadTimepoint(t: number, channelBytes: ArrayBuffer[], keep: number) {
      const m = meta
      if (!m || !usable()) return
      const nch = Math.min(m.nC, MAX_CHANNELS)
      // Channels stacked along z in one texture: one binding, one loop in the shader.
      //
      // ALLOCATED INSIDE AN OOM ERROR SCOPE, which is the whole reason a too-large cache can no longer
      // take the viewer down. WebGPU deliberately exposes no "free VRAM" figure, so neither we nor the
      // user can compute a safe budget — asking them for one was a question with no answerable form,
      // and setting it too high lost the device. `out-of-memory` is a scoped, recoverable error: the
      // texture comes back invalid instead, and the cache simply holds at whatever did fit.
      device.pushErrorScope('out-of-memory')
      const texture = device.createTexture({
        size: [m.nX, m.nY, depth * nch], dimension: '3d', format: 'r16uint',
        usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST,
      })
      const oom = await device.popErrorScope()
      if (oom) {
        if (!dead) texture.destroy()
        // Hold one below what we managed, so there is always room for the next frame to arrive.
        allowed = Math.max(2, slots.size - 1)
        recap()
        for (const gone of lruEvictions(order, capacity, spare(keep))) dropSlot(gone)
        return
      }
      if (!usable()) return
      for (let c = 0; c < Math.min(channelBytes.length, nch); c++) {
        device.queue.writeTexture(
          { texture, origin: [0, 0, c * depth] },
          channelBytes[c],
          { bytesPerRow: m.nX * m.bytesPerVoxel, rowsPerImage: m.nY },
          [m.nX, m.nY, depth],
        )
      }
      // `writeTexture` returns once the bytes are STAGED, so without this the caller times the CPU-side
      // copy and not the transfer — an upload cost under-reported by most of itself. The other
      // direction is a trap too: `onSubmittedWorkDone` has a ~100 ms quantum on this driver, so
      // anything faster than that is unmeasurable through it (it is what voided the audit's first G2).
      await device.queue.onSubmittedWorkDone()
      // The device can go during that await, and everything below allocates against it.
      if (!usable()) { if (!dead) texture.destroy(); return }

      const bg = device.createBindGroup({
        layout: bindGroupLayout,
        entries: [
          { binding: 0, resource: { buffer: uniforms } },
          { binding: 1, resource: texture.createView() },
          { binding: 2, resource: lutTex.createView() },
        ],
      })
      dropSlot(t)                                    // a re-upload replaces, never leaks
      slots.set(t, { texture, bindGroup: bg })
      touch(t)
      for (const gone of lruEvictions(order, capacity, spare(keep))) dropSlot(gone)
    },

    show(t: number): boolean {
      const slot = slots.get(t)
      if (!slot) return false
      bindGroup = slot.bindGroup
      boundT = t
      touch(t)
      return true
    },

    setCapacity(n: number) {
      requested = Math.max(2, Math.round(n))
      recap()
      // Spare the most recently touched AND the one on screen. They differ whenever the prefetch walk
      // has touched a resident neighbour since the last paint, which is most of the time.
      const keep = spare(order[order.length - 1] ?? -1)
      for (const gone of lruEvictions(order, capacity, keep)) dropSlot(gone)
    },
    vramCapped: () => allowed !== Infinity || byteCap < requested,

    hasTimepoint(t: number) { return slots.has(t) },
    residentTimepoints() { return [...order] },
    touch,
    get cache() { return { capacity, bytesPerTimepoint } },

    setCamera(cam: OrbitCamera) {
      u[0] = cam.yaw; u[1] = cam.pitch; u[2] = cam.dist
    },

    setChannels,

    // 2D wants exactly ONE step: with a one-plane box the single sample lands on the box midpoint,
    // which is that plane. So the floor is 1, not 16.
    setSteps(n: number) { steps = Math.max(1, Math.round(n)) },
    setOrthographic(on: boolean) { u[7] = on ? 1 : 0 },

    resize(): boolean {
      const dpr = window.devicePixelRatio || 1
      const w = Math.max(1, Math.round(canvas.clientWidth * dpr))
      const h = Math.max(1, Math.round(canvas.clientHeight * dpr))
      if (canvas.width === w && canvas.height === h) return false
      canvas.width = w; canvas.height = h
      return true
    },

    draw() {
      if (!usable() || !bindGroup) return
      u[3] = steps
      u[5] = canvas.width; u[6] = canvas.height
      pushUniforms()
      const enc = device.createCommandEncoder()
      const pass = enc.beginRenderPass({
        colorAttachments: [{
          view: ctx.getCurrentTexture().createView(),
          clearValue: { r: 0, g: 0, b: 0, a: 1 }, loadOp: 'clear', storeOp: 'store',
        }],
      })
      pass.setPipeline(pipeline)
      pass.setBindGroup(0, bindGroup)
      pass.draw(3)
      pass.end()
      device.queue.submit([enc.finish()])
    },

    destroy() {
      destroyed = true
      dropAll()
      if (dead) return                     // the device took its resources with it
      lutTex.destroy(); uniforms.destroy()
      device.destroy()
    },
  }
}
