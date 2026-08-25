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
// 6x render cost, silently. WEB_VIEWER_PLAN.md decision 3. The check is shared with the Settings
// diagnostic in `utils/webgpuProbe.ts` — one place, so a second consumer cannot forget the trap.

import { MIP_WGSL, POINTS_WGSL, SEGMENTS_WGSL } from './mipShader'
import {
  MAX_CHANNELS, LUT_STOPS, lutTextureBytes, extentUm,
  type ViewerMeta, type ViewerChannel, type OrbitCamera,
} from '../../utils/volumeViewer'
import { acquireGpuDevice, WebGpuUnavailable, type AdapterReport } from '../../utils/webgpuProbe'
export { WebGpuUnavailable, type AdapterReport }
import { cacheCapacity, lruEvictions } from '../../utils/volumeCache'
import { POINT_STRIDE, SEG_STRIDE } from '../../utils/viewerOverlays'
import { LABEL_PALETTE_N, labelPaletteBytes } from '../../utils/viewerLabels'

/** Bytes in the uniform struct: 7 leading vec4s + one vec4 per channel slot. */
const UNIFORM_BYTES = 7 * 16 + MAX_CHANNELS * 16
/** Float index of channel slot 0 — seven vec4s in. Written out because getting it wrong shifts every
 *  channel's contrast window by one slot, which renders as the wrong channel being bright. */
const CH0 = 28
/** Label ids are UInt32 on disk and `r32uint` on the GPU. Anything narrower is widened client-side
 *  (`utils/viewerLabels.ts`) rather than given a second texture format. */
const LABEL_BPV = 4

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
   *
   * `zLo` is the first loaded plane (0-based). It exists for the OVERLAYS: their coordinates are
   * absolute image µm, so a view showing planes 10-17 has to know where its box starts or every marker
   * lands a slab's worth of z away from its cell.
   */
  setImage(meta: ViewerMeta, budgetBytes: number, zDepth?: number, zLo?: number,
           withLabels?: boolean): void
  /**
   * Upload one timepoint — one raw little-endian slab per channel, each exactly
   * `nX*nY*nZ*bytesPerVoxel` long — and hold it. Resolves once the bytes are actually on the GPU, so
   * the caller can time the transfer rather than the staging copy. Evicts to stay inside the budget,
   * never evicting `keep`.
   */
  uploadTimepoint(t: number, channelBytes: ArrayBuffer[], keep: number,
                  labelBytes?: ArrayBuffer | null): Promise<void>
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
  /**
   * How many timepoints fit in the current budget, what one costs, and how many z planes one holds.
   *
   * `zDepth` is published because the CALLER has to size its requests to match, and a second copy of
   * that fact on the client is exactly how the two came apart. Ask the renderer what shape it is in;
   * do not re-derive it from the view mode.
   */
  readonly cache: { capacity: number; bytesPerTimepoint: number; zDepth: number }
  setCamera(cam: OrbitCamera): void
  setChannels(channels: ViewerChannel[]): void
  setSteps(steps: number): void
  /** Orthographic projection. Required for the 2D view — under perspective a flat plane foreshortens
   *  towards the edges, which is wrong for a view people measure on. */
  setOrthographic(on: boolean): void
  /**
   * Replace the overlay point instances (`POINT_STRIDE` floats each — see `utils/viewerOverlays.ts`).
   * Uploaded once per (image, populations), not per frame: the data is ordered by timepoint so a frame
   * is a range within it.
   */
  setOverlayPoints(data: Float32Array): void
  /**
   * Which slice of the instance buffer to draw, and how. `sizePx` is a SCREEN size — a cell marker is
   * annotation, so it must stay legible zoomed out and not swallow the cell zoomed in.
   *
   * `planeLo`/`planeHi` are the z planes actually LOADED, inclusive: the 2D view passes the same index
   * twice, a cropped 3D view passes its range, and `-1` draws every plane. A range rather than one
   * index because a 3D view cropped to eight planes would otherwise draw the whole stack's cells
   * against a box that holds eight of them.
   */
  setOverlayDraw(first: number, count: number, sizePx: number,
                 planeLo: number, planeHi?: number): void
  /** Replace the track-tail segment instances (`SEG_STRIDE` floats each). Same lifetime as the points:
   *  once per (image, populations), never per frame. */
  setOverlaySegments(data: Float32Array): void
  /** Which slice of the segment buffer to draw, and how wide in screen px. A tail is contiguous in that
   *  buffer by construction — see `buildTrackBuffer`. */
  setOverlaySegmentDraw(first: number, count: number, widthPx: number): void
  /**
   * How the mask is drawn. `opacity` 0 switches it off in the shader without dropping the textures, so
   * a toggle is free; `setImage(..., withLabels)` is what decides whether they are fetched at all.
   * `contourPx` is napari's `contour` — an outline that many voxels thick instead of a filled region,
   * which is what lets the signal under the mask stay readable.
   */
  setLabelStyle(opacity: number, contourPx: number): void
  /** Match the drawing buffer to the element's CSS size. Returns true when the size changed. */
  resize(): boolean
  draw(): void
  /** Rejects with the reason if the device is lost — VRAM pressure is the one to watch. */
  readonly lost: Promise<GPUDeviceLostInfo>
  destroy(): void
}

export async function createVolumeRenderer(canvas: HTMLCanvasElement): Promise<VolumeRenderer> {
  const { device, report } = await acquireGpuDevice()
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
      // VERTEX **and** fragment. The overlay passes read this uniform in their VERTEX stage — the
      // camera projects a point before there is a fragment to shade — and a binding a stage cannot see
      // is a pipeline-creation validation error, not a warning. `createRenderPipeline` then hands back
      // an INVALID pipeline; setting it makes the whole render pass invalid, and the volume draws in
      // that same pass, so the canvas goes black the moment any overlay is switched on. It rendered
      // fine until then, which is exactly why this survived being written.
      { binding: 0, visibility: GPUShaderStage.VERTEX | GPUShaderStage.FRAGMENT,
        buffer: { type: 'uniform', minBindingSize: UNIFORM_BYTES } },
      { binding: 1, visibility: GPUShaderStage.FRAGMENT,
        texture: { sampleType: 'uint', viewDimension: '3d' } },
      { binding: 2, visibility: GPUShaderStage.FRAGMENT,
        texture: { sampleType: 'float', viewDimension: '2d' } },
      // The mask and its palette. Always BOUND, even with no segmentation shown — WebGPU has no
      // optional binding, so switching labels off binds a 1x1x1 placeholder and sets the opacity to 0
      // rather than swapping layouts. The shader never reads it at opacity 0.
      { binding: 3, visibility: GPUShaderStage.FRAGMENT,
        texture: { sampleType: 'uint', viewDimension: '3d' } },
      { binding: 4, visibility: GPUShaderStage.FRAGMENT,
        texture: { sampleType: 'float', viewDimension: '2d' } },
    ],
  })
  const pipeline = device.createRenderPipeline({
    layout: device.createPipelineLayout({ bindGroupLayouts: [bindGroupLayout] }),
    vertex: { module, entryPoint: 'vs' },
    fragment: { module, entryPoint: 'fs', targets: [{ format }] },
    primitive: { topology: 'triangle-list' },
  })

  // The overlay pipeline shares the MIP's bind group LAYOUT so it shares the uniform buffer, and
  // therefore the camera. `project()` in the shader is the exact inverse of the ray construction; a
  // second camera would put a marker next to its cell rather than on it, and would still look
  // plausible. Alpha-blended over the finished MIP in the same pass — no second attachment, no clear.
  const pointsModule = device.createShaderModule({ code: POINTS_WGSL })
  const pointsErrs = (await pointsModule.getCompilationInfo()).messages.filter(m => m.type === 'error')
  if (pointsErrs.length) {
    throw new Error('Points shader: ' + pointsErrs.map(m => `${m.lineNum}:${m.message}`).join(' | '))
  }
  const pointsPipeline = device.createRenderPipeline({
    layout: device.createPipelineLayout({ bindGroupLayouts: [bindGroupLayout] }),
    vertex: {
      module: pointsModule, entryPoint: 'vs',
      buffers: [{
        arrayStride: POINT_STRIDE * 4,
        stepMode: 'instance',
        attributes: [
          { shaderLocation: 0, offset: 0, format: 'float32x3' },   // centre, absolute image µm
          { shaderLocation: 1, offset: 12, format: 'float32x3' },  // rgb
          { shaderLocation: 2, offset: 24, format: 'float32' },    // z plane
        ],
      }],
    },
    fragment: {
      module: pointsModule, entryPoint: 'fs',
      targets: [{
        format,
        blend: {
          color: { srcFactor: 'src-alpha', dstFactor: 'one-minus-src-alpha', operation: 'add' },
          alpha: { srcFactor: 'one', dstFactor: 'one-minus-src-alpha', operation: 'add' },
        },
      }],
    },
    primitive: { topology: 'triangle-list' },
  })

  // Track tails. A third pipeline rather than a second topology: WebGPU draws 1px lines only, and a
  // 1px tail over a noisy MIP is close to invisible (napari's tail_width defaults to 4).
  const segModule = device.createShaderModule({ code: SEGMENTS_WGSL })
  const segErrs = (await segModule.getCompilationInfo()).messages.filter(m => m.type === 'error')
  if (segErrs.length) {
    throw new Error('Segments shader: ' + segErrs.map(m => `${m.lineNum}:${m.message}`).join(' | '))
  }
  const segPipeline = device.createRenderPipeline({
    layout: device.createPipelineLayout({ bindGroupLayouts: [bindGroupLayout] }),
    vertex: {
      module: segModule, entryPoint: 'vs',
      buffers: [{
        arrayStride: SEG_STRIDE * 4,
        stepMode: 'instance',
        attributes: [
          { shaderLocation: 0, offset: 0, format: 'float32x3' },   // from, absolute image µm
          { shaderLocation: 1, offset: 12, format: 'float32x3' },  // to
          { shaderLocation: 2, offset: 24, format: 'float32x3' },  // rgb
          { shaderLocation: 3, offset: 36, format: 'float32' },    // z plane of the segment's END
        ],
      }],
    },
    fragment: {
      module: segModule, entryPoint: 'fs',
      targets: [{
        format,
        blend: {
          color: { srcFactor: 'src-alpha', dstFactor: 'one-minus-src-alpha', operation: 'add' },
          alpha: { srcFactor: 'one', dstFactor: 'one-minus-src-alpha', operation: 'add' },
        },
      }],
    },
    primitive: { topology: 'triangle-list' },
  })

  const uniforms = device.createBuffer({
    size: UNIFORM_BYTES, usage: GPUBufferUsage.UNIFORM | GPUBufferUsage.COPY_DST,
  })
  const lutTex = device.createTexture({
    size: [LUT_STOPS, MAX_CHANNELS], format: 'rgba8unorm',
    usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST,
  })
  // The label palette: one row, written once and never again. Ids index it modulo its width.
  const palTex = device.createTexture({
    size: [LABEL_PALETTE_N, 1], format: 'rgba8unorm',
    usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST,
  })
  device.queue.writeTexture({ texture: palTex }, labelPaletteBytes(),
                            { bytesPerRow: LABEL_PALETTE_N * 4 }, [LABEL_PALETTE_N, 1])
  // Bound wherever a timepoint has no mask, because a bind group must be complete.
  const noLabels = device.createTexture({
    size: [1, 1, 1], dimension: '3d', format: 'r32uint',
    usage: GPUTextureUsage.TEXTURE_BINDING,
  })

  const u = new Float32Array(UNIFORM_BYTES / 4)
  /** t → its volume texture, its mask (when one is shown) and the bind group that reads them. */
  const slots = new Map<number,
    { texture: GPUTexture; labelTexture: GPUTexture | null; bindGroup: GPUBindGroup }>()
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
  /** Whether a mask rides along in each timepoint's slot. Set by `setImage`, because it changes what a
   *  timepoint COSTS and therefore how many fit — a mask is 4 bytes a voxel against the image's 2. */
  let labels = false
  let steps = 256
  let destroyed = false
  /** Overlay instances, grown on demand. One buffer for the whole movie — see `setOverlayPoints`. */
  let pointBuf: GPUBuffer | null = null
  let pointCap = 0
  let pointFirst = 0
  let pointCount = 0
  let segBuf: GPUBuffer | null = null
  let segCap = 0
  let segFirst = 0
  let segCount = 0
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
    if (!dead) {
      const slot = slots.get(t)
      slot?.texture.destroy()
      slot?.labelTexture?.destroy()
    }
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
      const o = CH0 + c * 4
      u[o] = ch ? ch.lo : 0
      u[o + 1] = ch ? ch.hi : 1
      u[o + 2] = ch && ch.visible ? 1 : 0
    }
  }

  return {
    adapter: report,
    lost: device.lost,

    setImage(m: ViewerMeta, budgetBytes: number, zd = m.nZ, zLo = 0, withLabels = false) {
      // NOT gated on the device being alive, deliberately. Nothing here touches the GPU except
      // `dropAll()` and `setChannels`, which guard themselves — while what it DOES set is the geometry
      // every later decision is derived from. A `!usable()` early return left the renderer describing a
      // 2D plane while the client fetched whole volumes: the cache reported capacity 170 for a 4-volume
      // budget, so the read-ahead thought a frame was cheap and queued dozens of 326 MB requests, and
      // the server's read time went from 0.5 s to 3.7 s under its own contention.
      dropAll()
      bindGroup = null
      meta = m
      depth = Math.max(1, Math.min(zd, m.nZ))
      const nch = Math.min(m.nC, MAX_CHANNELS)
      labels = withLabels
      // The mask is part of what a timepoint costs, so it is part of what decides how many fit. Leaving
      // it out would let the cache promise a capacity it cannot hold, and the frame that discovers that
      // is an out-of-memory scope firing mid-scrub rather than a smaller cache.
      bytesPerTimepoint = m.nX * m.nY * depth * (m.bytesPerVoxel * nch + (withLabels ? LABEL_BPV : 0))
      allowed = Infinity                       // a new shape gets a fresh chance at the limit
      byteCap = cacheCapacity(budgetBytes, bytesPerTimepoint)
      recap()
      const [ex, ey, ez] = extentUm(m, depth)
      u[8] = ex; u[9] = ey; u[10] = ez
      // ext.w — where the loaded slab STARTS up the stack, in µm. Overlay coordinates are absolute, so
      // without this a cropped 3D view would draw them against a box that no longer begins at zero.
      u[11] = Math.max(0, zLo) * (m.voxelUm[2] || 1)
      // dims.z is ONE channel's own depth, not the stacked height: the ray marches one channel's box
      // and the shader offsets by `c * zpc` to reach the others. Using the stacked height here squashes
      // every channel into 1/nch of the volume — a render that looks like a thin slab of real data.
      u[12] = m.nX; u[13] = m.nY; u[14] = depth; u[15] = depth
      u[4] = nch
      setChannels(m.channels)
    },

    async uploadTimepoint(t: number, channelBytes: ArrayBuffer[], keep: number,
                          labelBytes: ArrayBuffer | null = null) {
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
      // The mask goes in the SAME error scope and the same slot as the image it annotates. One
      // allocation failing has to take the pair down together: a slot holding a volume and no mask
      // would render the image with the outlines silently missing.
      const labelTexture = (labels && labelBytes) ? device.createTexture({
        size: [m.nX, m.nY, depth], dimension: '3d', format: 'r32uint',
        usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST,
      }) : null
      const oom = await device.popErrorScope()
      if (oom) {
        if (!dead) { texture.destroy(); labelTexture?.destroy() }
        // Hold one below what we managed, so there is always room for the next frame to arrive.
        allowed = Math.max(2, slots.size - 1)
        recap()
        for (const gone of lruEvictions(order, capacity, spare(keep))) dropSlot(gone)
        return
      }
      if (!usable()) return
      if (labelTexture && labelBytes) {
        device.queue.writeTexture(
          { texture: labelTexture }, labelBytes,
          { bytesPerRow: m.nX * LABEL_BPV, rowsPerImage: m.nY }, [m.nX, m.nY, depth],
        )
      }
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
      if (!usable()) { if (!dead) { texture.destroy(); labelTexture?.destroy() } return }

      const bg = device.createBindGroup({
        layout: bindGroupLayout,
        entries: [
          { binding: 0, resource: { buffer: uniforms } },
          { binding: 1, resource: texture.createView() },
          { binding: 2, resource: lutTex.createView() },
          { binding: 3, resource: (labelTexture ?? noLabels).createView() },
          { binding: 4, resource: palTex.createView() },
        ],
      })
      dropSlot(t)                                    // a re-upload replaces, never leaks
      slots.set(t, { texture, labelTexture, bindGroup: bg })
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
    get cache() { return { capacity, bytesPerTimepoint, zDepth: depth } },

    setCamera(cam: OrbitCamera) {
      u[0] = cam.yaw; u[1] = cam.pitch; u[2] = cam.dist
      // The pan rides the camera rather than being a separate setter: it IS camera state, and a second
      // entry point is a second thing to forget on the frame path.
      u[24] = cam.panX || 0; u[25] = cam.panY || 0
    },

    setChannels,

    // 2D wants exactly ONE step: with a one-plane box the single sample lands on the box midpoint,
    // which is that plane. So the floor is 1, not 16.
    setSteps(n: number) { steps = Math.max(1, Math.round(n)) },
    setOrthographic(on: boolean) { u[7] = on ? 1 : 0 },

    setOverlayPoints(data: Float32Array) {
      if (!usable()) return
      if (data.length === 0) { pointCount = 0; return }
      // Grown, never shrunk: a buffer is destroyed and reallocated only when it is too small, so
      // toggling a population off and on again does not churn VRAM.
      if (!pointBuf || data.length > pointCap) {
        pointBuf?.destroy()
        pointCap = data.length
        pointBuf = device.createBuffer({
          size: pointCap * 4, usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
        })
      }
      device.queue.writeBuffer(pointBuf, 0, data)
    },

    setOverlayDraw(first: number, count: number, sizePx: number,
                   planeLo: number, planeHi = planeLo) {
      pointFirst = Math.max(0, Math.floor(first))
      pointCount = Math.max(0, Math.floor(count))
      u[16] = Math.max(1, sizePx)
      u[17] = planeLo
      u[19] = Math.max(planeLo, planeHi)
    },

    setOverlaySegments(data: Float32Array) {
      if (!usable()) return
      if (data.length === 0) { segCount = 0; return }
      if (!segBuf || data.length > segCap) {
        segBuf?.destroy()
        segCap = data.length
        segBuf = device.createBuffer({
          size: segCap * 4, usage: GPUBufferUsage.VERTEX | GPUBufferUsage.COPY_DST,
        })
      }
      device.queue.writeBuffer(segBuf, 0, data)
    },

    setOverlaySegmentDraw(first: number, count: number, widthPx: number) {
      segFirst = Math.max(0, Math.floor(first))
      segCount = Math.max(0, Math.floor(count))
      u[18] = Math.max(1, widthPx)
    },

    setLabelStyle(opacity: number, contourPx: number) {
      u[20] = Math.max(0, Math.min(1, opacity))
      u[21] = Math.max(0, Math.round(contourPx))
      u[22] = LABEL_PALETTE_N
    },

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
      // Overlays go in the SAME pass, after the volume: `loadOp: 'clear'` has already run, so this
      // blends over a finished MIP without a second attachment or a second clear. Tails first, then
      // points: a marker has to sit ON TOP of the path that leads to it, not under it.
      if (segBuf && segCount > 0) {
        pass.setPipeline(segPipeline)
        pass.setBindGroup(0, bindGroup)
        pass.setVertexBuffer(0, segBuf)
        pass.draw(6, segCount, 0, segFirst)
      }
      if (pointBuf && pointCount > 0) {
        pass.setPipeline(pointsPipeline)
        pass.setBindGroup(0, bindGroup)
        pass.setVertexBuffer(0, pointBuf)
        pass.draw(6, pointCount, 0, pointFirst)
      }
      pass.end()
      device.queue.submit([enc.finish()])
    },

    destroy() {
      destroyed = true
      dropAll()
      pointBuf?.destroy(); pointBuf = null
      segBuf?.destroy(); segBuf = null
      if (dead) return                     // the device took its resources with it
      lutTex.destroy(); palTex.destroy(); noLabels.destroy(); uniforms.destroy()
      device.destroy()
    },
  }
}
