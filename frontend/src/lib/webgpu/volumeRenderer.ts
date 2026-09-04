// The WebGPU device, textures and draw loop behind the volume viewer. Everything that needs a GPU is
// here; the logic that does not is in `utils/volumeViewer.ts` (and is unit-tested there).
//
// Lifecycle: `createVolumeRenderer(canvas)` → `setImage(meta, budgetBytes)` once per image →
// `uploadFrame(t, bufs)` per timepoint → `show(t)` + `draw()` per frame → `destroy()`. Nothing is
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
import { pickSrgbCanvasFormats } from './canvasFormat'
export { WebGpuUnavailable, type AdapterReport }
import { cacheCapacity, lruEvictions } from '../../utils/volumeCache'
import { POINT_STRIDE, SEG_STRIDE } from '../../utils/viewerOverlays'
import { LABEL_PALETTE_N, labelPaletteBytes } from '../../utils/viewerLabels'

/** Bytes in the uniform struct: 7 leading vec4s + one vec4 per channel slot. */
const UNIFORM_BYTES = 7 * 16 + MAX_CHANNELS * 16
/** Float index of channel slot 0 — seven vec4s in. Written out because getting it wrong shifts every
 *  channel's contrast window by one slot, which renders as the wrong channel being bright. */
const CH0 = 28
/** Float index of the labels vec4 (opacity, contourPx, LABEL_PALETTE_N). Named because the harness
 *  reads it — a NEW leading vec4 added AFTER this one shifts everything downstream and used to be
 *  silent (labels wrote into pan.x/pan.y and nothing drew). See `docs/todo/spike/webgpu/shader_check.mjs`. */
const LAB0 = 20
/** Label ids are UInt32 on disk and `r32uint` on the GPU. Anything narrower is widened client-side
 *  (`utils/viewerLabels.ts`) rather than given a second texture format. */
const LABEL_BPV = 4
/** Probe side for `sampleFrame`. 128 because `128 * 4` is already a multiple of the 256-byte
 *  `bytesPerRow` alignment a texture-to-buffer copy requires — no padded rows to unpick. */
export const PROBE_PX = 128

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
           withLabels?: boolean, renderNX?: number, renderNY?: number): void
  /**
   * Upload one timepoint — one raw little-endian slab per channel, each exactly
   * `nX*nY*nZ*bytesPerVoxel` long — and hold it. Resolves once the bytes are actually on the GPU, so
   * the caller can time the transfer rather than the staging copy. Evicts to stay inside the budget,
   * never evicting `keep`.
   */
  uploadFrame(t: number, channelBytes: ArrayBuffer[], keep: number,
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
                 planeLo: number, planeHi?: number, borderPx?: number): void
  /** Replace the track-tail segment instances (`SEG_STRIDE` floats each). Same lifetime as the points:
   *  once per (image, populations), never per frame. */
  setOverlaySegments(data: Float32Array): void
  /** Which slice of the segment buffer to draw, how wide in screen px, and its OWN plane bounds
   *  — ribbons carry their own z-reach so a viewer can widen the tails independently of the point
   *  window. Negative `planeLo` disables the filter (3D volume view, no z-slicing). */
  setOverlaySegmentDraw(first: number, count: number, widthPx: number,
                        planeLo: number, planeHi?: number): void
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
  /**
   * What the shader is actually being told, read back out of the uniform block.
   *
   * Not a convenience: the uniform is the ONE thing a black frame cannot be reasoned about without.
   * Every input to it is computed correctly in isolation and the failure mode is a canvas with nothing
   * on it, so "is the box the size I think, is the camera where I think" has no other answer. Read
   * after `draw()`, which is where the per-frame fields are written.
   */
  uniformState(): UniformState
  /**
   * Render the CURRENT state into a small offscreen target and read the pixels back.
   *
   * The one question a blank viewer cannot otherwise answer: did the shader produce black, or did it
   * produce an image that never reached the screen? Everything else is consistent with both — the
   * fetch reports bytes, the cache reports residency, the uniforms read back correct, and the canvas
   * is black either way. This renders the same pipeline with the same bind group into its own texture,
   * so it is independent of the canvas, the compositor and whatever the driver does with them.
   *
   * `null` when there is nothing bound to render yet, or the device is gone.
   */
  sampleFrame(withOverlays?: boolean): Promise<FrameSample | null>
  /** How many overlay instances the next draw will issue — `[points, tails]`. A non-zero count is the
   *  difference between a pass that draws the volume alone and one that can be invalidated by an
   *  overlay, which is not visible from anywhere else. */
  overlayCounts(): [number, number]
  /**
   * Clear the canvas to a flat colour and draw NOTHING else.
   *
   * The last split a blank viewer needs. `sampleFrame` proves the shader produces an image, but it
   * renders into its OWN texture — so it cannot tell "the image never reaches the canvas" from "the
   * canvas never reaches the screen". This uses the real swap chain and the simplest operation there
   * is: no pipeline, no bind group, no shader, just the clear. If the colour appears, the canvas
   * composites and the problem is between the draw and the swap-chain texture. If it does not, nothing
   * this canvas produces is ever shown, and no amount of rendering will change that.
   */
  setTestPattern(on: boolean): void
  /** Reconfigure how the canvas composites with the page — see the note on `alphaMode`. The shader
   *  writes alpha 1 everywhere, so this changes no pixel we produce, only which compositor path the
   *  browser takes to show them. */
  setAlphaMode(mode: GPUCanvasAlphaMode): void
  /**
   * Brick renderer only: point the internal fetch loop at (projectUid, imageUid, valueName). The
   * brick scheduler decides what to fetch every frame; the URL base has to be known here rather
   * than at each call site. Absent on the flat renderer — callers use `?.` to remain agnostic.
   */
  setBrickSource?(source: {
    projectUid: string; imageUid: string
    valueName?: string
    /** Segmentation value_name for the mask overlay. When set, the brick renderer's fetch loop
     *  fires a parallel `labels=<name>` request per intensity brick and writes the u32 ids into
     *  the label atlas at the same slot. Undefined = no label fetches; the placeholder texture
     *  stays bound and the shader skips the label path via `p.lab.x == 0`. */
    labelName?: string
    /** Opaque revision that changes when the SAME store is rewritten in place (a task re-run
     *  overwriting `ccidSmoothed.ome.zarr`). Treated as an identity component: any diff drops the
     *  atlas the same way a valueName change does. See `viewerCacheClearChannel.ts`. */
    rev?: string
  } | null): void
  /**
   * Brick renderer only: how to ask the caller for a redraw when a fetched brick lands. Brick
   * uploads are asynchronous — a fetch that resolves between frames updates the atlas without
   * anything on the caller's side noticing, so the shader keeps drawing the pre-arrival state.
   * Called ONCE per new brick (batched inside a single microtask by design). Absent on the flat
   * renderer — the flat path is caller-driven end-to-end.
   */
  setNeedsRedraw?(cb: (() => void) | null): void
  /**
   * Brick renderer only: hook called with per-channel brightness after each landed brick, so the
   * caller can grow `seenMax` from real data (the flat renderer does this in `uploadFrame`, but
   * bricks stream per-viewport and never see a whole timepoint's bytes). Without it the contrast
   * slider's ceiling stays at whatever the server first shipped, and dragging `hi` below the dtype
   * headroom locks the range. Absent on the flat renderer.
   */
  setOnBrickLoaded?(cb: ((perChannelMax: number[]) => void) | null): void
  /**
   * Brick renderer only: signal that the displayed timepoint just advanced (either via
   * `show(t)` on a ready t, or via the scheduler auto-catching-up once core bricks land after
   * a scrub-past-cold). ViewerWindow syncs `shownT` here so overlays draw at the same t the
   * volume is currently painting — otherwise a scrub past residency draws volume at the new t
   * with overlays still at the old one. Absent on the flat renderer.
   */
  setOnDisplayAdvanced?(cb: ((t: number) => void) | null): void
  /**
   * Brick renderer only: per-writeBrick timing hook — CPU-side duration of one writeBrick call
   * plus its byte count. Bench harness uses this to time the atlas-upload path. Absent on the
   * flat renderer.
   */
  setOnBrickWritten?(cb: ((durationMs: number, bytes: number) => void) | null): void
  /**
   * Brick renderer only: per-frame GPU + CPU sub-frame timings. Fires asynchronously (frame N+K)
   * from the timestamp-query readback path — GPU-side `gpuFrameMs` is populated only on adapters
   * with the `timestamp-query` feature; CPU-side buckets always populate. Not correlated 1:1
   * with the CPU-side `BenchSample` frames. Absent on the flat renderer. See
   * `docs/todo/BRICK_OCTREE_TRANSPLANTS_PLAN.md` P1.
   */
  setOnFrameTimings?(cb: ((s: import('../../utils/benchRecorder').GpuFrameSample) => void) | null): void
  /**
   * Brick renderer only: which timepoints to prefetch in the background. Typically the playback
   * window around the current `t` (see `prefetchWindow`). The renderer schedules a fetch per
   * scheduled brick × each prefetch `t`; arrived bricks sit LRU-warmed in the atlas until
   * `show(t)` swaps them onto the page table. Empty = current-t only. Absent on the flat
   * renderer — its own timepoint cache uses `uploadFrame` + `setCapacity`.
   */
  setPrefetchTimepoints?(list: number[]): void
  /**
   * Brick renderer only: safest prefetch depth given atlas capacity and the current per-t core
   * brick count. Callers pass their preferred cap (e.g. `4` during playback); the renderer clamps
   * to whatever fits alongside boundT without evicting it. Returns the caller's cap untouched
   * when the atlas isn't bound yet. Dml3RG-shape / small-cache regression guard — see
   * `maxSafePrefetchDepth` in `utils/pageTable.ts`.
   */
  maxSafePrefetchDepth?(requestedCap: number): number
  /**
   * Brick renderer only: floor for the SSE-picked LOD — coarsest level the scheduler is allowed
   * to use. `undefined` (or a negative number) means no floor (freely SSE). Threaded from
   * ViewerWindow's `slabLevel` computed. Replaces the 8b780fd pin: the pin blocked adaptive LOD
   * outright (SispLk zoom-in stuck at L5, 2026-08-29 screenshot). Over-fetch protection now
   * comes from `MAX_INTERSECT_BRICKS` inside `scheduleBricks` — coarser than picking a hard pin,
   * but wide-viewport-on-huge-L0 (f8gzA2 fit) is exactly what the intersect count catches.
   * Absent on the flat renderer — its `pickVolumeLevel` already picks per fetch.
   */
  setLevelFloor?(level: number | undefined): void
  /**
   * Brick renderer only: tune the LOD picker knobs at runtime. `maxIntersect` is the CORE brick
   * ceiling for the over-fetch guard (higher = more ambitious); `bias` shifts the SSE-picked
   * level (positive = coarser, negative = finer). Exposed as URL params for interactive tuning;
   * see ViewerWindow's mount.
   */
  setSchedulerKnobs?(k: { maxIntersect?: number; bias?: number }): void
  /**
   * Brick renderer only: enable/disable the "hold going-finer until current stable" gate. `true`
   * (default) protects the prev-level fallback from arriving mid-load. `false` swaps levels
   * eagerly — useful for A/B feel testing.
   */
  setHoldFinerEnabled?(on: boolean): void
  /**
   * Fast 2D plane switch — invalidate cached bytes WITHOUT tearing down the textures. On the
   * BRICK renderer the ~64 MB atlas texture is preserved (per-brick contents invalidated); on
   * the FLAT renderer the per-timepoint slots stay allocated but stamped stale, so
   * `show`/`hasTimepoint` miss and `uploadFrame` re-uploads on next visit. Both paths avoid
   * the 200 ms+ freeze `setImage` incurs from destroying every cached texture up front — the
   * pain Dominik hit on Dml3RG's 2D wheel (2026-08-29). Callers that don't have this method
   * fall through to a full `setImage` reallocate.
   */
  setZPlane?(zLo: number): void
  /**
   * Brick renderer only: snapshot of the atlas residency for the Debug mini map. Returns
   * every resident brick's virtual key plus the in-flight fetch keys — the caller filters
   * by `t + level` to draw the current-timepoint grid. Cheap (a `pageTable.entries()` walk
   * plus one `inflight.keys()` snapshot); safe to call every frame. Absent on the flat
   * renderer, whose per-timepoint slab-cache has a different residency shape.
   */
  brickResidency?(): {
    resident: { t: number; level: number; bx: number; by: number; bz: number }[]
    inflight: { t: number; level: number; bx: number; by: number; bz: number }[]
    currentLevel: number | undefined
    /** Brick edge in voxels — `[bx, by, bz]`. Fixed for the atlas's lifetime; the caller
     *  derives per-level grid dims from this + `meta.nX/nY` + `2^level`. */
    brickSizeVox: readonly [number, number, number]
    /** The timepoint the shader's pageTableCpu currently addresses — what's being DRAWN.
     *  May differ from `boundT` when the hold-on-cold rule kept the display at a resident
     *  t while the scheduler chases the target. `-1` before any t has been shown. */
    displayT: number
    /** The timepoint the SCHEDULER is fetching for — what the user asked for last. */
    boundT: number
    /** True when the canvas reflects the TARGET the user asked for AND is complete — i.e.
     *  `displayT === boundT` and every core viewport brick at `displayT` is resident. False
     *  covers both flavours of "canvas isn't the whole truth":
     *    - stale: hold-on-cold keeps `displayT` at the last-good t while the scheduler
     *      chases the new one, so pixels are FROM AN OLDER FRAME than the user scrubbed to.
     *    - partial: the "unblank" rule (ad0a20ec) promoted `displayT` before residency
     *      caught up, so pixels are the target frame with EMPTY_SLOT holes.
     *  The chip in `ViewerWindow.vue` surfaces both. */
    displayValid: boolean
    /** Diagnostic: how many CORE viewport bricks at `(displayT, currentLevel)` are absent from
     *  the atlas. If this is > 0 while `inflight.length === 0` and the "Loading bricks…" chip
     *  is on, the scheduler has stalled — the fetch loop should have called `kickFetch` for
     *  them but didn't. Used by the bench chip readout. */
    missing: number
    /** Diagnostic: same as `missing` but at `(boundT, currentLevel)` — the timepoint the
     *  scheduler is chasing, not the one currently drawn. Splits "hold-on-cold stall"
     *  (missing@bound > 0 while missing@display = 0) from "actual scheduler stall"
     *  (missing@display > 0). */
    missingAtBoundT: number
  }
  /** Rejects with the reason if the device is lost — VRAM pressure is the one to watch. */
  readonly lost: Promise<GPUDeviceLostInfo>
  destroy(): void
}

/** What the shader actually produced, 0-1 per channel — see `sampleFrame`. */
export interface FrameSample {
  /** Brightest of R, G, B over the probe. 0 means the shader drew nothing at all. */
  max: number
  /** Mean over R, G, B — separates "one bright speck" from "an image". */
  mean: number
  /** Fraction of probe pixels that are not pure black. */
  lit: number
  /** Probe side, px. */
  size: number
}

/** The uniform block in the units a person reads, for the Debug panel — see `uniformState`. */
export interface UniformState {
  /** Camera distance, µm. */
  dist: number
  /** The LOADED box, µm — x, y, z. In the 2D view z is one voxel deep. */
  ext: [number, number, number]
  /** Pan across the screen's axes, µm. */
  pan: [number, number]
  /** Ray samples per pixel. 1 in the 2D view. */
  steps: number
  ortho: boolean
  /** Channels the shader will composite. */
  nch: number
  /** The DRAWING BUFFER the frame was rendered at, px. Not the CSS size: they come apart on a
   *  device-pixel-ratio change, and a stale one here is a frame drawn for a different canvas. */
  canvas: [number, number]
}

export async function createVolumeRenderer(
  canvas: HTMLCanvasElement,
  /**
   * Called with any GPU error raised AFTER setup — a bad draw, a bad write, a bind group built per
   * timepoint. WebGPU hands these to the console and carries on, so without a caller listening the
   * only symptom is a black canvas, which is indistinguishable from an empty channel or a contrast
   * window that excludes the data. Optional: a caller with nowhere to show it should not be forced to.
   */
  onError?: (message: string) => void,
): Promise<VolumeRenderer> {
  const { device, report } = await acquireGpuDevice()
  // EVERY resource below is created inside a validation scope. WebGPU does not throw on a bad layout or
  // a bad pipeline — it returns an INVALID object and logs to the console — and setting an invalid
  // pipeline poisons the whole render pass, so the volume drawn in that same pass never appears. A
  // black canvas is the entire symptom. That has already cost a day once (the vertex-stage visibility
  // of binding 0), so the scope is around the construction rather than around the one call that was
  // wrong last time.
  device.pushErrorScope('validation')
  const ctx = canvas.getContext('webgpu')
  if (!ctx) throw new WebGpuUnavailable('Canvas gave no WebGPU context')
  // sRGB canvas policy — one helper across every WebGPU renderer here. See `./canvasFormat.ts`.
  // `format` is what the render pipelines target AND what the pass color-attachment view uses; the
  // canvas itself is configured at the linear base, with the sRGB view format declared as
  // compatible so `createView({ format })` at draw time gives the pipeline an sRGB attachment.
  const { base: canvasFormat, viewFormat: format } = pickSrgbCanvasFormats()
  /**
   * How the canvas composites with the page. Flippable at runtime (see `setAlphaMode`) so
   * the two paths can be compared without an app rebuild.
   */
  let alphaMode: GPUCanvasAlphaMode = 'opaque'
  /**
   * Configure the swap chain. Called again on every SIZE CHANGE — assigning `canvas.width` RESETS
   * the canvas (that is what the attribute does, for every context type). The configuration is
   * supposed to survive it and `getCurrentTexture()` is supposed to hand back a texture of the new
   * size; on the driver observed here it did not (`sampleFrame` measured 22% of pixels lit while the
   * canvas stayed black), because the first `configure` had happened at the default 300x150 and
   * nothing reattached it afterwards. Reconfiguring is cheap and idempotent.
   */
  const configureCtx = () => ctx.configure({
    device, format: canvasFormat, viewFormats: [format], alphaMode,
  })
  configureCtx()

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
    { texture: GPUTexture; labelTexture: GPUTexture | null; bindGroup: GPUBindGroup;
      /** The `planeVersion` this slot's BYTES describe. On a 2D plane switch `setZPlane`
       *  bumps the renderer's counter; any slot stamped with the older number is stale (its
       *  texture holds the previous plane's data) and `show`/`hasTimepoint` treat it as a
       *  cache miss. Aged out lazily by the LRU as new t's uploads land — not `dropAll`d up
       *  front, so a wheel scroll through many z planes doesn't freeze the main thread on the
       *  Dml3RG-shape stores' 200+ ms texture-destroy loop. */
      planeVersion: number }>()
  /** Monotonically-incrementing generation counter, bumped by `setZPlane`. Matches slot
   *  stamps to name freshness without a per-slot boolean that would need resetting. */
  let planeVersion = 0
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
  /** XY dimensions actually loaded — `meta.nX`/`meta.nY` at level 0, else the coarser pyramid level's
   *  dimensions. Set by `setImage`, used by `uploadFrame`'s texture allocation and by the u12/u13
   *  shader uniform (dims.x/y). Level does NOT change the physical extent — a level-1 volume of a 3.3
   *  mm image is still 3.3 mm across, just half the voxels — so camera fitting stays on `meta.nX`. */
  let renderNX = 0
  let renderNY = 0
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
  /** Clear the canvas to a flat colour instead of rendering — see `setTestPattern`. */
  let testPattern = false
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

  /**
   * Encode the volume and, optionally, the overlays into an open pass. ONE encoder for both the canvas
   * and the probe, so "what the probe renders" and "what the screen renders" cannot drift — and the
   * drift is exactly what a blank canvas with a working shader would be.
   *
   * `withOverlays` exists because the overlays are the one part that can invalidate the WHOLE pass: an
   * invalid overlay pipeline or an instanced draw past the end of its buffer discards everything in
   * the pass, including the volume drawn into it first. Rendering the pass twice, with and without,
   * says whether that is happening without anyone having to reason about it.
   */
  function encodePass(pass: GPURenderPassEncoder, withOverlays: boolean) {
    pass.setPipeline(pipeline)
    pass.setBindGroup(0, bindGroup!)
    pass.draw(3)
    if (!withOverlays) return
    // Overlays go in the SAME pass, after the volume: `loadOp: 'clear'` has already run, so this
    // blends over a finished MIP without a second attachment or a second clear. Tails first, then
    // points: a marker has to sit ON TOP of the path that leads to it, not under it.
    if (segBuf && segCount > 0) {
      pass.setPipeline(segPipeline)
      pass.setBindGroup(0, bindGroup!)
      pass.setVertexBuffer(0, segBuf)
      pass.draw(6, segCount, 0, segFirst)
    }
    if (pointBuf && pointCount > 0) {
      pass.setPipeline(pointsPipeline)
      pass.setBindGroup(0, bindGroup!)
      pass.setVertexBuffer(0, pointBuf)
      pass.draw(6, pointCount, 0, pointFirst)
    }
  }

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

  const setupError = await device.popErrorScope()
  if (setupError) throw new Error('GPU setup: ' + setupError.message)
  // Anything that goes wrong from here on is UNCAPTURED — per-frame work is not worth an error scope
  // and its round trip. Reported once and then left alone: a bad draw repeats every frame, and a
  // message that rewrites itself sixty times a second is not a message.
  let reported = false
  device.onuncapturederror = e => {
    if (reported || destroyed) return
    reported = true
    onError?.(e.error.message)
  }

  return {
    adapter: report,
    lost: device.lost,

    setImage(m: ViewerMeta, budgetBytes: number, zd = m.nZ, zLo = 0, withLabels = false,
             nxRender = m.nX, nyRender = m.nY) {
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
      renderNX = Math.max(1, nxRender)
      renderNY = Math.max(1, nyRender)
      const nch = Math.min(m.nC, MAX_CHANNELS)
      labels = withLabels
      // The mask is part of what a timepoint costs, so it is part of what decides how many fit. Leaving
      // it out would let the cache promise a capacity it cannot hold, and the frame that discovers that
      // is an out-of-memory scope firing mid-scrub rather than a smaller cache. Sized in RENDER voxels,
      // so a coarser pyramid level shrinks the cost quadratically — the whole reason the 3D view can
      // load a big-XY image at all (`pickVolumeLevel` picks the deepest by default).
      bytesPerTimepoint = renderNX * renderNY * depth *
        (m.bytesPerVoxel * nch + (withLabels ? LABEL_BPV : 0))
      allowed = Infinity                       // a new shape gets a fresh chance at the limit
      byteCap = cacheCapacity(budgetBytes, bytesPerTimepoint)
      recap()
      // Extent is the PHYSICAL box (µm), not the pixel grid — a coarser level is the SAME 3.3 mm image
      // at fewer voxels, so `extentUm` stays on `m.nX`/`m.nY`. Only the texture dimensions shrink.
      const [ex, ey, ez] = extentUm(m, depth)
      u[8] = ex; u[9] = ey; u[10] = ez
      // ext.w — where the loaded slab STARTS up the stack, in µm. Overlay coordinates are absolute, so
      // without this a cropped 3D view would draw them against a box that no longer begins at zero.
      u[11] = Math.max(0, zLo) * (m.voxelUm[2] || 1)
      // dims.z is ONE channel's own depth, not the stacked height: the ray marches one channel's box
      // and the shader offsets by `c * zpc` to reach the others. Using the stacked height here squashes
      // every channel into 1/nch of the volume — a render that looks like a thin slab of real data.
      u[12] = renderNX; u[13] = renderNY; u[14] = depth; u[15] = depth
      u[4] = nch
      setChannels(m.channels)
    },

    async uploadFrame(t: number, channelBytes: ArrayBuffer[], keep: number,
                          labelBytes: ArrayBuffer | null = null) {
      const m = meta
      if (!m || !usable()) return
      const nch = Math.min(m.nC, MAX_CHANNELS)
      // Snapshot the renderer's geometry — `setImage` can rewrite `renderNX`/`renderNY`/`depth`
      // across either of the awaits below (a level swap driven by a zoom gesture is exactly when
      // this fires). If we allocated at the OLD dims and wrote at the NEW ones, `writeTexture` throws
      // `Texture copy range … touches outside of Texture` — measured on FtGoJO: 1012 create vs 2024
      // write. Snapshot everything derived from geometry too, so no closure read after an await can
      // pick up a stale value.
      const nx = renderNX, ny = renderNY, dp = depth
      // Channels stacked along z in one texture: one binding, one loop in the shader.
      //
      // ALLOCATED INSIDE AN OOM ERROR SCOPE, which is the whole reason a too-large cache can no longer
      // take the viewer down. WebGPU deliberately exposes no "free VRAM" figure, so neither we nor the
      // user can compute a safe budget — asking them for one was a question with no answerable form,
      // and setting it too high lost the device. `out-of-memory` is a scoped, recoverable error: the
      // texture comes back invalid instead, and the cache simply holds at whatever did fit.
      device.pushErrorScope('out-of-memory')
      // Textures are sized in RENDER voxels — a level-1 volume of a 3.3 mm image is half the width of
      // level-0, so the buffer is 1/4 the bytes, which is the whole reason the 3D view can load big-XY
      // images at all (the client picks the coarsest level by default via `pickVolumeLevel`).
      // Format keys on the store dtype — 8-bit sources (Manual IBEX .ims → `|u1`) allocate `r8uint`,
      // 16-bit sources (Automated IBEX → `>u2`) keep `r16uint`. The mip shader binds `texture_3d<u32>`
      // in both cases and reads `.r` as a u32; contrast/LUT max already keys on `bytesPerVoxel`.
      const fmt: GPUTextureFormat = m.bytesPerVoxel === 1 ? 'r8uint' : 'r16uint'
      const texture = device.createTexture({
        size: [nx, ny, dp * nch], dimension: '3d', format: fmt,
        usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST,
      })
      // The mask goes in the SAME error scope and the same slot as the image it annotates. One
      // allocation failing has to take the pair down together: a slot holding a volume and no mask
      // would render the image with the outlines silently missing.
      const labelTexture = (labels && labelBytes) ? device.createTexture({
        size: [nx, ny, dp], dimension: '3d', format: 'r32uint',
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
      // If the renderer moved to a new geometry across the OOM-scope await (a level swap raced the
      // upload — `setImage` already ran `dropAll`), the fetched bytes describe the OLD level and the
      // slot would render at the wrong resolution. Destroy and abandon; the caller will refetch.
      if (nx !== renderNX || ny !== renderNY || dp !== depth) {
        if (!dead) { texture.destroy(); labelTexture?.destroy() }
        return
      }
      if (labelTexture && labelBytes) {
        device.queue.writeTexture(
          { texture: labelTexture }, labelBytes,
          { bytesPerRow: nx * LABEL_BPV, rowsPerImage: ny },
          [nx, ny, dp],
        )
      }
      for (let c = 0; c < Math.min(channelBytes.length, nch); c++) {
        device.queue.writeTexture(
          { texture, origin: [0, 0, c * dp] },
          channelBytes[c],
          { bytesPerRow: nx * m.bytesPerVoxel, rowsPerImage: ny },
          [nx, ny, dp],
        )
      }
      // `writeTexture` returns once the bytes are STAGED, so without this the caller times the CPU-side
      // copy and not the transfer — an upload cost under-reported by most of itself. The other
      // direction is a trap too: `onSubmittedWorkDone` has a ~100 ms quantum on this driver, so
      // anything faster than that is unmeasurable through it (it is what voided the audit's first G2).
      await device.queue.onSubmittedWorkDone()
      // The device can go during that await, and everything below allocates against it.
      if (!usable()) { if (!dead) { texture.destroy(); labelTexture?.destroy() } return }
      // And again — the geometry could have moved while we waited for the queue. Registering a slot
      // whose textures don't match the current uniforms puts a wrong-resolution frame on screen.
      if (nx !== renderNX || ny !== renderNY || dp !== depth) {
        if (!dead) { texture.destroy(); labelTexture?.destroy() }
        return
      }

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
      slots.set(t, { texture, labelTexture, bindGroup: bg, planeVersion })
      touch(t)
      for (const gone of lruEvictions(order, capacity, spare(keep))) dropSlot(gone)
    },

    show(t: number): boolean {
      const slot = slots.get(t)
      // A slot from an older planeVersion holds the previous plane's bytes — treat as a miss
      // so the caller re-fetches. See `setZPlane` for the rationale.
      if (!slot || slot.planeVersion !== planeVersion) return false
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

    hasTimepoint(t: number) {
      const slot = slots.get(t)
      return slot !== undefined && slot.planeVersion === planeVersion
    },
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
    setZPlane(zLo: number) {
      // Fast plane switch (2D plane view). `setImage` would `dropAll` every cached texture and
      // reallocate — measured 200+ ms of sync main-thread work on Dml3RG with Keep=all (181
      // slots × ~1 ms per `texture.destroy()`). The texture SHAPE stays the same across plane
      // switches (renderNX × renderNY × depth × nch — none of those move), so we only need to
      // invalidate the CONTENTS: bump `planeVersion`, and the mismatch turns show/hasTimepoint
      // into cache misses that route the caller through `uploadFrame` on next visit. Stale
      // textures age out lazily via `dropSlot` inside uploadFrame's re-upload path — no
      // upfront destroy loop.
      if (!meta || !usable()) return
      u[11] = Math.max(0, zLo) * (meta.voxelUm[2] || 1)
      pushUniforms()
      planeVersion++
      // The currently-drawn slot is now stale — release the bind group so the next `draw` sees
      // nothing to paint (blank canvas until show(t) rebinds a fresh slot). Caller (ViewerWindow
      // `zPump`) fires gotoT/showT immediately after, which kicks the fetch.
      bindGroup = null
      boundT = -1
    },

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
                   planeLo: number, planeHi = planeLo, borderPx = 0) {
      pointFirst = Math.max(0, Math.floor(first))
      pointCount = Math.max(0, Math.floor(count))
      u[16] = Math.max(1, sizePx)
      u[17] = planeLo
      u[19] = Math.max(planeLo, planeHi)
      // Piggy-backs on the labels vec4's unused .w slot — see SHARED_WGSL in `mipShader.ts`. Zero
      // keeps the point shader on its pre-border path (same fragment output as before).
      u[LAB0 + 3] = Math.max(0, borderPx)
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

    setOverlaySegmentDraw(first: number, count: number, widthPx: number,
                          planeLo: number, planeHi = planeLo) {
      segFirst = Math.max(0, Math.floor(first))
      segCount = Math.max(0, Math.floor(count))
      u[18] = Math.max(1, widthPx)
      // Segments' own plane bounds live in pan.z / pan.w — see SEGMENTS_WGSL. Points' bounds stay
      // on ov.y / ov.w so widening the tail's z-reach doesn't drag the points into planes they
      // don't belong on.
      u[26] = planeLo
      u[27] = Math.max(planeLo, planeHi)
    },

    setLabelStyle(opacity: number, contourPx: number) {
      u[LAB0] = Math.max(0, Math.min(1, opacity))
      u[LAB0 + 1] = Math.max(0, Math.round(contourPx))
      u[LAB0 + 2] = LABEL_PALETTE_N
    },

    async sampleFrame(withOverlays = false) {
      if (!usable() || !bindGroup) return null
      // Its own square target rather than a copy of the canvas: a canvas texture is transient (it
      // belongs to the compositor between frames) and copying one needs a COPY_SRC usage on the
      // context, which would change how every real frame is presented for the sake of a diagnostic.
      const N = PROBE_PX
      const tex = device.createTexture({
        size: [N, N], format, usage: GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.COPY_SRC,
      })
      // bytesPerRow must be a multiple of 256; PROBE_PX * 4 already is, which is why it is 128 and not
      // an arbitrary 100.
      const buf = device.createBuffer({ size: N * N * 4, usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ })
      // The probe is square, so tell the shader that — `aspect` comes from these two and a stretched
      // aspect would frame the volume differently from what is on screen.
      const [w, h] = [u[5], u[6]]
      u[5] = N; u[6] = N
      pushUniforms()
      const enc = device.createCommandEncoder()
      const pass = enc.beginRenderPass({
        colorAttachments: [{
          view: tex.createView(),
          clearValue: { r: 0, g: 0, b: 0, a: 1 }, loadOp: 'clear', storeOp: 'store',
        }],
      })
      encodePass(pass, withOverlays)
      pass.end()
      enc.copyTextureToBuffer({ texture: tex }, { buffer: buf, bytesPerRow: N * 4 }, [N, N])
      device.queue.submit([enc.finish()])
      // Put the real canvas size back before anything else draws.
      u[5] = w; u[6] = h
      pushUniforms()
      try {
        await buf.mapAsync(GPUMapMode.READ)
        const px = new Uint8Array(buf.getMappedRange().slice(0))
        let max = 0, sum = 0, lit = 0
        for (let i = 0; i < px.length; i += 4) {
          const m = Math.max(px[i], px[i + 1], px[i + 2])
          if (m > 0) lit++
          if (m > max) max = m
          sum += px[i] + px[i + 1] + px[i + 2]
        }
        return {
          max: max / 255, mean: sum / (px.length / 4 * 3) / 255,
          lit: lit / (px.length / 4), size: N,
        }
      } catch { return null }
      finally { buf.destroy(); tex.destroy() }
    },

    uniformState() {
      return {
        dist: u[2], ext: [u[8], u[9], u[10]] as [number, number, number],
        pan: [u[24], u[25]] as [number, number],
        steps: u[3], ortho: u[7] > 0.5, nch: u[4],
        canvas: [u[5], u[6]] as [number, number],
      }
    },

    resize(): boolean {
      const dpr = window.devicePixelRatio || 1
      const w = Math.max(1, Math.round(canvas.clientWidth * dpr))
      const h = Math.max(1, Math.round(canvas.clientHeight * dpr))
      if (canvas.width === w && canvas.height === h) return false
      canvas.width = w; canvas.height = h
      // Reattach the swap chain to the canvas it just reset. See `configureCtx`.
      if (usable()) configureCtx()
      return true
    },

    overlayCounts(): [number, number] {
      return [pointBuf ? pointCount : 0, segBuf ? segCount : 0]
    },

    setAlphaMode(mode: GPUCanvasAlphaMode) {
      if (mode === alphaMode || !usable()) return
      alphaMode = mode
      configureCtx()
    },

    setTestPattern(on: boolean) { testPattern = on },

    draw() {
      if (!usable() || !bindGroup) return
      u[3] = steps
      u[5] = canvas.width; u[6] = canvas.height
      pushUniforms()
      const enc = device.createCommandEncoder()
      const pass = enc.beginRenderPass({
        colorAttachments: [{
          // Explicit sRGB view over the linear canvas base — gamma-encodes the shader's linear
          // output at write time so the on-screen frame matches the offline movie renderer's
          // `_linear_to_srgb`. Same reasoning as the pipeline `targets: [{ format }]` above.
          view: ctx.getCurrentTexture().createView({ format }),
          // Magenta, and nothing drawn over it — see `setTestPattern`. A colour no image contains, so
          // "did it work" needs no interpretation.
          clearValue: testPattern ? { r: 1, g: 0, b: 1, a: 1 } : { r: 0, g: 0, b: 0, a: 1 },
          loadOp: 'clear', storeOp: 'store',
        }],
      })
      if (testPattern) {
        pass.end()
        device.queue.submit([enc.finish()])
        return
      }
      encodePass(pass, true)
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
      // Detach the canvas swap chain BEFORE the device dies. Without the `unconfigure()` step,
      // `device.destroy()` on the still-bound context left the swap chain in a state the next
      // `ctx.configure(newDevice)` couldn't fully recover from — 2D→3D rendered an empty
      // canvas (Dominik, 2026-09-03). Skipping `device.destroy()` instead leaked ~2 GB of
      // texture pool and brick's next atlas alloc OOM'd. Both steps, in this order.
      ctx.unconfigure()
      device.destroy()
    },
  }
}
