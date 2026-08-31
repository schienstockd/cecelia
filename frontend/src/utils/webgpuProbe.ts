// WebGPU adapter probe shared by the volume renderer and the Settings diagnostic.
//
// THE ADAPTER TRAP — see lib/webgpu/volumeRenderer.ts comment for the full rationale. `requestAdapter({})`
// returns the INTEGRATED GPU on hybrid machines. Classifying which side we landed on takes both signals:
// `adapter.info.vendor` is normalized by Chromium to `"nvidia"`/`"amd"`/`"intel"`/`"apple"` and is the
// first-class tell when it's non-empty; `maxTextureDimension3D > 2048` is the fallback for browsers
// that blank the name (Firefox has historically blanked every info field, and Dawn on Linux Vulkan
// can also report 2048 on an NVIDIA discrete adapter — the case that made this refactor).
//
// TWO ENTRY POINTS. `acquireGpuDevice()` is for the renderers — it asks for a device and throws
// `WebGpuUnavailable` when there is nothing to give back. `probeWebGpu()` is for the Settings diagnostic
// — it never throws; the whole point is to REPORT the failure state.
//
// r16uint IS mandatory in the WebGPU spec, but only some codepaths (storage / render) are — the volume
// renderer uses it as a sampled 3D texture, which is a mandatory capability. The probe still tries to
// create a 1x1x1 r16uint texture inside a validation error scope, so a broken driver surfaces here
// instead of as a blank viewer later. See docs/archive/gpu-diagnostic-prompt.md (archived brief).

export type GpuVerdict = 'ready' | 'reduced' | 'unavailable'

/** What an adapter says it IS, when it says anything — all four fields are optional in the spec and
 *  Firefox has historically blanked every one. Empty strings when the browser gives nothing. */
export interface GpuAdapterName {
  vendor: string
  architecture: string
  device: string
  description: string
}

export interface AdapterReport {
  /**
   * Whether this looks like the discrete GPU. False means the browser handed us the integrated one.
   *
   * Set by `classifyAdapter(name, maxTextureDimension3D)` — the adapter NAME first (Chromium normalises
   * vendor to `"nvidia"`/`"amd"`/`"intel"`/`"apple"` for us), the LIMIT as a fallback when the browser
   * blanks the name. Mesa's `iris` reports 16384 for Intel iGPU and Dawn/Linux reports 2048 for NVIDIA
   * discrete — either alone gets this wrong.
   */
  looksDiscrete: boolean
  maxTextureDimension3D: number
  /** The DEVICE's own `maxBufferSize` after we asked the adapter for its max. Dawn/Linux defaults it
   *  to 256 MB even on cards that can do 4 GB, and the tile atlas needs the higher figure — one
   *  1024² × slots × nC × 2 texture is a 800 MB buffer on a whole slide. */
  maxBufferSize: number
  hasTimestamps: boolean
  /** The adapter's own identification, reported rather than interpreted — the point is to put the real
   *  answer beside the proxy above instead of replacing one guess with another. */
  name: GpuAdapterName
}

/** `adapter.info`, defaulted. Typed loosely because `info` is still optional in the DOM lib on some
 *  TypeScript versions, and a missing field must read as "the browser said nothing", not as a crash. */
export function adapterName(adapter: GPUAdapter): GpuAdapterName {
  const i = (adapter as GPUAdapter & { info?: Partial<GpuAdapterName> }).info
  return {
    vendor: i?.vendor ?? '', architecture: i?.architecture ?? '',
    device: i?.device ?? '', description: i?.description ?? '',
  }
}

/** The adapter's name as one string, or `''` when the browser blanked every field. */
export function adapterNameText(n: GpuAdapterName): string {
  return [n.vendor, n.architecture, n.device, n.description].filter(Boolean).join(' ')
}

/**
 * Classify the adapter as discrete or integrated from its NAME first, then the limit as a fallback.
 *
 * `maxTextureDimension3D` alone is a weak proxy: Mesa's `iris` reports 16384 for Intel iGPU, and Dawn
 * on Linux Vulkan can report 2048 (the WebGPU spec default) even on an NVIDIA discrete card — the case
 * that flagged this bug. `adapter.info.vendor` is normalized by Chromium to a short slug ("nvidia",
 * "amd", "intel", "apple") without needing the developer-features flag, and Firefox has historically
 * blanked it entirely — hence the fallback.
 *
 * `apple` is intentionally NOT tagged integrated: M-series Apple GPUs are on-die but strong, and their
 * limits typically hit 16384 anyway. Fall through to the limit check for them.
 */
export function classifyAdapter(name: GpuAdapterName, maxTextureDimension3D: number): boolean {
  const text = adapterNameText(name).toLowerCase()
  if (/nvidia|geforce|quadro|\brtx\b|\bgtx\b|radeon|\bamd\b|rdna/.test(text)) return true
  if (/\bintel\b|iris|llvmpipe|swiftshader|microsoft basic|software rasterizer/.test(text)) return false
  return maxTextureDimension3D > 2048
}

export interface GpuLimitsDump {
  maxTextureDimension3D: number
  maxBufferSize: number
  maxStorageBufferBindingSize: number
}

export interface GpuProbeReport {
  /** `navigator.gpu` is present in this browser. */
  supported: boolean
  /** `requestAdapter({powerPreference:'high-performance'})` returned an adapter. */
  adapterFound: boolean
  /** Discrete-vs-integrated verdict from `classifyAdapter(name, limit)` — name-first, limit as
   *  fallback. See `classifyAdapter` for the details of both signals. */
  looksDiscrete: boolean
  hasTimestamps: boolean
  /** What the adapter says it is. Empty strings when the browser gives nothing, which is the state
   *  that made the proxy above necessary in the first place. */
  name: GpuAdapterName
  /** Present when an adapter was returned. */
  limits?: GpuLimitsDump
  /** r16uint sampled-texture support — the volume renderer's on-disk format. Null when we could not
   *  build a device to check. */
  hasR16Uint: boolean | null
  verdict: GpuVerdict
  /** One short line explaining the verdict, meant for the UI. */
  reason: string
}

export class WebGpuUnavailable extends Error {}

/**
 * Ask the browser for a WebGPU device with the high-performance preference. Used by the volume
 * renderer and any other GPU consumer that needs a real device.
 *
 * `powerPreference: 'high-performance'` is not advice — without it the browser picks the integrated
 * GPU on hybrid machines. Returns the report so a caller can log which side it landed on without
 * re-deriving the check.
 */
export async function acquireGpuDevice(): Promise<{
  adapter: GPUAdapter, device: GPUDevice, report: AdapterReport,
}> {
  if (!('gpu' in navigator)) throw new WebGpuUnavailable('This browser has no WebGPU')
  const adapter = await navigator.gpu.requestAdapter({ powerPreference: 'high-performance' })
  if (!adapter) throw new WebGpuUnavailable('No WebGPU adapter available')

  const maxDim3D = adapter.limits.maxTextureDimension3D
  const name = adapterName(adapter)
  // Ask the adapter for its FULL limits, not the WebGPU defaults. Dawn on Linux Vulkan defaults
  // `maxBufferSize` to 256 MB even on cards that support 4 GB — the tile atlas is a single 800 MB
  // buffer for a whole slide, so leaving it at the default is exactly the "Buffer size exceeds max
  // buffer size limit" error the first f8gzA2 mount hit (Dominik, 2026-08-25). The adapter reports
  // what it can actually give us; asking for that is not asking for anything the adapter did not
  // already offer, so this is safe on every card.
  const requiredLimits: Record<string, number> = {
    maxBufferSize: adapter.limits.maxBufferSize,
    maxStorageBufferBindingSize: adapter.limits.maxStorageBufferBindingSize,
    maxTextureDimension3D: adapter.limits.maxTextureDimension3D,
    maxTextureDimension2D: adapter.limits.maxTextureDimension2D,
  }
  // Best-effort: request `timestamp-query` when the adapter has it, so the bench harness can split
  // whole-`drawMs` into GPU render vs CPU scheduler/upload/submit. The probe already reports this
  // as `hasTimestamps`, but until we ask for it in `requiredFeatures` the device won't actually
  // expose the querySet path. Silently omitted when unsupported — no throw, and the renderer
  // handles the missing case by not creating a query set.
  const requiredFeatures: GPUFeatureName[] = []
  if (adapter.features.has('timestamp-query')) requiredFeatures.push('timestamp-query')
  const device = await adapter.requestDevice({ requiredLimits, requiredFeatures })
  const report: AdapterReport = {
    maxTextureDimension3D: maxDim3D,
    maxBufferSize: device.limits.maxBufferSize,
    looksDiscrete: classifyAdapter(name, maxDim3D),
    hasTimestamps: adapter.features.has('timestamp-query'),
    name,
  }
  return { adapter, device, report }
}

/**
 * Never-throws probe used by the Settings diagnostic. Every failure mode becomes a report field so the
 * UI can render an explanation instead of a stack trace.
 */
export async function probeWebGpu(): Promise<GpuProbeReport> {
  if (!('gpu' in navigator)) {
    return {
      supported: false, adapterFound: false, looksDiscrete: false, hasTimestamps: false,
      name: { vendor: '', architecture: '', device: '', description: '' },
      hasR16Uint: null, verdict: 'unavailable',
      reason: 'WebGPU is not available in this browser',
    }
  }
  let adapter: GPUAdapter | null = null
  try {
    adapter = await navigator.gpu.requestAdapter({ powerPreference: 'high-performance' })
  } catch {
    adapter = null
  }
  if (!adapter) {
    return {
      supported: true, adapterFound: false, looksDiscrete: false, hasTimestamps: false,
      name: { vendor: '', architecture: '', device: '', description: '' },
      hasR16Uint: null, verdict: 'unavailable',
      reason: 'No WebGPU adapter available — check the browser flag and OS graphics settings',
    }
  }
  const maxDim3D = adapter.limits.maxTextureDimension3D
  const name = adapterName(adapter)
  const looksDiscrete = classifyAdapter(name, maxDim3D)
  const hasTimestamps = adapter.features.has('timestamp-query')
  const limits: GpuLimitsDump = {
    maxTextureDimension3D: maxDim3D,
    maxBufferSize: adapter.limits.maxBufferSize,
    maxStorageBufferBindingSize: adapter.limits.maxStorageBufferBindingSize,
  }

  // r16uint check — build a throwaway device inside a validation error scope, try to create the same
  // shape of texture the volume renderer would, then drop them both. A driver that will refuse the
  // format surfaces here rather than as a blank viewer.
  let device: GPUDevice | null = null
  let hasR16Uint: boolean | null = null
  try {
    device = await adapter.requestDevice()
    device.pushErrorScope('validation')
    const tex = device.createTexture({
      size: [1, 1, 1], dimension: '3d', format: 'r16uint',
      usage: GPUTextureUsage.TEXTURE_BINDING | GPUTextureUsage.COPY_DST,
    })
    const err = await device.popErrorScope()
    hasR16Uint = !err
    tex.destroy()
  } catch {
    hasR16Uint = null
  } finally {
    device?.destroy()
  }

  const { verdict, reason } = verdictFrom({
    supported: true, adapterFound: true, looksDiscrete, hasR16Uint,
  })
  return {
    supported: true, adapterFound: true, looksDiscrete, hasTimestamps,
    name,
    limits, hasR16Uint, verdict, reason,
  }
}

/**
 * Pure decision from the probe fields. Extracted so the verdict logic can be unit-tested without a
 * WebGPU device.
 */
export function verdictFrom(input: {
  supported: boolean
  adapterFound: boolean
  looksDiscrete: boolean
  hasR16Uint: boolean | null
}): { verdict: GpuVerdict, reason: string } {
  if (!input.supported) {
    return { verdict: 'unavailable', reason: 'WebGPU is not available in this browser' }
  }
  if (!input.adapterFound) {
    return { verdict: 'unavailable', reason: 'No WebGPU adapter available' }
  }
  if (input.hasR16Uint === false) {
    return { verdict: 'unavailable', reason: 'r16uint 3D textures unsupported — the viewer cannot render' }
  }
  if (!input.looksDiscrete) {
    return {
      verdict: 'reduced',
      reason: 'Integrated GPU — performance will be reduced',
    }
  }
  return { verdict: 'ready', reason: 'Discrete GPU detected' }
}
