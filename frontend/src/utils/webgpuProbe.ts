// WebGPU adapter probe shared by the volume renderer and the Settings diagnostic.
//
// THE ADAPTER TRAP — see lib/webgpu/volumeRenderer.ts comment for the full rationale. `requestAdapter({})`
// returns the INTEGRATED GPU on hybrid machines, and Firefox blanks every `adapter.info` field, so there
// is nothing to read back. `maxTextureDimension3D > 2048` is the practical proxy: discrete cards report
// 16384, integrated 2048. Same tell used by WEB_VIEWER_PLAN.md decision 3.
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
   * A PROXY, and a weak one on Linux: Mesa's `iris` reports `maxTextureDimension3D` 16384 for Intel
   * integrated, the same as a discrete card, so this reads "discrete" on a hybrid Linux laptop that is
   * actually running on the iGPU — where Mesa then segfaulted opening an image (Dominik, 2026-08-25).
   * Prefer `name` wherever it says anything.
   */
  looksDiscrete: boolean
  maxTextureDimension3D: number
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
  /** `maxTextureDimension3D > 2048` — the adapter-trap proxy for discrete silicon. */
  looksDiscrete: boolean
  hasTimestamps: boolean
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
  const report: AdapterReport = {
    maxTextureDimension3D: maxDim3D,
    looksDiscrete: maxDim3D > 2048,
    hasTimestamps: adapter.features.has('timestamp-query'),
    name: adapterName(adapter),
  }
  const device = await adapter.requestDevice()
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
      hasR16Uint: null, verdict: 'unavailable',
      reason: 'No WebGPU adapter available — check the browser flag and OS graphics settings',
    }
  }
  const maxDim3D = adapter.limits.maxTextureDimension3D
  const looksDiscrete = maxDim3D > 2048
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
