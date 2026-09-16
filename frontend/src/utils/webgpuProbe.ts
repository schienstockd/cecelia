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
  /** Runtime support for `binding_array<texture_3d<u32>, N>` — the WGSL feature the multi-atlas
   *  renderer needs to actually sample `textures[1..]`. Two-part check (WGSL parse + bindGroup
   *  runtime) mirrored from `docs/todo/spike/webgpu/diagnostic.html` §A. When false, the atlas
   *  sizer clamps to N=1 so the P2 machinery stays dormant. See
   *  `docs/todo/WEBGPU_MULTI_ATLAS_PLAN.md` → Decision 6. */
  bindingArraySupported: boolean
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

/** The adapter's name as one string, or `''` when the browser blanked every field. Consecutive
 *  duplicate tokens are collapsed — Safari fills all four fields with the same generic string
 *  ("apple apple apple apple"), which reads as a stutter without adding any information. */
export function adapterNameText(n: GpuAdapterName): string {
  const parts = [n.vendor, n.architecture, n.device, n.description].filter(Boolean)
  const out: string[] = []
  for (const p of parts) {
    if (out.length === 0 || out[out.length - 1].toLowerCase() !== p.toLowerCase()) out.push(p)
  }
  return out.join(' ')
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
 * `apple` is treated as NOT-integrated regardless of the limit. M-series Apple GPUs are unified-memory
 * (technically on-die) but comfortably run the viewer, so the user-facing "performance will be reduced"
 * warning would be a false positive. An earlier version deferred to the limit, but Safari's WebGPU
 * reports the spec baseline (2048), which flipped Apple back into the reduced bucket — see the readout
 * `apple apple apple apple / maxTextureDimension3D 2048`. Trust the name here.
 */
export function classifyAdapter(name: GpuAdapterName, maxTextureDimension3D: number): boolean {
  const text = adapterNameText(name).toLowerCase()
  if (/nvidia|geforce|quadro|\brtx\b|\bgtx\b|radeon|\bamd\b|rdna/.test(text)) return true
  if (/\bapple\b/.test(text)) return true
  if (/\bintel\b|iris|llvmpipe|swiftshader|microsoft basic|software rasterizer/.test(text)) return false
  return maxTextureDimension3D > 2048
}

/** True when the adapter is Apple silicon — the caller uses this to pick honest copy ("Apple GPU"
 *  rather than "Discrete") without threading the whole name through. Same name-matching rule as
 *  `classifyAdapter`. */
export function isAppleAdapter(name: GpuAdapterName): boolean {
  return /\bapple\b/.test(adapterNameText(name).toLowerCase())
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
  // buffer size limit" error the first f8gzA2 mount hit. The adapter reports
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
  const bindingArraySupported = await probeBindingArraySupport(device)
  const report: AdapterReport = {
    maxTextureDimension3D: maxDim3D,
    maxBufferSize: device.limits.maxBufferSize,
    looksDiscrete: classifyAdapter(name, maxDim3D),
    hasTimestamps: adapter.features.has('timestamp-query'),
    name,
    bindingArraySupported,
  }
  return { adapter, device, report }
}

/**
 * Runtime probe for `binding_array<texture_3d<u32>, 4>`. Two checks — WGSL parse AND runtime
 * bindGroup — both must pass, otherwise the shader has no way to sample atlases past `[0]`.
 *
 * Mirrors `docs/todo/spike/webgpu/diagnostic.html` §A tryBindingArray exactly; kept in sync so a
 * device flip in the diagnostic (Chromium version bump, driver update) reads the same way in
 * production. Never throws. On Brave/Chromium 151 + Dawn Vulkan (2026-09-16 run, two passes)
 * this returns false because the runtime API for `resource: [view0, view1, …]` isn't shipped —
 * the WGSL side parses but createBindGroup errors with "Failed to read the 'buffer' property".
 * See `docs/todo/WEBGPU_MULTI_ATLAS_PLAN.md` → Decision 6.
 */
export async function probeBindingArraySupport(device: GPUDevice): Promise<boolean> {
  device.pushErrorScope('validation')
  let ok = false
  const created: GPUTexture[] = []
  try {
    const mod = device.createShaderModule({
      code: `@group(0) @binding(0) var atlases: binding_array<texture_3d<u32>, 4>;
             @fragment fn fs() -> @location(0) vec4<f32> {
               let v: vec4<u32> = textureLoad(atlases[0], vec3<i32>(0,0,0), 0);
               return vec4<f32>(f32(v.x), 0.0, 0.0, 1.0);
             }`,
    })
    const info = await mod.getCompilationInfo()
    if (info.messages.some(m => m.type === 'error')) {
      await device.popErrorScope()
      return false
    }
    // `arraySize` on a bindGroupLayoutEntry + `resource: <array>` on the bindGroup is the runtime
    // half of binding_array. Not in the DOM lib types yet, so cast — that's the whole reason we
    // probe rather than trust `device.features`.
    const layout = device.createBindGroupLayout({
      entries: [{
        binding: 0, visibility: GPUShaderStage.FRAGMENT,
        texture: { sampleType: 'uint', viewDimension: '3d' },
        arraySize: 4,
      } as unknown as GPUBindGroupLayoutEntry],
    })
    const views: GPUTextureView[] = []
    for (let i = 0; i < 4; i++) {
      const t = device.createTexture({
        size: [1, 1, 1], dimension: '3d', format: 'r32uint',
        usage: GPUTextureUsage.TEXTURE_BINDING,
      })
      created.push(t)
      views.push(t.createView())
    }
    device.createBindGroup({
      layout,
      entries: [{ binding: 0, resource: views as unknown as GPUTextureView }],
    })
    ok = true
  } catch {
    ok = false
  } finally {
    created.forEach(t => t.destroy())
  }
  const err = await device.popErrorScope()
  return ok && !err
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
    supported: true, adapterFound: true, looksDiscrete, hasR16Uint, isApple: isAppleAdapter(name),
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
  /** True when `adapter.info.vendor === 'apple'` — the copy branch, not the classification branch.
   *  Apple silicon reads as `looksDiscrete=true` for the verdict (it runs the viewer fine), but the
   *  reason line should not call unified-memory hardware "Discrete". */
  isApple?: boolean
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
  if (input.isApple) return { verdict: 'ready', reason: 'Apple GPU — ready' }
  return { verdict: 'ready', reason: 'Discrete GPU detected' }
}
