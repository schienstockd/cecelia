// Shared canvas-format policy for every WebGPU renderer in this app (volume, brick-volume, tile).
//
// The problem this fixes: `navigator.gpu.getPreferredCanvasFormat()` on its own returns EITHER
// `'bgra8unorm'` OR `'rgba8unorm'` — the base, linear format. Configuring the canvas with either
// means the fragment shader's linear accumulator is written LITERALLY to the framebuffer; the
// compositor then presents those bytes with an sRGB transfer applied by the OS/browser, which
// makes mid-tones look right on some platforms and washed-out on others depending on how the
// canvas is composited. The offline movie renderer (`api/src/image_render.jl` → `_linear_to_srgb`)
// gamma-encodes its output EXPLICITLY so the two surfaces have to agree — pinning the viewer
// canvas to an sRGB view guarantees the same transfer, regardless of the platform's default.
//
// Direct sRGB canvas configuration (`format: 'bgra8unorm-srgb'`) is not spec-guaranteed for every
// browser — the supported configure formats are the LINEAR ones (`bgra8unorm`/`rgba8unorm`/
// `rgba16float`). The spec-approved recipe is `viewFormats + createView({format})`: configure
// the canvas at the preferred base, declare the sRGB view as a compatible view format, and
// create the render-pass color-attachment view with that format. WebGPU then auto-encodes the
// shader's linear output at write time. This survives every browser + GPU + OS combination.

/** Configure a WebGPU canvas context so its render pipelines emit sRGB-encoded pixels, matching
 *  the offline movie renderer's `composite_rgb` output. Returns:
 *   - `base`: the format to pass to `ctx.configure({ format: base, … })`.
 *   - `viewFormat`: the format to use for both pipeline `targets: [{ format: viewFormat }]` and
 *     the color-attachment view — `ctx.getCurrentTexture().createView({ format: viewFormat })`.
 *  Callers must pass viewFormat to BOTH sites, or GPU validation will reject the pipeline as
 *  incompatible with the attachment.
 */
export function pickSrgbCanvasFormats(): {
  base: GPUTextureFormat
  viewFormat: GPUTextureFormat
} {
  const base = navigator.gpu.getPreferredCanvasFormat()   // 'bgra8unorm' | 'rgba8unorm'
  const viewFormat = (base + '-srgb') as GPUTextureFormat
  return { base, viewFormat }
}
