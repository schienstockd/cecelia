// Read a frame's pixels off the GPU — the ONE offscreen render + readback every renderer shares.
//
// Why not `canvas.toDataURL()`: that asks the BROWSER for a snapshot of the canvas, and a WebGPU
// canvas's snapshot is the browser's business — what it holds between frames, after a reconfigure, or
// with a fingerprinting shield in front of it differs by browser, flag and profile, and it can read
// back black while the screen shows the image. Rendering the current state into our OWN texture and
// copying it out answers from the GPU, so what is saved is what the renderer draws, on every browser.

/** `copyTextureToBuffer` requires each row to start on a 256-byte boundary. */
export const ROW_ALIGN = 256

/** The padded row stride for a `w`-px wide RGBA8 copy. */
export function paddedRowBytes(w: number): number {
  return Math.ceil(w * 4 / ROW_ALIGN) * ROW_ALIGN
}

/**
 * Tightly packed RGBA from a padded GPU copy. `bgra` swaps R and B — a `bgra8unorm` target (the
 * preferred canvas format on most platforms) copies out in that order.
 */
export function packRows(src: Uint8Array, w: number, h: number, stride: number,
                         bgra: boolean): Uint8ClampedArray<ArrayBuffer> {
  const out = new Uint8ClampedArray(w * h * 4)
  for (let y = 0; y < h; y++) {
    const s = y * stride, d = y * w * 4
    if (!bgra) { out.set(src.subarray(s, s + w * 4), d); continue }
    for (let x = 0; x < w * 4; x += 4) {
      out[d + x] = src[s + x + 2]; out[d + x + 1] = src[s + x + 1]
      out[d + x + 2] = src[s + x];  out[d + x + 3] = src[s + x + 3]
    }
  }
  return out
}

/**
 * Render one pass into a private `w`×`h` target and read it back as RGBA. `encode` draws into the
 * pass exactly as the canvas frame does; the caller points its uniforms at `w`×`h` first and restores
 * them after this returns its promise (the writes are queued in order, so restoring before the await
 * is safe). `null` when the target is past the device's texture limit or the read fails.
 */
export async function renderToPixels(
  device: GPUDevice, format: GPUTextureFormat, w: number, h: number,
  encode: (pass: GPURenderPassEncoder) => void,
): Promise<Uint8ClampedArray<ArrayBuffer> | null> {
  const max = device.limits.maxTextureDimension2D
  if (w < 1 || h < 1 || w > max || h > max) return null
  const stride = paddedRowBytes(w)
  const tex = device.createTexture({
    size: [w, h], format, usage: GPUTextureUsage.RENDER_ATTACHMENT | GPUTextureUsage.COPY_SRC,
  })
  const buf = device.createBuffer({ size: stride * h, usage: GPUBufferUsage.COPY_DST | GPUBufferUsage.MAP_READ })
  const enc = device.createCommandEncoder()
  const pass = enc.beginRenderPass({
    colorAttachments: [{
      view: tex.createView(), clearValue: { r: 0, g: 0, b: 0, a: 1 }, loadOp: 'clear', storeOp: 'store',
    }],
  })
  encode(pass)
  pass.end()
  enc.copyTextureToBuffer({ texture: tex }, { buffer: buf, bytesPerRow: stride }, [w, h])
  device.queue.submit([enc.finish()])
  try {
    await buf.mapAsync(GPUMapMode.READ)
    return packRows(new Uint8Array(buf.getMappedRange()), w, h, stride, format.startsWith('bgra'))
  } catch { return null }
  finally { buf.destroy(); tex.destroy() }
}

