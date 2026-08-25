// What the CANVAS ELEMENT actually holds, as opposed to what a shader produced.
//
// The volume viewer's blank-screen bug has two halves that every other readout conflates. The renderer
// can prove its shader works by rendering into its own texture (`sampleFrame`), and the screen can
// still be black — because that probe never touches the swap chain. The gap between them is where the
// pixels are being lost, and it has exactly two sides:
//
//   the draw never lands in `getCurrentTexture()`  →  the canvas element is black too
//   the canvas is never composited                 →  the canvas element holds the image
//
// `createImageBitmap(canvas)` snapshots the element itself, through the same path `drawImage` and
// `toBlob` use, so it answers that question without anyone having to judge what they are looking at.
// It works on a WebGPU canvas: the snapshot is defined on the canvas, not on the context.

/** Brightness of a canvas snapshot, 0-1. Same shape as the renderer's `FrameSample` so the two lines
 *  can be read against each other. */
export interface CanvasSample {
  max: number
  mean: number
  /** Fraction of sampled pixels that are not pure black. */
  lit: number
  size: number
}

/**
 * Read `el` back and measure it, downscaled to `size`×`size`.
 *
 * Returns null when the browser will not snapshot the canvas — which is itself an answer, and one
 * worth reporting rather than swallowing.
 */
export async function sampleCanvas(
  el: HTMLCanvasElement, size = 128,
): Promise<CanvasSample | null> {
  if (!el.width || !el.height) return null
  try {
    // Downscaled in the DECODE rather than by drawing a full-size bitmap: a 768x640 snapshot is 2 MB
    // and this runs on a frame that is already doing work.
    const bmp = await createImageBitmap(el, 0, 0, el.width, el.height, {
      resizeWidth: size, resizeHeight: size, resizeQuality: 'low',
    })
    const off = new OffscreenCanvas(size, size)
    const c2 = off.getContext('2d', { willReadFrequently: true })
    if (!c2) { bmp.close(); return null }
    c2.drawImage(bmp, 0, 0)
    bmp.close()
    const px = c2.getImageData(0, 0, size, size).data
    return measureRgba(px, size)
  } catch { return null }
}

/** The measurement itself, over raw RGBA bytes. Pure, so what "lit" means is pinned by a test rather
 *  than by whatever a driver happened to return the day it was written. */
export function measureRgba(px: Uint8ClampedArray | Uint8Array, size: number): CanvasSample {
  let max = 0, sum = 0, lit = 0
  for (let i = 0; i < px.length; i += 4) {
    const m = Math.max(px[i], px[i + 1], px[i + 2])
    if (m > 0) lit++
    if (m > max) max = m
    sum += px[i] + px[i + 1] + px[i + 2]
  }
  const n = px.length / 4
  return { max: max / 255, mean: sum / (n * 3) / 255, lit: lit / n, size }
}
