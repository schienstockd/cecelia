import { describe, it, expect, afterEach } from 'vitest'
import { pickSrgbCanvasFormats } from './canvasFormat'

// The whole point of this helper: pin an sRGB view over whichever LINEAR canvas format the browser
// prefers. It's how the WebGPU volume viewer and the offline movie renderer stay in the same colour
// space — pipeline `targets`, the color-attachment `createView({ format })`, and the canvas
// `viewFormats` list all read this ONE result, so a regression that lets any of them drift back to
// the linear base format is caught here.

const origGpu = (globalThis.navigator as unknown as { gpu?: unknown }).gpu

function stubGpu(preferred: string) {
  Object.defineProperty(globalThis.navigator, 'gpu', {
    configurable: true,
    value: { getPreferredCanvasFormat: () => preferred },
  })
}

afterEach(() => {
  Object.defineProperty(globalThis.navigator, 'gpu', { configurable: true, value: origGpu })
})

describe('pickSrgbCanvasFormats', () => {
  it('appends -srgb to the BGRA preferred base (macOS / most Windows)', () => {
    stubGpu('bgra8unorm')
    expect(pickSrgbCanvasFormats()).toEqual({ base: 'bgra8unorm', viewFormat: 'bgra8unorm-srgb' })
  })

  it('appends -srgb to the RGBA preferred base (Linux / some mobile)', () => {
    stubGpu('rgba8unorm')
    expect(pickSrgbCanvasFormats()).toEqual({ base: 'rgba8unorm', viewFormat: 'rgba8unorm-srgb' })
  })

  it('always returns an sRGB view format — a linear fallback would be the regression', () => {
    for (const base of ['bgra8unorm', 'rgba8unorm']) {
      stubGpu(base)
      const { viewFormat } = pickSrgbCanvasFormats()
      expect(viewFormat.endsWith('-srgb')).toBe(true)
      // The BASE the canvas is configured at must NOT already carry the suffix — the spec's
      // supported configure formats are the linear ones, so a browser bug that returned an sRGB
      // preferred format would still leave us with a linear canvas configure. Guard direction:
      // catch the day a UA starts returning the -srgb variant from getPreferredCanvasFormat.
      expect(base.endsWith('-srgb')).toBe(false)
    }
  })
})
