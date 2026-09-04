// Segmentation masks in the browser volume viewer (docs/todo/WEB_VIEWER_PLAN.md → P4). Pure — the
// texture and the draw are in `lib/webgpu/`.
//
// A mask is another zarr of the same geometry, so it rides the SAME slab route, the same shape guard
// and the same timepoint slot as the image: `/api/viewer/slab?labels=<value_name>`. Uploading it into
// the image's slot is the point, not an optimisation — a mask cached separately can be one frame behind
// the pixels it outlines, and an outline that is one frame stale is worse than no outline, because it
// still looks like an answer.
//
// the viewer's Labels layer is the parity bar for the 2D view: filled at 0.7 opacity by default, with an
// optional `contour` outline in voxels (`python/cecelia/utils/viewer_utils.py` → `add_labels`). For 3D
// there is NO bar — viewer cannot project a Labels layer at all — so the shader takes the nearest
// surface along the ray rather than a maximum, which is the only reading of "the label you can see".

import { distinctColors } from '../plots/plot'

/** the viewer's `add_labels` default, kept so a mask reads the same in both viewers. */
export const LABEL_OPACITY = 0.7

/**
 * Palette rows. Label ids map by `id % LABEL_PALETTE_N`, so this is "how many cells can touch before
 * two of them share a colour" — 64 is far past what a person distinguishes by hue, and the cost is one
 * 64x1 texture.
 */
export const LABEL_PALETTE_N = 64

/** `hsl(H S% L%)` → three 0-255 bytes. Only the form `distinctColors` emits; anything else throws
 *  rather than silently returning black, which would render as "that cell has no mask". */
export function hslCssToRgb(css: string): [number, number, number] {
  const m = /^hsl\(\s*([\d.]+)\s*,?\s+([\d.]+)%\s*,?\s+([\d.]+)%\s*\)$/.exec(css.trim())
  if (!m) throw new Error(`not an hsl() colour: ${css}`)
  const h = Number(m[1]) / 360, s = Number(m[2]) / 100, l = Number(m[3]) / 100
  // CSS Color 4 §7.1, the reference conversion.
  const c = (1 - Math.abs(2 * l - 1)) * s
  const x = c * (1 - Math.abs(((h * 6) % 2) - 1))
  const mm = l - c / 2
  const seg = Math.floor(h * 6) % 6
  const rgb = [[c, x, 0], [x, c, 0], [0, c, x], [0, x, c], [x, 0, c], [c, 0, x]][seg]
  return rgb.map(v => Math.round((v + mm) * 255)) as [number, number, number]
}

/**
 * The label palette as an `rgba8unorm` row, ready for `writeTexture`.
 *
 * `distinctColors` is the house "N visually distinct colours" helper (`plots/plot.ts`) — a golden-angle
 * hue rotation, deterministic. That property is what makes it the right one here rather than a
 * categorical palette: consecutive ids land on consecutive rows, segmentation labels neighbouring cells
 * with near-consecutive ids, and the golden angle puts consecutive rows as far apart in hue as they can
 * be. Two touching cells therefore come out contrasting, which is the only thing a mask palette has to
 * do. the viewer's own shuffled colormap is not reproducible here and was never part of the parity bar.
 */
export function labelPaletteBytes(n = LABEL_PALETTE_N): Uint8Array {
  const out = new Uint8Array(n * 4)
  distinctColors(n).forEach((css, i) => {
    const [r, g, b] = hslCssToRgb(css)
    out[i * 4] = r; out[i * 4 + 1] = g; out[i * 4 + 2] = b; out[i * 4 + 3] = 255
  })
  return out
}

/**
 * A label slab as the `r32uint` texture wants it, whatever width the store used.
 *
 * Every label store in the projects here is UInt32 and `X-Slab-Bpv` says so, but a UInt16 one is
 * legal and reads as a perfectly plausible mask at half the width — every id doubled, every second
 * plane garbage. Widening is a copy over ids, which is cheap next to the fetch; refusing outright
 * would fail a store that is not actually wrong.
 */
export function widenLabelSlab(buf: ArrayBuffer, bytesPerVoxel: number): ArrayBuffer {
  if (bytesPerVoxel === 4) return buf
  if (bytesPerVoxel === 2) return Uint32Array.from(new Uint16Array(buf)).buffer
  if (bytesPerVoxel === 1) return Uint32Array.from(new Uint8Array(buf)).buffer
  throw new Error(`label slab has ${bytesPerVoxel} bytes per voxel, which is not a label width`)
}

/** `X-Slab-Bpv` → the number, defaulting to 4 (what every real store is) when the header is absent. */
export function labelBpv(header: string | null): number {
  const n = Number(header)
  return Number.isFinite(n) && n > 0 ? n : 4
}
