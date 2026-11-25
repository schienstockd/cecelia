/**
 * Many opaque dots in ONE blit — the gating dot plot's path for a large point count.
 *
 * **Why.** The plot draws each dot as a circle in one path per colour (`PlotLayers.vue`). That is ~64
 * `fill()` calls, but the path still holds one arc per event and the rasteriser walks every one: 600k
 * events took 1.5–2.2 s per paint in Chromium, and a gate plot paints several times per axis change
 * (axes, then dots, then each overlay). So above `SPLAT_MIN` the dots are stamped into a pixel buffer
 * instead — a precomputed antialiased disk per sub-pixel phase, composited source-over in draw order —
 * and the buffer is laid down with one `drawImage`. Same disks, same order, same colours.
 *
 * Opaque dots only: a translucent layer must go through `plots/dimLayer` (one composite for the whole
 * layer), and the caller keeps its arcs whenever `globalAlpha` is not 1.
 */
import { domOffscreen, type Offscreen } from './dimLayer'

/** Below this many dots the arcs are cheap and stay as they are. */
export const SPLAT_MIN = 20_000
/** Sub-pixel phases per axis a stamp is precomputed for (a dot lands within 1/4 px of where an arc would). */
const PHASES = 4
/** Coverage samples per pixel axis (4×4 = 16 levels of antialiasing). */
const SUB = 4

export type Rgb = readonly [number, number, number]   // 0–255

/** Coverage stamps of a disk of radius `rd` device px: `stamps[qy * PHASES + qx]` is a `(2R+1)²` grid of
 *  0–1 coverage, for a centre at `(qx + 0.5) / PHASES` px past the stamp's middle pixel's left edge. */
export function diskStamps(rd: number): { R: number; stamps: Float32Array[] } {
  const R = Math.ceil(rd) + 1, K = 2 * R + 1, r2 = rd * rd
  const stamps: Float32Array[] = []
  for (let qy = 0; qy < PHASES; qy++) for (let qx = 0; qx < PHASES; qx++) {
    const cx = (qx + 0.5) / PHASES, cy = (qy + 0.5) / PHASES
    const s = new Float32Array(K * K)
    for (let j = 0; j < K; j++) for (let i = 0; i < K; i++) {
      let hit = 0
      for (let sy = 0; sy < SUB; sy++) for (let sx = 0; sx < SUB; sx++) {
        const dx = (i - R) + (sx + 0.5) / SUB - cx, dy = (j - R) + (sy + 0.5) / SUB - cy
        if (dx * dx + dy * dy <= r2) hit++
      }
      s[j * K + i] = hit / (SUB * SUB)
    }
    stamps.push(s)
  }
  return { R, stamps }
}

/**
 * Stamp disks into an RGBA buffer (`wDev`×`hDev`, non-premultiplied, as `ImageData` holds it): each dot
 * composited source-over what is already there, so a later dot covers an earlier one like a later arc.
 * `x`/`y` are device px; a dot off the buffer is clipped like a canvas clips it.
 */
export function stampDisks(buf: Uint8ClampedArray, wDev: number, hDev: number,
                           disk: ReturnType<typeof diskStamps>, x: number, y: number, rgb: Rgb): void {
  if (!Number.isFinite(x) || !Number.isFinite(y)) return
  const { R, stamps } = disk, K = 2 * R + 1
  const bx = Math.floor(x), by = Math.floor(y)
  const qx = Math.min(PHASES - 1, Math.floor((x - bx) * PHASES)), qy = Math.min(PHASES - 1, Math.floor((y - by) * PHASES))
  const s = stamps[qy * PHASES + qx]
  for (let j = 0; j < K; j++) {
    const py = by + j - R
    if (py < 0 || py >= hDev) continue
    for (let i = 0; i < K; i++) {
      const a = s[j * K + i]
      if (a === 0) continue
      const px = bx + i - R
      if (px < 0 || px >= wDev) continue
      const o = (py * wDev + px) * 4
      const da = buf[o + 3] / 255
      const outA = a + da * (1 - a)
      const k = da * (1 - a)
      buf[o] = (rgb[0] * a + buf[o] * k) / outA
      buf[o + 1] = (rgb[1] * a + buf[o + 1] * k) / outA
      buf[o + 2] = (rgb[2] * a + buf[o + 2] * k) / outA
      buf[o + 3] = outA * 255
    }
  }
}

/** A CSS colour as 0–255 RGB, the way the canvas parses it (a theme var's value, `rgb()`, a hex). */
export function cssRgb(c: CanvasRenderingContext2D, css: string): Rgb {
  const saved = c.fillStyle
  c.fillStyle = '#000000'; c.fillStyle = css   // COLOUR-OK: a parse sentinel (an unparsable css reads black), never painted
  const v = String(c.fillStyle); c.fillStyle = saved
  const hex = /^#([0-9a-f]{6})$/i.exec(v)
  if (hex) { const n = parseInt(hex[1], 16); return [(n >> 16) & 255, (n >> 8) & 255, n & 255] }
  const m = /rgba?\(([^)]+)\)/.exec(v)
  if (m) { const p = m[1].split(',').map(Number); return [p[0] || 0, p[1] || 0, p[2] || 0] }
  return [0, 0, 0]
}

/**
 * Paint opaque dots of radius `r` CSS px onto `c` (which may carry a device scale) as one image. `emit`
 * is called with a `dot(px, py, rgb)` to place each dot in CSS px, in draw order. Returns false when it
 * could not (a skewed transform, no offscreen) — the caller then draws its arcs.
 */
export function splatDots(c: CanvasRenderingContext2D, w: number, h: number, r: number,
                          emit: (dot: (px: number, py: number, rgb: Rgb) => void) => void,
                          offscreen: Offscreen = domOffscreen): boolean {
  const t = c.getTransform()
  if (t.b !== 0 || t.c !== 0 || t.a <= 0 || Math.abs(t.a - t.d) > 1e-9) return false
  const s = t.a, wDev = Math.max(1, Math.round(w * s)), hDev = Math.max(1, Math.round(h * s))
  const off = offscreen(wDev, hDev)
  if (!off) return false
  const img = off.ctx.createImageData(wDev, hDev)
  const disk = diskStamps(r * s)
  emit((px, py, rgb) => stampDisks(img.data, wDev, hDev, disk, px * s, py * s, rgb))
  off.ctx.putImageData(img, 0, 0)
  c.drawImage(off.image, 0, 0, w, h)
  return true
}
