/**
 * Many dots in ONE blit — the large-cloud path of the gating dot plot (`PlotLayers.vue`) and the cluster map.
 *
 * **Why.** The plot draws each dot as a circle in one path per colour (`PlotLayers.vue`). That is ~64
 * `fill()` calls, but the path still holds one arc per event and the rasteriser walks every one: 600k
 * events took 1.5–2.2 s per paint in Chromium, and a gate plot paints several times per axis change
 * (axes, then dots, then each overlay). So above `SPLAT_MIN` the dots are stamped into a pixel buffer
 * instead — a precomputed antialiased disk per sub-pixel phase, composited source-over in draw order —
 * and the buffer is laid down with one `drawImage`. Same disks, same order, same colours.
 *
 * Translucent DOTS take `alpha`: each is composited at `alpha` × coverage, exactly as an arc filled under
 * that `globalAlpha` is, so overlaps build up the same way (the cluster map). That is not a translucent
 * LAYER — a dimmed layer is one composite of the whole picture and goes through `plots/dimLayer`.
 */
import { domOffscreen, type Offscreen } from './dimLayer'

/** Below this many dots the arcs are cheap and stay as they are. */
export const SPLAT_MIN = 20_000
/** Above this disk radius (device px) one filled arc per dot is cheaper than stamping it: 600k dots at
 *  radius 4 took 0.4 s stamped vs 0.9 s as arcs, at radius 9.3 (a hi-res export) 2.1 s vs 1.5 s. For a
 *  caller that fills each dot on its own; one path per colour (the gate plot) has cheaper arcs still. */
export const SPLAT_MAX_RD = 6
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
 * Stamp disks into a premultiplied RGBA buffer (`wDev`×`hDev`): each dot composited source-over
 * what is already there at `alpha` × coverage, so a later dot covers an earlier one like a later arc.
 * `x`/`y` are device px; a dot off the buffer is clipped like a canvas clips it.
 */
export function stampDisks(acc: Uint8ClampedArray, wDev: number, hDev: number,
                           disk: ReturnType<typeof diskStamps>, x: number, y: number, rgb: Rgb, alpha = 1): void {
  if (!Number.isFinite(x) || !Number.isFinite(y)) return
  const { R, stamps } = disk, K = 2 * R + 1
  const bx = Math.floor(x), by = Math.floor(y)
  const qx = Math.min(PHASES - 1, Math.floor((x - bx) * PHASES)), qy = Math.min(PHASES - 1, Math.floor((y - by) * PHASES))
  const s = stamps[qy * PHASES + qx]
  const r0 = rgb[0], g0 = rgb[1], b0 = rgb[2]
  const j0 = Math.max(0, R - by), j1 = Math.min(K, hDev - by + R)
  const i0 = Math.max(0, R - bx), i1 = Math.min(K, wDev - bx + R)
  for (let j = j0; j < j1; j++) {
    let o = ((by + j - R) * wDev + bx + i0 - R) * 4
    for (let k = j * K + i0, kEnd = j * K + i1; k < kEnd; k++, o += 4) {
      const cov = s[k]
      if (cov === 0) continue
      const a = cov * alpha, q = 1 - a
      acc[o] = r0 * a + acc[o] * q
      acc[o + 1] = g0 * a + acc[o + 1] * q
      acc[o + 2] = b0 * a + acc[o + 2] * q
      acc[o + 3] = a * 255 + acc[o + 3] * q
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
 * Paint dots of radius `r` CSS px onto `c` (which may carry a device scale) as one image. `emit` is
 * called with a `dot(px, py, rgb)` to place each dot in CSS px, in draw order; every dot is drawn at
 * `alpha` (an arc's `globalAlpha`). The image itself is laid down at full opacity, whatever `c`'s
 * `globalAlpha`. Returns false when it could not (a skewed transform, no offscreen) — the caller then
 * draws its arcs.
 */
export function splatDots(c: CanvasRenderingContext2D, w: number, h: number, r: number,
                          emit: (dot: (px: number, py: number, rgb: Rgb) => void) => void,
                          { alpha = 1, offscreen = domOffscreen }: { alpha?: number; offscreen?: Offscreen } = {}): boolean {
  const t = c.getTransform()
  if (t.b !== 0 || t.c !== 0 || t.a <= 0 || Math.abs(t.a - t.d) > 1e-9) return false
  const s = t.a, wDev = Math.max(1, Math.round(w * s)), hDev = Math.max(1, Math.round(h * s))
  const off = offscreen(wDev, hDev)
  if (!off) return false
  const img = off.ctx.createImageData(wDev, hDev)
  const disk = diskStamps(r * s)
  const d = img.data
  // stamped premultiplied (8 bits, as a canvas keeps its own pixels), then turned into the straight alpha
  // `ImageData` holds
  emit((px, py, rgb) => stampDisks(d, wDev, hDev, disk, px * s, py * s, rgb, alpha))
  for (let o = 0; o < d.length; o += 4) {
    const A = d[o + 3]
    if (A === 0 || A === 255) continue
    const k = 255 / A
    d[o] *= k; d[o + 1] *= k; d[o + 2] *= k
  }
  off.ctx.putImageData(img, 0, 0)
  const ga = c.globalAlpha
  c.globalAlpha = 1
  c.drawImage(off.image, 0, 0, w, h)
  c.globalAlpha = ga
  return true
}
