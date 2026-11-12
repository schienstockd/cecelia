// Colour-vision-deficiency separation — can two colours be told apart by someone with protanopia,
// deuteranopia or tritanopia? Used by `cvd.test.ts` to hold the app's STATE colours (severity, the
// status ink, warn vs danger) apart in every colour scheme, read straight from style.css.
//
// Method — two published parts, and the threshold is calibrated to exactly this pair:
//   * CVD simulation: Machado, Oliveira & Fernandes (2009), "A Physiologically-based Model for
//     Simulation of Color Vision Deficiency", IEEE TVCG 15(6):1291–1298, DOI 10.1109/TVCG.2009.113 —
//     the severity-1.0 matrices, applied in linear sRGB and clamped to gamut.
//   * Distance: Euclidean distance in OKLab ×100 (Ottosson 2020, https://bottosson.github.io/posts/oklab/).
// Thresholds follow the dataviz validator this was cross-checked against: ≥ 8 under protan AND deutan
// for colours that must be told apart; 6–8 only with a second cue (a label, an icon); ≥ 15 under
// normal vision. Golden values from that validator are asserted in `cvd.test.ts`.

import { parseHex } from './colour'

export type Vision = 'normal' | 'protan' | 'deutan' | 'tritan'

const MACHADO: Record<Exclude<Vision, 'normal'>, number[][]> = {
  protan: [[0.152286, 1.052583, -0.204868], [0.114503, 0.786281, 0.099216], [-0.003882, -0.048116, 1.051998]],
  deutan: [[0.367322, 0.860646, -0.227968], [0.280085, 0.672501, 0.047413], [-0.011820, 0.042940, 0.968881]],
  tritan: [[1.255528, -0.076749, -0.178779], [-0.078411, 0.930809, 0.147602], [0.004733, 0.691367, 0.303900]],
}

const toLinear = (c: number) => c <= 0.04045 ? c / 12.92 : ((c + 0.055) / 1.055) ** 2.4

function linearRgb(hex: string, vision: Vision): [number, number, number] {
  const rgb = parseHex(hex)
  if (!rgb) throw new Error(`not a hex colour: ${hex}`)
  const lin = rgb.map(v => toLinear(v / 255)) as [number, number, number]
  if (vision === 'normal') return lin
  const m = MACHADO[vision]
  return m.map(row => Math.min(1, Math.max(0, row[0] * lin[0] + row[1] * lin[1] + row[2] * lin[2]))) as [number, number, number]
}

function oklab([r, g, b]: [number, number, number]): [number, number, number] {
  const l = Math.cbrt(0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b)
  const m = Math.cbrt(0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b)
  const s = Math.cbrt(0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b)
  return [
    0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s,
    1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s,
    0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s,
  ]
}

/** OKLab ΔE ×100 between two hex colours as seen with `vision`. */
export function deltaE(a: string, b: string, vision: Vision = 'normal'): number {
  const p = oklab(linearRgb(a, vision)), q = oklab(linearRgb(b, vision))
  return 100 * Math.hypot(p[0] - q[0], p[1] - q[1], p[2] - q[2])
}

/** Two colours must be told apart under red-green CVD: the worse of protan and deutan. */
export const cvdSeparation = (a: string, b: string) => Math.min(deltaE(a, b, 'protan'), deltaE(a, b, 'deutan'))

/** CVD target for colours that must be told apart on colour alone; the floor is legal only with a second cue. */
export const CVD_TARGET = 8
export const CVD_FLOOR = 6
export const NORMAL_FLOOR = 15
