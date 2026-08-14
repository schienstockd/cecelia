// Colour maths — hex parsing, alpha compositing and WCAG contrast. The ONE place these live; there was
// no colour utility in the app before, which is why `moduleTagStyle` was about to grow its own
// luminance formula. Pure and DOM-free ⇒ unit-tested.
//
// Relative luminance and contrast ratio are WCAG 2.1 (w3.org/TR/WCAG21/#dfn-relative-luminance,
// #dfn-contrast-ratio) — the sRGB linearisation with the 0.03928 knee and the 0.2126/0.7152/0.0722
// coefficients, not a naive average. Golden values are asserted against the spec's own examples.

/** `#rgb`/`#rrggbb`/`#rrggbbaa` → `[r, g, b]` in 0-255, or null. Any alpha is IGNORED (see `composite`). */
export function parseHex(hex: string): [number, number, number] | null {
  const m = /^#?([0-9a-f]{3}|[0-9a-f]{6}|[0-9a-f]{8})$/i.exec(hex.trim())
  if (!m) return null
  const h = m[1]
  if (h.length === 3) return [0, 1, 2].map(i => parseInt(h[i] + h[i], 16)) as [number, number, number]
  return [0, 2, 4].map(i => parseInt(h.slice(i, i + 2), 16)) as [number, number, number]
}

export const toHex = (rgb: readonly number[]): string =>
  '#' + rgb.map(c => Math.round(Math.max(0, Math.min(255, c))).toString(16).padStart(2, '0')).join('')

/**
 * `fg` at opacity `alpha` over `bg` — what the browser actually paints for a `#rrggbb22` fill.
 *
 * The direction matters and is easy to get backwards: this is `fg·a + bg·(1-a)`. Inverting it makes a
 * 13%-opacity tint render as a nearly-solid colour, which is a mistake that *looks* plausible.
 */
export function composite(fg: string, bg: string, alpha: number): string {
  const f = parseHex(fg), b = parseHex(bg)
  if (!f || !b) return fg
  const a = Math.max(0, Math.min(1, alpha))
  return toHex(f.map((c, i) => c * a + b[i] * (1 - a)))
}

/** Linear interpolation between two colours; `t = 0` → `a`, `t = 1` → `b`. */
export function mix(a: string, b: string, t: number): string {
  const x = parseHex(a), y = parseHex(b)
  if (!x || !y) return a
  const k = Math.max(0, Math.min(1, t))
  return toHex(x.map((c, i) => c * (1 - k) + y[i] * k))
}

const linear = (c: number) => {
  const s = c / 255
  return s <= 0.03928 ? s / 12.92 : Math.pow((s + 0.055) / 1.055, 2.4)
}

/** WCAG 2.1 relative luminance, 0 (black) to 1 (white). */
export function luminance(hex: string): number {
  const rgb = parseHex(hex)
  if (!rgb) return 0
  const [r, g, b] = rgb.map(linear)
  return 0.2126 * r + 0.7152 * g + 0.0722 * b
}

/** WCAG 2.1 contrast ratio, 1 (identical) to 21 (black on white). Symmetric in its arguments. */
export function contrastRatio(a: string, b: string): number {
  const la = luminance(a), lb = luminance(b)
  return (Math.max(la, lb) + 0.05) / (Math.min(la, lb) + 0.05)
}

/** WCAG AA for body text. Large text is 3.0, which none of our chrome qualifies as. */
export const WCAG_AA = 4.5

/**
 * `colour` mixed toward `toward` by the LEAST amount that reaches `target` contrast against `bg`.
 *
 * The point is to keep the hue's identity while making it readable: a module accent lifted 34% toward
 * white is still recognisably that module's purple, where replacing it with `--cc-text` would throw the
 * identity away. Returns `toward` if even that cannot reach the target (a caller wanting a guarantee
 * should check `contrastRatio` on the result rather than assume).
 *
 * Stepped at 1% rather than solved analytically because contrast is not monotonic in a general mix —
 * mixing a light colour toward white *lowers* contrast against a light background — and 100 luminance
 * evaluations is nothing next to being subtly wrong for one hue.
 */
export function readableOn(colour: string, bg: string, target = WCAG_AA, toward = '#ffffff'): string {
  if (contrastRatio(colour, bg) >= target) return colour
  for (let i = 1; i <= 100; i++) {
    const c = mix(colour, toward, i / 100)
    if (contrastRatio(c, bg) >= target) return c
  }
  return toward
}
