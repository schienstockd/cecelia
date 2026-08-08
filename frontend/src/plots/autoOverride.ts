// AUTO-OVERRIDES: settings the app changed on the user's behalf, and how it says so.
//
// Sometimes a chosen option cannot be honoured and the app substitutes another. That is fine — silently
// substituting is not. The user is left looking at a plot that disagrees with its own controls, with no
// way to tell whether the setting is broken or the data made it impossible.
//
// This existed already, ad hoc and twice: the gating plots amber the axis-transform select when the
// server auto-linearises a measure whose range can't take logicle (`plotmeta` reports the transform it
// actually USED). GatePlotPanel and GatePairsPanel each had their own copy of the comparison, their own
// amber class, and — in GatePlotPanel's case — a tooltip that said only "Axis transform", so the amber
// announced that something happened without ever saying what. One concept, one shape, one wording:
//
//   1. build an `AutoOverride` where the substitution is DECIDED (nowhere else knows why),
//   2. mark the affected control with `.cc-auto-override` (amber — see style.css),
//   3. use `overrideTooltip` for the hover text, so the explanation is never optional.

export interface AutoOverride {
  /** the user-facing name of the setting, as it reads on the control ("Transform", "Rotate X labels") */
  setting: string
  /** what the user asked for */
  from: string
  /** what was used instead */
  to: string
  /** why, in a few words — this is the part that was missing */
  why: string
}

/**
 * Hover text for an overridden control. One line, house style (docs/UI.md): what changed, then why.
 * Falls back to the plain label when nothing was overridden, so a call site needs no conditional.
 */
export function overrideTooltip(o: AutoOverride | null, fallback: string): string {
  return o ? `${o.setting}: using ${o.to} instead of ${o.from} — ${o.why}` : fallback
}

/**
 * SHOW THE EFFECTIVE VALUE, WRITE THE PREFERENCE.
 *
 * The other half of announcing an override, and the half that is easy to miss: an ambered control still
 * displaying the value that was *not* used tells the user their setting is being ignored. The gating
 * transform selects have always done this — the select's getter reads the transform the server USED
 * (`effXt`) while its setter writes the user's preference (`xt`), so it reads `linear` with the amber
 * border and picking something else still records what you asked for.
 *
 * `whenOverridden` is what the substitution amounts to for this control's value type — `true` for a
 * "Rotate X labels" toggle, the used transform for a select.
 *
 * Note the control becomes effectively STUCK while the override holds (writing the preference doesn't
 * change what's displayed). That is correct: the plot really is rotated, and the amber + tooltip say why.
 * It frees itself as soon as the override lifts — a wider panel, shorter labels, a compatible measure.
 */
export const effectiveOf = <T>(o: AutoOverride | null, preference: T, whenOverridden: T): T =>
  o ? whenOverridden : preference

/**
 * Do two override sets say the same thing? The emitter uses this to stay QUIET when a re-render
 * substituted exactly what the last one did — a fresh `[]` announced as news is still news to Vue,
 * and the host turns it into a readout the board writes down, which re-renders the panel, which
 * re-renders the plot ("Maximum recursive updates exceeded"). An override is four short strings, so
 * compare them; there is no identity to lean on.
 */
export function sameOverrides(a: AutoOverride[], b: AutoOverride[]): boolean {
  return a.length === b.length &&
    a.every((o, i) => o.setting === b[i].setting && o.from === b[i].from &&
                      o.to === b[i].to && o.why === b[i].why)
}

/** A one-line notice for a plot footer, when there is no single control to mark. */
export function overrideNote(overrides: AutoOverride[]): string {
  if (!overrides.length) return ''
  return `Adjusted: ${overrides.map(o => `${o.setting} → ${o.to}`).join(', ')}`
}

// ── the specific overrides ────────────────────────────────────────────────────

/**
 * Gating axis transform: the server reports the transform it actually USED, which differs from the
 * user's preference when the measure's range can't take it (a bounded / 0–1 / near-constant measure on
 * logicle collapses to a line, so the server falls back to linear). Reverts on its own when the next
 * measure is compatible — the server re-decides every request.
 */
export function transformOverride(preferred: string, used: string): AutoOverride | null {
  if (!preferred || !used || preferred === used) return null
  return { setting: 'Transform', from: preferred, to: used,
           why: "this measure's range can't use it" }
}

/**
 * Rotated x tick labels: category labels that don't fit their band overlap into an unreadable smear, so
 * they are rotated whether or not the user asked. Announced, because "Rotate X labels" is a control the
 * user owns — the plot disagreeing with an off toggle has to be explained, not just done.
 */
export function xRotationOverride(rotated: boolean, requested: boolean): AutoOverride | null {
  if (!rotated || requested) return null
  return { setting: 'X labels', from: 'horizontal', to: 'rotated',
           why: 'they would overlap' }
}

/**
 * Do the x tick labels need rotating to stay readable?
 *
 * Pure and deterministic: each of `n` categories gets an equal band of the plotting area, so a label
 * wider than its band (less a small gap) must collide with its neighbour. Measured with the same
 * canvas text metric the left margin uses, not a character count — label widths vary by more than 2×
 * between "T" and "B · Meandering".
 *
 * `plotWidth` is the OUTER width; `reserved` is the axis margins, so the difference is the band area.
 * Returns false for a width we don't know yet (0) — better one un-rotated first frame than rotating a
 * chart that had room.
 */
export function needsXRotation(
  labels: string[],
  plotWidth: number,
  measure: (s: string) => number,
  reserved = 60,
  gap = 6,
): boolean {
  if (labels.length < 2 || !(plotWidth > 0)) return false
  const band = (plotWidth - reserved) / labels.length
  if (!(band > 0)) return true                       // no room at all → rotating is the only hope
  const widest = labels.reduce((m, s) => Math.max(m, measure(String(s))), 0)
  return widest + gap > band
}
