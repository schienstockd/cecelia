// THE colour glossary — every colour this app assigns a MEANING to, and the reference to consult
// before choosing a new one. Sibling to `iconLegend.ts`: one describes what a glyph means, this
// describes what a swatch means.
//
// Two audiences, one list. A **user** opens it from the header palette (`pi-palette`, beside the
// icon key) to find out why a dot is red / amber / green, or why a track is one colour rather than
// another. An **author** reads it before hard-coding a new hex, so the app keeps saying one thing
// with one colour. `docs/UI.md` → *Colours* points here.
//
// It cannot rot: `colorLegend.test.ts` scans the design-token declarations in `style.css` (every
// `--cc-*` on `:root` whose value is a colour) plus every palette + track mode in `palettes.json`,
// and fails when one is missing here — or listed here and not declared. The parity test on
// `palettes.json` already pins the JSON side to the Julia offline renderer, so a change reaches
// EVERYWHERE with one edit.
//
// Rules this list encodes:
//   * ONE meaning per token. The audit that lit up `pi-replay` doing double duty (icons) applies to
//     colour too — `--cc-warn` and `--cc-sev-warn` are separate tokens on purpose (legacy warn vs
//     canonical severity), each with its own line.
//   * ONE token per meaning. If a new surface wants "the app talking to you", it uses `--cc-guide`;
//     it does not introduce a second whitish variable.
//   * Colour is NEVER the sole cue on severity. The severity trio pairs with the icon-glossary
//     shapes (`pi-check-circle` / `pi-exclamation-triangle` / `pi-times-circle`) — see
//     `lib/severity.ts`.

import palettesJson from '../plots/palettes.json'

export type ColorSwatch =
  | { kind: 'var';      cssVar: string }              // `var(--cc-...)`, resolved by the browser at render
  | { kind: 'palette';  hexes: readonly string[] }    // a row of qualitative swatches
  | { kind: 'gradient'; hexes: readonly string[] }    // the heat ramp, rendered as a linear-gradient bar
  | { kind: 'none' }                                  // a token that names a behaviour, not a colour

export interface ColorEntry {
  /** Machine-readable id. `--cc-*` for a CSS variable; `palette:<name>` / `heat-ramp` /
   *  `track-mode:<name>` for the JSON-driven ones. Displayed in the dialog as the quiet
   *  right-hand caption, and matched by the ratchet against style.css + palettes.json. */
  token: string
  /** What it means here — one short line, the user's words not the developer's. */
  means: string
  /** How to draw the swatch. Palette/gradient values come from `palettes.json`; var swatches
   *  resolve at render time so a theme change flows through. */
  swatch: ColorSwatch
}

export interface ColorFamily {
  title: string
  /** The rule that holds the family together — shown under the heading. */
  note?: string
  entries: ColorEntry[]
}

const PALETTE_HEXES = palettesJson.palettes as Record<string, readonly string[]>
const HEAT_HEXES    = palettesJson.heatRamp as readonly string[]

// Small helper so a JSON edit doesn't need two edits: the swatch always comes from the JSON.
const paletteSwatch  = (name: string): ColorSwatch => ({ kind: 'palette',  hexes: PALETTE_HEXES[name] ?? [] })
const gradientSwatch = (hexes: readonly string[]): ColorSwatch => ({ kind: 'gradient', hexes })
const varSwatch      = (cssVar: string): ColorSwatch => ({ kind: 'var', cssVar })
const noSwatch:  ColorSwatch = { kind: 'none' }

export const COLOR_LEGEND: ColorFamily[] = [
  {
    title: 'Severity',
    note: 'Never the only cue — always with the icon-glossary shape.',
    entries: [
      { token: '--cc-sev-ok',   means: 'Passed',                                 swatch: varSwatch('--cc-sev-ok') },
      { token: '--cc-sev-warn', means: 'A warning worth checking',               swatch: varSwatch('--cc-sev-warn') },
      { token: '--cc-sev-fail', means: 'Failed',                                 swatch: varSwatch('--cc-sev-fail') },
      { token: '--cc-active',   means: 'Running now — the task light beside ok / warn / fail',
        swatch: varSwatch('--cc-active') },
    ],
  },
  {
    title: 'Attention and destructive action',
    entries: [
      { token: '--cc-warn',   means: 'Amber — favourites, stale edits, timeline warnings; not QC severity',
        swatch: varSwatch('--cc-warn') },
      { token: '--cc-danger', means: 'Red — a button ARMED to destroy or shut down; not the same as fail',
        swatch: varSwatch('--cc-danger') },
    ],
  },
  {
    title: 'App instrumentation',
    entries: [
      { token: '--cc-viewer', means: 'The viewer\'s own controls and floating panels',
        swatch: varSwatch('--cc-viewer') },
      { token: '--cc-guide',  means: 'Cecelia talking to you — guides, lab log, the compass mark',
        swatch: varSwatch('--cc-guide') },
      { token: '--cc-selected', means: 'This panel or keyframe is picked',
        swatch: varSwatch('--cc-selected') },
    ],
  },
  {
    title: 'Form chrome',
    entries: [
      { token: '--cc-accent',        means: 'The default control colour',
        swatch: varSwatch('--cc-accent') },
      { token: '--cc-accent-strong', means: 'Engaged variant — border on a pressed control',
        swatch: varSwatch('--cc-accent-strong') },
      { token: '--cc-accent-soft',   means: 'Text on an accent-tinted surface',
        swatch: varSwatch('--cc-accent-soft') },
      { token: '--cc-accent-tint',   means: 'Soft wash behind an "option is on" surface',
        swatch: varSwatch('--cc-accent-tint') },
      { token: '--cc-accent-tint-2', means: 'Its hover step',
        swatch: varSwatch('--cc-accent-tint-2') },
    ],
  },
  {
    title: 'Surfaces and ink',
    entries: [
      { token: '--cc-bg',         means: 'The page behind everything',
        swatch: varSwatch('--cc-bg') },
      { token: '--cc-surface-1',  means: 'One layer up — cards and panels',
        swatch: varSwatch('--cc-surface-1') },
      { token: '--cc-surface-2',  means: 'Two layers up — dialogs, popovers, floating chrome',
        swatch: varSwatch('--cc-surface-2') },
      { token: '--cc-console-bg', means: 'The console log ground',
        swatch: varSwatch('--cc-console-bg') },
      { token: '--cc-text',       means: 'Body text',
        swatch: varSwatch('--cc-text') },
      { token: '--cc-text-dim',   means: 'Quieter text — captions, hints, dimmed rows',
        swatch: varSwatch('--cc-text-dim') },
      { token: '--cc-border',     means: 'One surface from another',
        swatch: varSwatch('--cc-border') },
    ],
  },
  {
    title: 'Series palettes',
    note: 'Both renderers read palettes.json — one edit, both sides.',
    entries: [
      { token: 'palette:cecelia',    means: 'The house palette — yellow, steel-blue, berry, grey and their kin',
        swatch: paletteSwatch('cecelia') },
      { token: 'palette:okabe-ito',  means: 'Colourblind-safe qualitative (Okabe & Ito, CUD)',
        swatch: paletteSwatch('okabe-ito') },
      { token: 'palette:tol-bright', means: 'Paul Tol bright — high contrast',
        swatch: paletteSwatch('tol-bright') },
      { token: 'palette:tol-muted',  means: 'Paul Tol muted — softer alternative',
        swatch: paletteSwatch('tol-muted') },
      { token: 'palette:tol-light',  means: 'Paul Tol light — for light backgrounds',
        swatch: paletteSwatch('tol-light') },
    ],
  },
  {
    title: 'Sequential ramps',
    note: 'Two on purpose. BLUE_HEAT for overlays, viridis for intensity.',
    entries: [
      { token: 'heat-ramp',
        means: 'FlowJo blue-heat — gating density and track "speed" mode',
        swatch: gradientSwatch(HEAT_HEXES) },
      { token: 'viridis',
        means: 'Perceptual ramp — image plane intensity and matrix heatmap cells',
        swatch: gradientSwatch(['#440154', '#3b528b', '#21918c', '#5ec962', '#fde725']) },
    ],
  },
  {
    title: 'Track colour modes',
    entries: [
      { token: 'track-mode:track', means: 'Cycle the house palette per track — tell adjacent tracks apart',
        swatch: paletteSwatch('cecelia') },
      { token: 'track-mode:speed', means: 'Heat-ramp by segment speed — fast is hot, slow is cool',
        swatch: gradientSwatch(HEAT_HEXES) },
      { token: 'track-mode:solid', means: 'One palette colour per source — several sources stay separable',
        swatch: noSwatch },
      { token: 'track-mode:pop', means: "The parent gated population's own colour — ribbons match the pop's swatch",
        swatch: noSwatch },
    ],
  },
]

/** Every token this glossary explains. */
export function legendTokens(): Set<string> {
  return new Set(COLOR_LEGEND.flatMap(f => f.entries.map(e => e.token)))
}

/** What one token means, or `undefined` — the lookup the ratchet and the dialog share. */
export function colorMeaning(token: string): ColorEntry | undefined {
  for (const f of COLOR_LEGEND) {
    const hit = f.entries.find(e => e.token === token)
    if (hit) return hit
  }
  return undefined
}
