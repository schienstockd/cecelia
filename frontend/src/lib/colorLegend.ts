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
      { token: '--cc-danger', means: 'Red — destructive text and outlines; not the same as fail',
        swatch: varSwatch('--cc-danger') },
      { token: '--cc-danger-fill', means: 'Muted red — a filled destructive button (Quit, armed delete)',
        swatch: varSwatch('--cc-danger-fill') },
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
      { token: '--cc-check', means: 'The Checks cluster — its frame (marks only, never text)',
        swatch: varSwatch('--cc-check') },
      { token: '--cc-check-text', means: 'The Checks cluster label (check accent as text)',
        swatch: varSwatch('--cc-check-text') },
    ],
  },
  {
    title: 'Form chrome',
    entries: [
      { token: '--cc-accent',        means: 'The default control colour',
        swatch: varSwatch('--cc-accent') },
      { token: '--cc-accent-strong', means: 'Engaged variant — border on a pressed control',
        swatch: varSwatch('--cc-accent-strong') },
      { token: '--cc-border-hover',  means: 'A hovered border or scrollbar thumb — one step up from the border',
        swatch: varSwatch('--cc-border-hover') },
      { token: '--cc-primary-fill',  means: 'Saffron — the primary action (Run, Open, Create): the one "go" colour',
        swatch: varSwatch('--cc-primary-fill') },
      { token: '--cc-on-primary',    means: 'Dark text on the saffron primary action',
        swatch: varSwatch('--cc-on-primary') },
      { token: '--cc-stage-data',        means: 'Data stage — tints the sidebar heading and the active page bar',
        swatch: varSwatch('--cc-stage-data') },
      { token: '--cc-stage-populations', means: 'Populations stage — sidebar heading and active page bar',
        swatch: varSwatch('--cc-stage-populations') },
      { token: '--cc-stage-explore',     means: 'Explore stage — sidebar heading and active page bar',
        swatch: varSwatch('--cc-stage-explore') },
      { token: '--cc-stage-analysis',    means: 'Analysis stage — sidebar heading and active page bar',
        swatch: varSwatch('--cc-stage-analysis') },
      { token: '--cc-next-fill',     means: 'Plum — the page\'s Next button (the set recipe\'s next step)',
        swatch: varSwatch('--cc-next-fill') },
      { token: '--cc-accent-fill',   means: 'Filled control with white text — picked chip, engaged toggle',
        swatch: varSwatch('--cc-accent-fill') },
      { token: '--cc-accent-soft',   means: 'Text on an accent-tinted surface',
        swatch: varSwatch('--cc-accent-soft') },
      { token: '--cc-accent-tint',   means: 'Soft wash behind an "option is on" surface',
        swatch: varSwatch('--cc-accent-tint') },
      { token: '--cc-accent-tint-2', means: 'Its hover step',
        swatch: varSwatch('--cc-accent-tint-2') },
      { token: '--cc-kiwi',          means: 'Kiwi (assist cockpit) accent — the paired-assistant panel',
        swatch: varSwatch('--cc-kiwi') },
      { token: '--cc-kiwi-strong',   means: 'Engaged variant — border on a pressed kiwi control',
        swatch: varSwatch('--cc-kiwi-strong') },
      { token: '--cc-kiwi-soft',     means: 'Text on a kiwi-tinted surface',
        swatch: varSwatch('--cc-kiwi-soft') },
      { token: '--cc-kiwi-tint',     means: 'Soft wash behind an "option is on" kiwi surface',
        swatch: varSwatch('--cc-kiwi-tint') },
      { token: '--cc-claude',        means: 'Claude Code — a terminal session, set apart from Kiwi',
        swatch: varSwatch('--cc-claude') },
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
      { token: '--cc-shadow', means: 'The drop shadow under a popover, panel or dialog',
        swatch: varSwatch('--cc-shadow') },
      { token: '--cc-shadow-soft', means: 'The faint shadow under a sticky control strip',
        swatch: varSwatch('--cc-shadow-soft') },
      { token: '--cc-backdrop', means: 'The dim behind a dialog or an overlay',
        swatch: varSwatch('--cc-backdrop') },
      { token: '--cc-wash', means: 'A hover wash on a row or a bare control',
        swatch: varSwatch('--cc-wash') },
      { token: '--cc-wash-strong', means: 'The same wash, engaged',
        swatch: varSwatch('--cc-wash-strong') },
    ],
  },
  {
    title: 'Coloured text and tints',
    note: 'Ink is a hue that reads as text on the page; a tint is the soft wash behind it.',
    entries: [
      { token: '--cc-ink-red', means: 'An error message, a failed state, a destructive hover',
        swatch: varSwatch('--cc-ink-red') },
      { token: '--cc-ink-amber', means: 'A warning line, a caution label',
        swatch: varSwatch('--cc-ink-amber') },
      { token: '--cc-ink-green', means: 'Done, available, a user-written entry',
        swatch: varSwatch('--cc-ink-green') },
      { token: '--cc-ink-blue', means: 'Information, a category label',
        swatch: varSwatch('--cc-ink-blue') },
      { token: '--cc-ink-orange', means: 'Open in the viewer; a resource-pool tag',
        swatch: varSwatch('--cc-ink-orange') },
      { token: '--cc-ink-cyan', means: 'A category label (tasks in the log console)',
        swatch: varSwatch('--cc-ink-cyan') },
      { token: '--cc-tint-red', means: 'The wash behind a failed or destructive item',
        swatch: varSwatch('--cc-tint-red') },
      { token: '--cc-tint-amber', means: 'The wash behind a warning',
        swatch: varSwatch('--cc-tint-amber') },
      { token: '--cc-tint-green', means: 'The wash behind a done item',
        swatch: varSwatch('--cc-tint-green') },
      { token: '--cc-tint-blue', means: 'The wash behind a running or informational item',
        swatch: varSwatch('--cc-tint-blue') },
    ],
  },
  {
    title: 'Over images, fills and paper',
    note: 'The same in every colour scheme: what is underneath is not the page.',
    entries: [
      { token: '--cc-media-bg', means: 'Behind an image — letterbox, thumbnail ground',
        swatch: varSwatch('--cc-media-bg') },
      { token: '--cc-hud-bg', means: 'A readout or chip floating over an image or plot',
        swatch: varSwatch('--cc-hud-bg') },
      { token: '--cc-hud-bg-soft', means: 'The lighter variant — hints, counters, a veil',
        swatch: varSwatch('--cc-hud-bg-soft') },
      { token: '--cc-hud-shade', means: 'A dark ring lifting a marker off bright pixels',
        swatch: varSwatch('--cc-hud-shade') },
      { token: '--cc-hud-halo', means: 'The outline that keeps white text legible on pixels',
        swatch: varSwatch('--cc-hud-halo') },
      { token: '--cc-hud-text', means: 'Text and marks over an image',
        swatch: varSwatch('--cc-hud-text') },
      { token: '--cc-hud-text-dim', means: 'Quieter text over an image',
        swatch: varSwatch('--cc-hud-text-dim') },
      { token: '--cc-hud-wash', means: 'A drafted region over an image',
        swatch: varSwatch('--cc-hud-wash') },
      { token: '--cc-swatch-edge', means: 'The hairline round a colour swatch',
        swatch: varSwatch('--cc-swatch-edge') },
      { token: '--cc-on-solid', means: 'Text or a knob on a deep fill (a picked chip, a delete)',
        swatch: varSwatch('--cc-on-solid') },
      { token: '--cc-on-bright', means: 'Text on a bright fill (amber, a population colour)',
        swatch: varSwatch('--cc-on-bright') },
      { token: '--cc-paper', means: 'An exported or printed figure\'s white ground',
        swatch: varSwatch('--cc-paper') },
      { token: '--cc-paper-2', means: 'A raised surface on paper',
        swatch: varSwatch('--cc-paper-2') },
      { token: '--cc-paper-ink', means: 'Text on paper',
        swatch: varSwatch('--cc-paper-ink') },
      { token: '--cc-paper-ink-dim', means: 'Quieter text on paper',
        swatch: varSwatch('--cc-paper-ink-dim') },
      { token: '--cc-paper-border', means: 'A border on paper',
        swatch: varSwatch('--cc-paper-border') },
      { token: '--cc-claude-mark', means: 'Where Claude pointed on a plot or card',
        swatch: varSwatch('--cc-claude-mark') },
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
