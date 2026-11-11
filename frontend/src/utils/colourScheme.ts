// The app's colour scheme — which value set the `--cc-*` colour tokens take. Dark is the default and
// needs no attribute; any other scheme is `data-theme="<id>"` on <html>, matched by a
// `:root[data-theme="<id>"]` block in style.css that overrides the scheme-following tokens.
// Chosen from the avatar menu (one-click switch) or Preferences → Interface, kept per profile (stores/settings.ts), applied before the first
// paint by main.ts so a light-scheme user never sees a dark flash. docs/UI.md → *Colour schemes*.

export type ColourScheme = 'dark' | 'light'

export const COLOUR_SCHEMES: ReadonlyArray<{ value: ColourScheme, label: string }> = [
  { value: 'dark',  label: 'Dark' },
  { value: 'light', label: 'Light' },
]

/** The localStorage key — read by main.ts before the store exists, written by the settings store. */
export const COLOUR_SCHEME_KEY = 'cc.colourScheme'

/** A stored value as a scheme; anything unknown (absent, a removed scheme) is dark. */
export function parseColourScheme(v: unknown): ColourScheme {
  return COLOUR_SCHEMES.some(s => s.value === v) ? v as ColourScheme : 'dark'
}

/** Point the tokens at `scheme`. Dark is the attribute's absence, so a missing attribute is never wrong. */
export function applyColourScheme(scheme: ColourScheme, root: { dataset: DOMStringMap }): void {
  if (scheme === 'dark') delete root.dataset.theme
  else root.dataset.theme = scheme
}

/** The scheme a one-click switch goes to: the next in the list, wrapping — with two, the other one. */
export function nextColourScheme(scheme: ColourScheme): ColourScheme {
  const i = COLOUR_SCHEMES.findIndex(s => s.value === scheme)
  return COLOUR_SCHEMES[(i + 1) % COLOUR_SCHEMES.length].value
}

/** Its menu label — named by what you switch TO, so the item reads as the action. */
export function colourSchemeLabel(scheme: ColourScheme): string {
  return `${COLOUR_SCHEMES.find(s => s.value === scheme)!.label} colours`
}
