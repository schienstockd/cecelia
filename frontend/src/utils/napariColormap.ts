// napari colormap NAME → a representative hex "colour" for a legend swatch.
//
// Covers the single-hue channel colormaps used for image channels (red/green/blue/…, napari's `bop`
// set, and a few single-colour extras). Continuous maps (viridis/turbo/magma/…) and unknown names
// return null — they aren't a channel tint, so the channel legend skips them. Pure + unit-tested.
const NAPARI_COLORMAP_HEX: Record<string, string> = {
  red: '#ff0000', green: '#00ff00', blue: '#0000ff',
  magenta: '#ff00ff', cyan: '#00ffff', yellow: '#ffff00',
  gray: '#d4d4d4', grey: '#d4d4d4',
  // napari `bop` single-hue colormaps (representative full-intensity colours)
  'bop blue': '#1e6fff', 'bop orange': '#ff7f0e', 'bop purple': '#9b30ff',
  // single-colour extras some pipelines use
  'i blue': '#0000ff', 'i green': '#00ff00', 'i red': '#ff0000',
}

/** Hex for a napari colormap name (case-insensitive), or null if it isn't a single-hue channel colour. */
export function napariColormapHex(name: string | null | undefined): string | null {
  if (!name) return null
  return NAPARI_COLORMAP_HEX[name] ?? NAPARI_COLORMAP_HEX[name.toLowerCase()] ?? null
}

// Reverse of `napariColormapHex`. The browser viewer stores channel colour as a 2-stop black→hex LUT
// and drops the colormap NAME, so a viewState snapshot has to reverse-lookup: what named palette is
// this hex? Feeds `buildViewState` → so a snapshot can carry a real colormap name that
// `seedConfigFromViewState` can read, instead of the always-null we used to emit (which made the
// batch/one-shot "fill from view" produce an empty channels map). Preference order matters — several
// names share a hex ('gray'/'grey', 'i red'/'red', …); the picker's canonical names are looked up
// first so a reverse from the palette round-trips.

export interface ColormapOption { value: string; label: string; hex: string }

/** A small, standard channel-colour palette for the batch-movie picker, rendered as swatches. Ordered
 *  so adjacent entries are distinct hues; blue / cyan / orange / magenta / yellow / purple are the more
 *  colour-blind-distinguishable set, with red + green kept for the classic channel assignments. Every
 *  `value` is a valid napari colormap name (passed straight to the viewer); the `hex` is derived from
 *  NAPARI_COLORMAP_HEX (one source of truth, no divergent colour list). */
export const CHANNEL_COLORMAP_OPTIONS: ColormapOption[] = (
  [
    ['blue', 'blue'], ['cyan', 'cyan'], ['green', 'green'], ['yellow', 'yellow'],
    ['bop orange', 'orange'], ['red', 'red'], ['magenta', 'magenta'],
    ['bop purple', 'purple'], ['gray', 'gray'],
  ] as [string, string][]
).map(([value, label]) => ({ value, label, hex: napariColormapHex(value) ?? '#888888' }))

// Hex → colormap name. Built from the picker's palette first (so those canonical names win when
// several map to the same hex — 'gray' over 'grey', 'red' over 'i red'), then padded with any names
// that AREN'T in the picker so a bop/i-colour still resolves.
const HEX_TO_NAPARI_COLORMAP: Record<string, string> = (() => {
  const out: Record<string, string> = {}
  for (const o of CHANNEL_COLORMAP_OPTIONS) out[o.hex.toLowerCase()] = o.value
  for (const [name, hex] of Object.entries(NAPARI_COLORMAP_HEX)) {
    const k = hex.toLowerCase()
    if (!(k in out)) out[k] = name
  }
  return out
})()

/** Napari colormap NAME for a hex (case-insensitive), or null if unknown. Exact match only — a
 *  "close" match would silently rewrite the user's colour. */
export function napariColormapForHex(hex: string | null | undefined): string | null {
  if (!hex) return null
  return HEX_TO_NAPARI_COLORMAP[hex.toLowerCase()] ?? null
}
