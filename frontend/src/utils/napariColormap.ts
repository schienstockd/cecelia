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
