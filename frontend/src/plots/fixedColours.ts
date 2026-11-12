// THE script-side home for colours that must NOT follow the app scheme — the paper an export lands on,
// marks drawn over the image, data-encoding hues, and neutral fallbacks. Interface chrome never lives
// here: it reads a `--cc-*` token (`tokenColour`, an inline `var()`, or `plotTheme` for plot ink/ground).
// A colour literal in script code outside the palette homes fails `cssScenarios.test.ts` → *colour
// literals in script code*. The CSS twins of the paper colours are `--cc-paper*` in style.css.
// docs/UI.md → *Colour schemes*.

// ── paper: every export (PNG / SVG / PDF board) is light, whatever the app scheme ─────────────────────
export const PAPER = '#ffffff'
export const PAPER_INK = '#111111'
export const PAPER_INK_DIM = '#555555'
export const PAPER_BORDER = '#c9ccd1'

// ── over the image / data ground ────────────────────────────────────────────────────────────────────
export const MEDIA_BG = '#000000'                    // behind microscopy pixels
export const HALO_DARK = 'rgba(0,0,0,0.7)'           // a label halo on a light ground
export const HALO_LIGHT = 'rgba(255,255,255,0.8)'    // … and on a dark one
export const LABEL_EDGE = 'rgba(0,0,0,0.55)'          // the thin border of a geom_label-style chip
/** The overlay author's neutral (`OVERLAY_GREY`, `api/src/overlay_author.jl`): an all-cells mask, an
 *  all-tracks tail — whatever a movie draws without a population colour. */
export const OVERLAY_GREY = '#9ca3af'
/** Spellings of "white" a population/gate colour may be stored as — a default that needs the ink. */
export const STORED_WHITES: ReadonlySet<string> = new Set(['white', '#fff', '#ffffff', '#fafafa'])
/** A colour VALUE that is white (a channel shown in white, a palette-less overlay) — not a ground. */
export const WHITE = '#ffffff'

// ── neutral fallbacks: a series / population / channel that has no colour of its own ─────────────────
export const NEUTRAL = '#9aa0a6'
/** A population with no stored colour — equal to the Julia default (`popmanager/picker.jl`). */
export const UNASSIGNED_POP = '#7c93b8'

// ── data encodings in the track plots (TrackScheme / TrackDiagnostics): one meaning, one hue ─────────
export const TRACK_TONE = {
  measured: '#5aa9e6',   // a track's own bars / the measurement series
  expected: '#e8a33d',   // the expectation / fit, and "needs attention" marks
  alert:    '#d9534f',   // a blocked join, an overlap, a reference line
  queued:   '#4a9d5f',   // an edit decided but not yet run
  queuedEdge: '#8fe0a3',
} as const

// ── cluster map (UMAP) — the unclustered grey and the whole-cloud backdrop ─────────────────────────────
export const UNCLUSTERED = '#555a6e'
export const CLUSTER_GHOST = { dark: '#3a3f4b', light: '#d4d7dd' } as const

/** The axes gizmo: the Blender / viewer convention for X / Y / Z, a coordinate frame not chrome. */
export const AXIS_XYZ: Record<string, string> = { X: '#e5484d', Y: '#46a758', Z: '#4a8fef' }
