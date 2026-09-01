// Side-by-side comparison — the pure half (docs/todo/MOVIE_COMPARE_PLAN.md).
//
// The picker is ONE control on both movie surfaces (the viewer's recorder and the batch panel): two
// multi-select, reorderable ChipSelects — the image's versions and its segmentations. The selection IS
// the mode — nothing selected records the ordinary movie, one records that one, two or more compare.
// So there is no "compare" switch to keep in sync with a list, and no second code path for the ordinary
// case.
//
// A movie is a GRID of the two lists — image VERSIONS across the columns, segmentation MASKS down the
// rows. Two of one and one of the other degenerates to a single row (a plain side-by-side comparison,
// whichever list it came from); two of BOTH is the cross-product. There is no mode to choose: the two
// selections fully determine the layout, which is `compareShape` below and `_compare_grid` in Julia
// (api/src/movie_rail.jl). The recording machinery under that is blind to what made two cells differ.
//
// Everything here is derivation the components would otherwise inline: what the persisted selection
// means against the items an image actually has, what the movie gets called, and how much work the
// user just asked for. Kept out of the SFCs so it can be tested (docs/DEV.md → Tests).

/** How a SINGLE-row comparison is arranged: all across, all stacked, or wrapped into a near-square
 *  grid (4 cells → 2x2). Only ever consulted when the two lists have not already fixed both
 *  directions — see `compareShape`. */
export type CompareLayout = 'row' | 'column' | 'grid'
/** D4 — `reference`: the first cell's contrast is applied to every cell (one ruler for a correction).
 *  `version`: each cell keeps the settings saved for its own version. */
export type CompareContrast = 'reference' | 'version'

export const COMPARE_LAYOUT_DEFAULT: CompareLayout = 'row'
/** Every layout there is — the picker's options and the restore validator read the SAME list, so a
 *  new one cannot be offered by one and rejected by the other. */
export const COMPARE_LAYOUTS: CompareLayout[] = ['row', 'column', 'grid']
export const COMPARE_CONTRAST_DEFAULT: CompareContrast = 'reference'

/** Items that no longer exist must not silently record the wrong thing. Drops unknown and duplicate
 *  names while KEEPING the user's order — the chip order is the column order. */
export function normaliseItems(selected: string[] | undefined, available: string[]): string[] {
  const known = new Set(available)
  const seen = new Set<string>()
  return (selected ?? []).filter(v => known.has(v) && !seen.has(v) && (seen.add(v), true))
}

/**
 * The LAYOUT a pair of selections means. When BOTH lists compare something there is nothing to
 * choose — versions run across, masks run down:
 *
 *   2 versions x 2 masks -> a 2x2 grid (`fixed`)
 *   2 versions x 1 mask  -> one list of 2, arranged by `layout`
 *   1 version  x 2 masks -> ditto — a single list is the layout toggle's whole reason to exist
 *   1 x 1                -> one cell, an ordinary single recording
 *
 * `fixed` is what the UI keys the layout toggle off: the choice only means something when ONE list
 * is doing the comparing. `grid` says the RESULT is two-dimensional — either because the cross
 * product made it so, or because the user asked for the cells to be wrapped.
 *
 * Mirrors `_compare_grid` + `_wrap_grid` (api/src/movie_rail.jl).
 */
export interface CompareShape { rows: number; cols: number; cells: number; grid: boolean; fixed: boolean }
export function compareShape(versions: string[], segmentations: string[],
                             layout: CompareLayout = COMPARE_LAYOUT_DEFAULT): CompareShape {
  const v = Math.max(1, versions.length)
  const s = Math.max(1, segmentations.length)
  const fixed = versions.length > 1 && segmentations.length > 1
  if (fixed) return { rows: s, cols: v, cells: v * s, grid: true, fixed }
  const cells = Math.max(v, s)
  const { rows, cols } = wrapShape(cells, layout)
  return { rows, cols, cells, grid: rows > 1 && cols > 1, fixed }
}

/**
 * How `cells` of one list are arranged. `grid` wraps them into the squarest rectangle that holds
 * them — 4 -> 2x2, 6 -> 3x2, 5 -> 3+2 (the short last row is centred by the compositor's existing
 * padding, so a non-square count needs no special case). Two cells wrap to a single row of two,
 * which IS the row layout, so small counts need no guard either.
 */
function wrapShape(cells: number, layout: CompareLayout): { rows: number; cols: number } {
  if (layout === 'column') return { rows: cells, cols: 1 }
  if (layout !== 'grid')   return { rows: 1, cols: cells }
  const cols = Math.ceil(Math.sqrt(cells))
  return { rows: Math.ceil(cells / cols), cols }
}

/** Is this a comparison at all — i.e. more than one cell to compose? */
export const isComparison = (shape: CompareShape): boolean => shape.cells > 1

/**
 * Filename addition for a selection, or '' when there is nothing to disambiguate.
 *
 * A movie is named after the IMAGE, so recording two variants of one would otherwise write the same
 * file twice — which is why a single pick already defaults to its own name as the suffix, and several
 * join with `-vs-`. The two lists contribute one part each, joined by `_`
 * (`default-vs-af_cellpose-vs-coastal`), so no two shapes of the same image collide.
 *
 * `default` alone stays blank for a VERSION: it is the plain movie, the one everything else is a
 * variant of. That exemption does NOT carry to a mask — a label set is usually *called* `default`, and
 * a movie with its mask drawn on is not the plain movie.
 */
export function compareSuffix(versions: string[], segmentations: string[]): string {
  const vPart = versions.length > 1 ? versions.join('-vs-')
              : (versions[0] && versions[0] !== 'default' ? versions[0] : '')
  const sPart = segmentations.length > 1 ? segmentations.join('-vs-') : (segmentations[0] ?? '')
  return [vPart, sPart].filter(Boolean).join('_')
}

/**
 * Render passes a shape costs — one full recording per CELL. A grid multiplies: 2 versions x 2 masks
 * is four renders, not two. The UI states it up front rather than letting the user discover it from a
 * progress bar that runs four times as long.
 */
export const comparePasses = (shape: CompareShape): number => Math.max(1, shape.cells)

/**
 * Hover help for the button that starts the render, stating the cost where the user commits to it.
 *
 * This used to be a permanent line under the chips. It was the explanatory text the house style rules
 * out, and in the viewer's narrow panel it stole enough width to push the contrast picker onto a
 * second row — so the count moved onto the action button instead of sitting there forever.
 */
export function compareActionTip(shape: CompareShape, single: string): string {
  if (!isComparison(shape)) return single
  const what = shape.fixed  ? `a ${shape.cols} x ${shape.rows} grid (versions across, masks down)`
             : shape.grid   ? `a ${shape.cols} x ${shape.rows} grid`
             : shape.cols === 1 ? `${shape.cells} stacked`
                                : `${shape.cells} side by side`
  return `Record ${what} — ${comparePasses(shape)} render passes`
}

/**
 * The batch config's version list, migrated from the single `valueName` it used to carry.
 *
 * A per-set config persisted before comparisons existed holds `valueName` and no `valueNames`; reading
 * it as "no versions selected" would silently switch a saved batch from the corrected version back to
 * the active one. Julia applies the same fallback (`_config_value_names`), so both ends agree on what
 * an old config means.
 */
export function versionsFromConfig(
  cfg: { valueNames?: string[]; valueName?: string }, available: string[],
): string[] {
  const list = cfg.valueNames ?? (cfg.valueName ? [cfg.valueName] : [])
  return normaliseItems(list, available)
}

/**
 * The batch config's segmentation list — which label masks the movie draws, and (2+) the grid's rows.
 *
 * No migration twin to `versionsFromConfig`: no config ever carried masks (the movie path could not
 * show them at all), so absent means absent. That distinction survives to Julia, where an absent list
 * leaves the canvas alone and an empty one means "no masks".
 */
export function segmentationsFromConfig(
  cfg: { labelValueNames?: string[] }, available: string[],
): string[] {
  return normaliseItems(cfg.labelValueNames, available)
}
