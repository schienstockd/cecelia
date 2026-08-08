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
// (api/src/napari_api.jl). The recording machinery under that is blind to what made two cells differ.
//
// Everything here is derivation the components would otherwise inline: what the persisted selection
// means against the items an image actually has, what the movie gets called, and how much work the
// user just asked for. Kept out of the SFCs so it can be tested (docs/DEV.md → Tests).

export type CompareLayout = 'row' | 'column'
/** D4 — `reference`: the first cell's contrast is applied to every cell (one ruler for a correction).
 *  `version`: each cell keeps the napari settings saved for its own version. */
export type CompareContrast = 'reference' | 'version'

export const COMPARE_LAYOUT_DEFAULT: CompareLayout = 'row'
export const COMPARE_CONTRAST_DEFAULT: CompareContrast = 'reference'

/** Items that no longer exist must not silently record the wrong thing. Drops unknown and duplicate
 *  names while KEEPING the user's order — the chip order is the column order. */
export function normaliseItems(selected: string[] | undefined, available: string[]): string[] {
  const known = new Set(available)
  const seen = new Set<string>()
  return (selected ?? []).filter(v => known.has(v) && !seen.has(v) && (seen.add(v), true))
}

/**
 * The LAYOUT a pair of selections means. There is nothing to choose: versions run across, masks run
 * down, and which of those actually materialises follows from how many of each are picked.
 *
 *   2 versions x 2 masks -> a 2x2 grid
 *   2 versions x 1 mask  -> one row of 2 (a plain side-by-side comparison)
 *   1 version  x 2 masks -> one row of 2 (ditto — a single list always goes side by side)
 *   1 x 1                -> one cell, an ordinary single recording
 *
 * `grid` is what the UI keys the layout toggle off: a row-vs-column choice only means something when
 * there is ONE row to point it at. Mirrors `_compare_grid` (api/src/napari_api.jl).
 */
export interface CompareShape { rows: number; cols: number; cells: number; grid: boolean }
export function compareShape(versions: string[], segmentations: string[]): CompareShape {
  const v = Math.max(1, versions.length)
  const s = Math.max(1, segmentations.length)
  const grid = versions.length > 1 && segmentations.length > 1
  const rows = grid ? s : 1
  const cols = grid ? v : Math.max(v, s)
  return { rows, cols, cells: rows * cols, grid }
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
  const what = shape.grid ? `a ${shape.cols} x ${shape.rows} grid (versions across, masks down)`
                          : `${shape.cols} side by side`
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
 * leaves the canvas alone and an empty one means "no masks" — see `_config_label_value_names`.
 */
export function segmentationsFromConfig(
  cfg: { labelValueNames?: string[] }, available: string[],
): string[] {
  return normaliseItems(cfg.labelValueNames, available)
}
