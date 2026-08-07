// Side-by-side version comparison — the pure half (docs/todo/MOVIE_COMPARE_PLAN.md).
//
// The picker is ONE control on both movie surfaces (the viewer's recorder and the batch panel): a
// multi-select, reorderable ChipSelect over the image's versions. The selection IS the mode — nothing
// selected records the active version, one records that version, two or more record a comparison. So
// there is no "compare" switch to keep in sync with a list, and no second code path for the ordinary
// case.
//
// Everything here is derivation the components would otherwise inline: what the persisted selection
// means against the versions an image actually has, what the movie gets called, and how much work the
// user just asked for. Kept out of the SFCs so it can be tested (docs/DEV.md → Tests).

export type CompareLayout = 'row' | 'column'
/** D4 — `reference`: column 1's contrast is applied to every column (one ruler for a correction).
 *  `version`: each column keeps the napari settings saved for its own version. */
export type CompareContrast = 'reference' | 'version'

export const COMPARE_LAYOUT_DEFAULT: CompareLayout = 'row'
export const COMPARE_CONTRAST_DEFAULT: CompareContrast = 'reference'

/** Versions that no longer exist must not silently record the wrong thing. Drops unknown and duplicate
 *  names while KEEPING the user's order — the chip order is the column order. */
export function normaliseVersions(selected: string[] | undefined, available: string[]): string[] {
  const known = new Set(available)
  const seen = new Set<string>()
  return (selected ?? []).filter(v => known.has(v) && !seen.has(v) && (seen.add(v), true))
}

/** Is this selection a side-by-side comparison? Two or more columns. */
export const isComparison = (versions: string[]): boolean => versions.length > 1

/**
 * Filename addition for a selection, or '' when there is nothing to disambiguate.
 *
 * A movie is named after the IMAGE, so recording two versions of one image would otherwise write the
 * same file twice — which is why a single version already defaults to its own name as the suffix. A
 * comparison joins them (`default-vs-af_corrected`), so it can't overwrite either single-version
 * recording. `default` alone stays blank: it is the plain movie, the one everything else is a variant
 * of.
 */
export function compareSuffix(versions: string[]): string {
  const named = versions.filter(v => v && v !== 'default')
  if (!versions.length) return ''
  if (versions.length === 1) return named[0] ?? ''
  return versions.join('-vs-')
}

/**
 * Render passes a selection costs. A comparison records each version in full and then composes them,
 * so it is N× the work of a single movie — the UI says so up front rather than letting the user
 * discover it from a progress bar that runs three times as long.
 */
export const comparePasses = (versions: string[]): number => Math.max(1, versions.length)

/**
 * Hover help for the button that starts the render, stating the cost where the user commits to it.
 *
 * This used to be a permanent line under the chips. It was the explanatory text the house style rules
 * out, and in the viewer's narrow panel it stole enough width to push the contrast picker onto a
 * second row — so the count moved onto the action button instead of sitting there forever.
 */
export function compareActionTip(versions: string[], single: string): string {
  if (!isComparison(versions)) return single
  return `Record ${versions.length} versions side by side — ${comparePasses(versions)} render passes`
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
  return normaliseVersions(list, available)
}
