// Channel-name assignment logic for the metadata panel: parsing the one-per-line list, picking a
// REFERENCE image to copy names from, and deciding which targets a given list can legitimately be
// written to. Pure and testable — the panel only wires it to controls (docs/DEV.md → Tests).

/** The only fields any of this needs. `CciaImage` satisfies it structurally. */
export interface ChannelSubject {
  uid: string
  name: string
  sizeC?: number | null
  channelNames?: string[]
}

/** One name per line, trimmed, blanks dropped — the textarea's contract. */
export function parseChannelNameList(text: string): string[] {
  return text.split('\n').map(s => s.trim()).filter(Boolean)
}

/** The list a reference image contributes, as textarea text. */
export function channelNamesAsText(img: ChannelSubject | undefined | null): string {
  return (img?.channelNames ?? []).join('\n')
}

/** Images usable as a reference: only those that actually carry names — an unnamed image would just
 *  blank the field, which reads as a broken control rather than a deliberate choice. */
export function referenceCandidates<T extends ChannelSubject>(images: T[]): T[] {
  return images.filter(i => (i.channelNames?.length ?? 0) > 0)
}

/**
 * Split targets into the ones `n` names can be written to and the ones they can't.
 *
 * An image with a KNOWN channel count that differs from `n` is skipped: writing 4 names onto a
 * 3-channel image doesn't fail, it just records a channel that isn't there, and every consumer
 * downstream (channel pickers, viewer, measurement) then offers it. A mismatch here almost always
 * means the image doesn't share the reference's acquisition setup, so the honest outcome is to do
 * the images that match and say which were left out.
 *
 * `sizeC` absent/null = not imported yet, so there is no count to contradict — those are written.
 */
export function splitByChannelCount<T extends ChannelSubject>(
  images: T[], n: number,
): { apply: T[], skipped: T[] } {
  const apply: T[] = [], skipped: T[] = []
  for (const img of images) {
    const c = img.sizeC
    ;(c === null || c === undefined || c === n ? apply : skipped).push(img)
  }
  return { apply, skipped }
}

/** One short line naming what was left out, or '' when nothing was. Names the images: with a handful
 *  of targets "which ones" is the whole question, and the caller has no other way to find out. */
export function skippedChannelCountMsg(skipped: ChannelSubject[], n: number): string {
  if (!skipped.length) return ''
  const shown = skipped.slice(0, 3).map(i => i.name).join(', ')
  const rest  = skipped.length > 3 ? `, +${skipped.length - 3} more` : ''
  return `Skipped ${skipped.length} image(s) without ${n} channel(s): ${shown}${rest}`
}

/**
 * An intensity column's display name, resolved by its CHANNEL INDEX — mirrors Julia `_channel_label`
 * (app/src/label_props.jl): `mean_intensity_3` → `names[3]`, `nuc_mean_intensity_0` → `nuc_<names[0]>`.
 * Returns `col` when it isn't an intensity column or its channel has no name.
 *
 * Never pair `channelNames` with a column list by position: a label set with a secondary store (nuc,
 * cell, …) measures every channel twice, interleaved, so the i-th column is not channel i.
 */
export function channelLabel(col: string, names: string[]): string {
  const m = col.match(/^(?:([a-z]+)_)?(?:mean|median)_intensity_(\d+)$/)
  const name = m ? names[parseInt(m[2], 10)] : undefined
  if (!m || !name) return col
  return m[1] ? `${m[1]}_${name}` : name
}

/**
 * What a param or stored list holds for a column: an intensity column (or per-track aggregate
 * `mean_intensity_0.mean`) as its CHANNEL NAME (`CD4`, `nuc_CD4`, `CD4.mean`), anything else unchanged —
 * mirrors Julia `canonical_channel_columns` (app/src/label_props.jl; docs/todo/CHANNEL_NAMES_PLAN.md).
 * Each image resolves the name to its own column, so a pick means the same marker on every image.
 * Stays raw when there is nothing to name it by, when the name sits on two channels (the index is the
 * only identity), or when a real column in `shadow` already carries that name (a channel called `area`).
 */
export function channelColumnName(col: string, names: string[], shadow: Iterable<string> = []): string {
  // a per-track aggregate is one of track_props' numeric suffixes (`_TRACK_NUM_AGG_SUFFIXES`)
  const m = col.match(/^((?:[a-z]+_)?(?:mean|median)_intensity_)(\d+)((?:\.(?:mean|median|sum|qUp|qLow|sd))?)$/)
  if (!m) return col
  const name = names[parseInt(m[2], 10)]
  if (!name) return col
  const lc = name.toLowerCase()
  if (names.filter(n => n.toLowerCase() === lc).length > 1) return col
  const base = channelLabel(m[1] + m[2], names)
  for (const s of shadow) if (s.toLowerCase() === base.toLowerCase()) return col
  return base + m[3]
}

/**
 * A stored column selection with every legacy raw intensity column replaced by what the picker now
 * stores for it (`rename`: raw → name, from `channelColumnName`). `null` when nothing changes, so a
 * caller only re-emits a selection that actually migrated.
 */
export function migrateColumnSelection(cur: string[], rename: Record<string, string>): string[] | null {
  let changed = false
  const out = cur.map(c => {
    const n = rename[c]
    if (n !== undefined && n !== c) { changed = true; return n }
    return c
  })
  return changed ? [...new Set(out)] : null
}
