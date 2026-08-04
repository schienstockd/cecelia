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
 * downstream (channel pickers, napari, measurement) then offers it. A mismatch here almost always
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
