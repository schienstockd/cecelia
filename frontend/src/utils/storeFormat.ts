// How a store is laid out on disk, as one short readout line for the image-metadata modal.
//
// Both zarr v2 and v3 coexist on disk indefinitely — there is no converter (docs/todo/ZARR_V3_PLAN.md
// D7), so an image imported before the bioformats2raw 0.12 upgrade stays v2 forever while a new one
// can be v3. "Which is this, and how is it chunked?" therefore has to be answerable in the UI rather
// than by reading `zarr.json` in a terminal.
//
// Kept out of the SFC so the formatting is unit-tested (docs/DEV.md → Tests).

/** The store-encoding fields `GET /api/images/stores` reports per version. All optional: a missing or
 *  unreadable store keeps its row but omits them (the modal shows "—" rather than dropping the row). */
export interface StoreEncoding {
  zarrFormat?: number
  ngffVersion?: string | null
  /** Chunk shape — for a SHARDED array this is the inner chunk, i.e. the unit of compression. */
  chunks?: number[] | null
  /** Shard shape (one file on disk), or null/absent when not sharded — which is every v2 store. */
  shard?: number[] | null
}

/**
 * An axis shape as `1×1×1×512×512`. Full shape, never abbreviated to the XY pair: t/c/z are usually 1
 * but not always (a shard may legitimately span z), and silently dropping axes is how a readout starts
 * misleading the person comparing two stores.
 */
export function formatShape(dims: number[] | null | undefined): string {
  if (!dims || dims.length === 0) return '—'
  return dims.map(d => String(d)).join('×')
}

/**
 * One line describing the layout: format, NGFF version, chunk, and shard.
 *
 * Sharding is stated as "not sharded" rather than omitted when absent. Omitting it would make an
 * unsharded store and a store whose shape we failed to read look identical, and those are different
 * answers — one is a fact about the file, the other is missing information.
 *
 * Returns `''` when there is nothing known at all, so the caller can skip the line entirely rather
 * than render a row of dashes.
 */
export function storeFormatLine(s: StoreEncoding | null | undefined): string {
  if (!s || (s.zarrFormat === undefined && !s.ngffVersion && !s.chunks)) return ''
  const parts: string[] = []
  if (s.zarrFormat !== undefined) parts.push(`zarr v${s.zarrFormat}`)
  if (s.ngffVersion) parts.push(`NGFF ${s.ngffVersion}`)
  if (s.chunks && s.chunks.length) parts.push(`chunks ${formatShape(s.chunks)}`)
  parts.push(s.shard && s.shard.length ? `shard ${formatShape(s.shard)}` : 'not sharded')
  return parts.join(' · ')
}
