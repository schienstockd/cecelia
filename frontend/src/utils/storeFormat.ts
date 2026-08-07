// How a store is laid out on disk, for the image-metadata modal: a short format title beside the
// value name, and the rest as labelled facts underneath.
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
  /** Chunk-key separator: `/` nests keys into a directory tree, `.` keeps them flat. */
  separator?: string | null
  /** The codec, already described by the API (`zstd + shuffle`) — not derived here. */
  label?: string | null
}

/** One labelled fact about the layout: a short key and its value. */
export interface StoreFact { k: string, v: string }

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
 * The format, for the head row beside the value name: `zarr v2 (NGFF 0.4)`.
 *
 * It sits there rather than among the facts below because it is what the entry *is* — the one fact
 * that classifies the store, and the one that differs between two versions of the same image. Keeping
 * it out of the facts row also stops that row wrapping to two crowded lines.
 *
 * The NGFF version is bracketed and dropped when unknown, which is not hypothetical: stores our own v2
 * writers produced before `write_multiscales_attrs` carry no version at all. Absent means absent — it
 * is not inferred from the zarr format, since 0.4 and 0.5 are pinned to v2/v3 only for what we write.
 *
 * `''` when the store could not be read, so the caller can say so once rather than twice.
 */
export function storeFormatTitle(s: StoreEncoding | null | undefined): string {
  if (!s || s.zarrFormat === undefined) return s?.ngffVersion ? `NGFF ${s.ngffVersion}` : ''
  return s.ngffVersion ? `zarr v${s.zarrFormat} (NGFF ${s.ngffVersion})` : `zarr v${s.zarrFormat}`
}

/**
 * What is left once the format is in the title: codec, chunk, shard, chunk-key style — as labelled
 * pairs rather than one `·`-joined sentence.
 *
 * The sentence form fit on one line but read as a run-on of unlabelled values (`zarr v2 · NGFF 0.4 ·
 * chunks 1×1×1×1024×1024 · not sharded · nested keys`), so telling a chunk from a shard meant parsing
 * prose. Naming each value costs one word and makes the row scannable down the page instead.
 *
 * Sharding is reported as `shard none` rather than omitted when absent. Omitting it would make an
 * unsharded store and a store whose shape we failed to read look identical, and those are different
 * answers — one is a fact about the file, the other is missing information.
 *
 * Returns `[]` when nothing is known, so the caller can skip the row entirely rather than render dashes.
 */
export function storeFormatFacts(s: StoreEncoding | null | undefined): StoreFact[] {
  if (!s || (s.zarrFormat === undefined && !s.chunks && !s.label)) return []
  const facts: StoreFact[] = []
  // Codec first: it is the one fact here a user chooses (Settings → Storage), the rest describe what
  // the store turned out to be. It arrives pre-described from the API — the two spellings of blosc's
  // shuffle live in `_describe_compressor`, not in a second place.
  if (s.label) facts.push({ k: 'codec', v: s.label })
  if (s.chunks && s.chunks.length) facts.push({ k: 'chunks', v: formatShape(s.chunks) })
  facts.push({ k: 'shard', v: s.shard && s.shard.length ? formatShape(s.shard) : 'none' })
  // Worth stating because it decides how many DIRECTORIES the store costs — most of its filesystem
  // footprint, and all of its cost on a network share (measured: 20 933 nested vs 4 flat on one 1.7 GB
  // import). Named rather than shown as a bare `/`, which reads as a path fragment.
  if (s.separator) facts.push({ k: 'keys', v: s.separator === '/' ? 'nested' : 'flat' })
  return facts
}
