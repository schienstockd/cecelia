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
  /** Per-level shape + chunk shape from `multiscales.datasets`, level 0 first. Absent when the
   *  store carries no multiscales metadata (unreadable, or a bare zarr). A single-row array is a
   *  meaningful answer, not a bug: `create_multiscales(nscales=1)` writes exactly one level, which
   *  is what an AF-corrected store looks like on disk. */
  levels?: Array<{ path: string, shape: number[], chunks: number[] }> | null
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

/** One row of the pyramid table: level label, XY-only shape and chunk shape (T/C/Z are already in
 *  the modal's Dimensions section and do not vary across levels), plus the XY chunk grid so a
 *  level whose chunks collapsed to one tile reads as `grid 1×1` instead of "just small". */
export interface StoreLevelRow { level: string, xy: string, chunk: string, grid: string }

/** XY dims from a shape/chunk whose axes are `[t, c, z, y, x]` (the ONE order every cecelia writer
 *  emits — bf2raw's series and our own `create_multiscales` both). `-1` when an axis is missing,
 *  which the row renders as `—`. */
function xyPair(dims: number[] | null | undefined): [number, number] {
  if (!dims || dims.length < 2) return [-1, -1]
  return [dims[dims.length - 2], dims[dims.length - 1]]
}

function fmtXY(pair: [number, number]): string {
  const [y, x] = pair
  return y < 0 || x < 0 ? '—' : `${y}×${x}`
}

/**
 * Compact per-level table for the pyramid: XY shape, XY chunk, and the tile grid at that level
 * (`ceil(shape / chunk)`). The grid column is the point of this whole readout — a level whose
 * chunks were "capped to the frame" (bf2raw's behaviour when the level shrinks below one chunk)
 * shows up as `1×1` instead of the reader having to eyeball the numbers.
 *
 * Returns `[]` when no levels were reported, so the caller collapses the section entirely rather
 * than rendering an empty table.
 */
export function storeLevelRows(s: StoreEncoding | null | undefined): StoreLevelRow[] {
  if (!s?.levels?.length) return []
  return s.levels.map((lvl, i) => {
    const [sy, sx] = xyPair(lvl.shape)
    const [cy, cx] = xyPair(lvl.chunks)
    // Grid at this level: how many tiles cover the frame. `ceil(shape/chunk)`. A missing axis
    // (shape or chunk unreadable) reports `—` rather than a silently-1 that would say "this level
    // is one tile" whether it is or isn't.
    const gy = sy < 0 || cy <= 0 ? -1 : Math.ceil(sy / cy)
    const gx = sx < 0 || cx <= 0 ? -1 : Math.ceil(sx / cx)
    return {
      level: `L${i}`,
      xy:    fmtXY([sy, sx]),
      chunk: fmtXY([cy, cx]),
      grid:  fmtXY([gy, gx]),
    }
  })
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
