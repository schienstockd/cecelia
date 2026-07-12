// Minimal, dependency-free ZIP builder (STORE method — no compression). Enough to bundle a handful
// of text files (e.g. the analysis board's per-plot CSVs) into ONE download instead of dozens of
// separate "allow multiple downloads" prompts. Store (not deflate) keeps this tiny and dependency-
// free; CSVs zip is a convenience, not a size optimisation. Format: PKZIP APPNOTE 6.3.2 — local file
// headers + central directory + end-of-central-directory. Time/date are stamped 0 (we don't thread a
// clock through the UI, and archive timestamps don't matter here).

// CRC-32 (IEEE 802.3, the polynomial ZIP requires). Lazily-built lookup table.
let CRC_TABLE: Uint32Array | null = null
function crcTable(): Uint32Array {
  if (CRC_TABLE) return CRC_TABLE
  const t = new Uint32Array(256)
  for (let n = 0; n < 256; n++) {
    let c = n
    for (let k = 0; k < 8; k++) c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1
    t[n] = c >>> 0
  }
  CRC_TABLE = t
  return t
}
export function crc32(bytes: Uint8Array): number {
  const t = crcTable()
  let c = 0xffffffff
  for (let i = 0; i < bytes.length; i++) c = t[(c ^ bytes[i]) & 0xff] ^ (c >>> 8)
  return (c ^ 0xffffffff) >>> 0
}

interface ZipEntry { name: string; data: Uint8Array; crc: number; offset: number }

// Build a ZIP Blob from named text files. Duplicate names are disambiguated (`x.csv` → `x (2).csv`).
export function zipTextFiles(files: { name: string; text: string }[]): Blob {
  const enc = new TextEncoder()
  const seen = new Map<string, number>()
  const parts: Uint8Array[] = []
  const entries: ZipEntry[] = []
  let offset = 0

  for (const f of files) {
    let name = f.name
    const dupe = seen.get(name)
    if (dupe !== undefined) { const next = dupe + 1; seen.set(name, next); name = uniquify(name, next) }
    else seen.set(name, 1)
    const nameBytes = enc.encode(name)
    const data = enc.encode(f.text)
    const crc = crc32(data)
    const header = localHeader(nameBytes, data.length, crc)
    entries.push({ name, data, crc, offset })
    parts.push(header, nameBytes, data)
    offset += header.length + nameBytes.length + data.length
  }

  const central: Uint8Array[] = []
  let cdSize = 0
  for (const e of entries) {
    const nameBytes = enc.encode(e.name)
    const rec = centralHeader(nameBytes, e.data.length, e.crc, e.offset)
    central.push(rec, nameBytes)
    cdSize += rec.length + nameBytes.length
  }
  const eocd = endOfCentralDir(entries.length, cdSize, offset)
  // cast: TS's BlobPart wants ArrayBufferView<ArrayBuffer>, but our Uint8Arrays infer as
  // Uint8Array<ArrayBufferLike>; they're plain ArrayBuffer-backed, so this is safe.
  return new Blob([...parts, ...central, eocd] as BlobPart[], { type: 'application/zip' })
}

// `x.csv` (n=2) → `x (2).csv`; extensionless → `x (2)`
function uniquify(name: string, n: number): string {
  const dot = name.lastIndexOf('.')
  return dot > 0 ? `${name.slice(0, dot)} (${n})${name.slice(dot)}` : `${name} (${n})`
}

function localHeader(nameBytes: Uint8Array, size: number, crc: number): Uint8Array {
  const b = new Uint8Array(30); const v = new DataView(b.buffer)
  v.setUint32(0, 0x04034b50, true)  // local file header signature
  v.setUint16(4, 20, true)          // version needed
  v.setUint16(6, 0, true)           // flags
  v.setUint16(8, 0, true)           // method: store
  v.setUint16(10, 0, true)          // mod time
  v.setUint16(12, 0, true)          // mod date
  v.setUint32(14, crc, true)
  v.setUint32(18, size, true)       // compressed size (== uncompressed for store)
  v.setUint32(22, size, true)       // uncompressed size
  v.setUint16(26, nameBytes.length, true)
  v.setUint16(28, 0, true)          // extra length
  return b
}

function centralHeader(nameBytes: Uint8Array, size: number, crc: number, offset: number): Uint8Array {
  const b = new Uint8Array(46); const v = new DataView(b.buffer)
  v.setUint32(0, 0x02014b50, true)  // central dir header signature
  v.setUint16(4, 20, true)          // version made by
  v.setUint16(6, 20, true)          // version needed
  v.setUint16(8, 0, true)           // flags
  v.setUint16(10, 0, true)          // method: store
  v.setUint16(12, 0, true)          // mod time
  v.setUint16(14, 0, true)          // mod date
  v.setUint32(16, crc, true)
  v.setUint32(20, size, true)
  v.setUint32(24, size, true)
  v.setUint16(28, nameBytes.length, true)
  v.setUint16(30, 0, true)          // extra length
  v.setUint16(32, 0, true)          // comment length
  v.setUint16(34, 0, true)          // disk number start
  v.setUint16(36, 0, true)          // internal attrs
  v.setUint32(38, 0, true)          // external attrs
  v.setUint32(42, offset, true)     // local header offset
  return b
}

function endOfCentralDir(count: number, cdSize: number, cdOffset: number): Uint8Array {
  const b = new Uint8Array(22); const v = new DataView(b.buffer)
  v.setUint32(0, 0x06054b50, true)  // EOCD signature
  v.setUint16(4, 0, true)           // disk number
  v.setUint16(6, 0, true)           // cd start disk
  v.setUint16(8, count, true)       // entries this disk
  v.setUint16(10, count, true)      // entries total
  v.setUint32(12, cdSize, true)
  v.setUint32(16, cdOffset, true)
  v.setUint16(20, 0, true)          // comment length
  return b
}
