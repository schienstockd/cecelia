// Storage box (Settings) — REST wrappers for the on-demand storage scan + reclaim, plus the pure
// byte formatter. Kept out of the .vue SFC so the formatting logic is unit-tested (docs/DEV.md →
// Tests: pure logic in utils/*). Backend: api/src/storage_api.jl.

export interface ReclaimableImage {
  imageUid: string
  name: string
  setUid: string
  bytes: number             // total of the non-active versions that would be freed
  activeVersion: string     // the version kept
  versions?: { valueName: string; bytes: number }[]   // the non-active versions being removed
}

export interface StorageSummary {
  diskTotal: number
  diskAvailable: number
  imageBytes: number   // image OME-ZARR stores only — not labels/labelProps/other task-dir data
  reclaimableBytes: number
  reclaimable: ReclaimableImage[]
  /** Bytes nothing can reach: leftovers a cancelled/crashed run abandoned (staging dirs, import
   *  scratch, unregistered or truncated stores). DISTINCT from `reclaimable`, which is real data the
   *  user could choose to drop. Freed by the `store-debris` data patch, which uses the same detector
   *  (`store_sweep.summarise`), so the number here is what that patch would actually free. */
  debris?: StorageDebris
}

export interface StorageDebris {
  count: number
  bytes: number
  /** items a sweep would report but NOT delete — a store touched recently enough to look in-flight */
  activeSkipped: number
  byWhy: Record<string, number>
}

/** One short line for the storage box, or '' when there is nothing to say. The box should ANNOUNCE
 *  leftover bytes rather than wait to be discovered in Data patches — but say nothing when clean. */
export function debrisLine(d: StorageDebris | null | undefined): string {
  if (!d || d.count <= 0) return ''
  const what = d.count === 1 ? '1 leftover item' : `${d.count} leftover items`
  return `${what} · ${formatBytes(d.bytes)}`
}

/** Human-readable size. Binary units (1024); one decimal below 100, integer above. */
export function formatBytes(n: number): string {
  if (!Number.isFinite(n) || n <= 0) return '0 B'
  const units = ['B', 'KB', 'MB', 'GB', 'TB', 'PB']
  const i = Math.min(Math.floor(Math.log(n) / Math.log(1024)), units.length - 1)
  const v = n / 1024 ** i
  const s = i === 0 ? Math.round(v).toString()
          : v >= 100 ? Math.round(v).toString()
          : v.toFixed(1).replace(/\.0$/, '')
  return `${s} ${units[i]}`
}

/** Compression the image OME-ZARRs we write use. Advanced, and rendered as a COMPARISON TABLE rather
 *  than a dropdown: the trade-off is the only reason there is a choice, so the numbers have to be on
 *  screen at the point of deciding. Every field below is a display string measured and formatted by
 *  the BACKEND (app/src/config.jl → IMAGE_COMPRESSOR_CHOICES) — the frontend never computes or
 *  restates a number, so there is one place the measurements live. */
export interface CompressorChoice {
  name: string
  label: string
  size: string        // resulting store size, e.g. "0.64 GB"
  ratio: string       // how much smaller than raw, e.g. "2.74x" — SIZE, not speed
  write: string       // whole-store write time, e.g. "5.1 s"
  read: string        // warm per-plane read, e.g. "1.7 ms"
  url: string         // the codec's own site
}

export interface CompressorSettings {
  current: string
  default: string
  measuredOn: string  // one short line naming what every row was measured on
  docsUrl: string     // the shuffle filter is Blosc's, not the codec's
  choices: CompressorChoice[]
}

export async function fetchCompressor(): Promise<CompressorSettings> {
  const res = await fetch('/api/storage/compressor')
  const data = await res.json().catch(() => ({}))
  if (!res.ok) throw new Error((data as any)?.error ?? `HTTP ${res.status}`)
  return data as CompressorSettings
}

/** Applies to stores written from here on — existing ones are untouched. */
export async function setCompressor(name: string): Promise<string> {
  const res = await fetch('/api/storage/compressor/set', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ name }),
  })
  const data = await res.json().catch(() => ({}))
  if (!res.ok) throw new Error((data as any)?.error ?? `HTTP ${res.status}`)
  return (data as any).current as string
}

export async function fetchStorageSummary(projectUid: string): Promise<StorageSummary> {
  const res = await fetch(`/api/storage/summary?projectUid=${encodeURIComponent(projectUid)}`)
  const data = await res.json().catch(() => ({}))
  if (!res.ok) throw new Error((data as any)?.error ?? `HTTP ${res.status}`)
  return data as StorageSummary
}

export async function reclaimStorage(
  projectUid: string, imageUids: string[],
): Promise<{ freedBytes: number; reclaimed: string[] }> {
  const res = await fetch('/api/storage/reclaim', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ projectUid, imageUids }),
  })
  const data = await res.json().catch(() => ({}))
  if (!res.ok) throw new Error((data as any)?.error ?? `HTTP ${res.status}`)
  return data as { freedBytes: number; reclaimed: string[] }
}
