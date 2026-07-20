// Storage box (Settings) — REST wrappers for the on-demand storage scan + reclaim, plus the pure
// byte formatter. Kept out of the .vue SFC so the formatting logic is unit-tested (docs/DEV.md →
// Tests: pure logic in utils/*). Backend: api/src/storage_api.jl.

export interface ReclaimableImage {
  imageUid: string
  name: string
  setUid: string
  bytes: number
  activeVersion: string
}

export interface StorageSummary {
  diskTotal: number
  diskAvailable: number
  imageBytes: number   // image OME-ZARR stores only — not labels/labelProps/other task-dir data
  reclaimableBytes: number
  reclaimable: ReclaimableImage[]
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
