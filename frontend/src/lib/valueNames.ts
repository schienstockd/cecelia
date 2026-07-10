// Reusable value_name (segmentation) presence check across images. Thin wrapper over
// POST /api/images/value-name-check → partitions image uids into those that carry the value_name and
// those that don't. Use wherever a feature must skip images lacking a value_name (e.g. copy gating
// across images). Fails open (all available) so the caller's server-side guard stays the source of truth.
export interface ValueNameCheck { available: string[]; missing: string[] }

export async function imagesWithValueName(
  projectUid: string, valueName: string, imageUids: string[],
): Promise<ValueNameCheck> {
  if (!projectUid || !imageUids.length) return { available: [], missing: [] }
  try {
    const res = await fetch('/api/images/value-name-check', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ projectUid, valueName, imageUids }),
    })
    if (!res.ok) throw new Error(`HTTP ${res.status}`)
    return await res.json() as ValueNameCheck
  } catch {
    return { available: imageUids, missing: [] }
  }
}
