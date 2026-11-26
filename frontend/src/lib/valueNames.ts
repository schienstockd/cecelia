// Reusable value_name (segmentation) presence check across images. Thin wrapper over
// POST /api/images/value-name-check → partitions image uids into those that carry the value_name and
// those that don't. Use wherever a feature must skip images lacking a value_name (e.g. copy gating
// across images). Fails open (all available) so the caller's server-side guard stays the source of truth.
// `popType` (flow/track) also asks which available images already have that gating (`gated`) — what a
// gating copy would replace. `null` ⇒ unknown (no `popType`, or the check failed): never read as "none".
import { postJson, tryApi } from '../utils/api'

export interface ValueNameCheck { available: string[]; missing: string[]; gated: string[] | null }

export async function imagesWithValueName(
  projectUid: string, valueName: string, imageUids: string[], popType?: string,
): Promise<ValueNameCheck> {
  if (!projectUid || !imageUids.length) return { available: [], missing: [], gated: null }
  const r = await tryApi(postJson<Partial<ValueNameCheck> | null>('/api/images/value-name-check',
    { projectUid, valueName, imageUids, ...(popType ? { popType } : {}) }))
  if (!r.ok) return { available: imageUids, missing: [], gated: null }
  const d = r.data ?? {}
  return { available: d.available ?? [], missing: d.missing ?? [], gated: d.gated ?? null }
}
