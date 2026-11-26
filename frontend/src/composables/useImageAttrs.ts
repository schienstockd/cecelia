// Image-attribute discovery — the ONE fetch of GET /api/plots/attrs, shared by every surface that
// groups/colours/facets by a per-image attribute (summary "compare by attribute" AND the cluster UMAP
// colour/facet-by-attribute). Kept here so the two don't hand-roll the same request twice
// (feedback_use_existing_framework). Returns the attribute names present across the selected images
// plus their distinct values: [{ name, values }].
import { getJson, tryApi } from '../utils/api'

export interface ImageAttr { name: string; values: string[] }

export async function fetchImageAttrs(
  projectUid: string, setUid: string | null, imageUids: string[],
): Promise<ImageAttr[]> {
  if (!projectUid || !setUid) return []
  const r = await tryApi(getJson<{ attrs?: ImageAttr[] }>('/api/plots/attrs', {
    query: { projectUid, setUid, imageUids: imageUids.length ? imageUids.join(',') : undefined },
  }))
  return r.ok ? (r.data?.attrs ?? []) : []
}
