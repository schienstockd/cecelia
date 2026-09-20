// VN versioning — fetch the union of `vN` keys across a set of images for a given (field,
// value_name). Feeds the chain-designer's per-node "Input version" picker (P3) and, later, the
// general vn picker's version chip (P6, docs/todo/VN_VERSIONING_PLAN.md). Kept as a pure fetch
// helper (out of the .vue SFC) so the wiring is testable and reused across pickers.
//
// Backend: `GET /api/versions` in `api/src/routes.jl` (`api_versions_list`). See there for the
// rationale — a chain runs against N images that may carry different vN sets per value_name; the
// UNION is what the picker offers, and per-image resolution happens at run time in the reader
// (`versioned_get_field_at`).

export interface VersionsResult {
  versions: string[]   // e.g. ["v1", "v2", "v3"], numeric-sorted
}

export async function fetchVersions(params: {
  projectUid: string
  imageUids: string[]
  valueName?: string   // '' or omitted ⇒ the active value_name
  field?: string       // defaults to "filepath"
}): Promise<VersionsResult> {
  const q = new URLSearchParams()
  q.set('projectUid', params.projectUid)
  q.set('imageUids', params.imageUids.join(','))
  if (params.valueName) q.set('valueName', params.valueName)
  if (params.field) q.set('field', params.field)
  const res = await fetch(`/api/versions?${q.toString()}`)
  const data = await res.json().catch(() => ({}))
  if (!res.ok) throw new Error((data as any)?.error ?? `HTTP ${res.status}`)
  return { versions: Array.isArray((data as any)?.versions) ? (data as any).versions : [] }
}
