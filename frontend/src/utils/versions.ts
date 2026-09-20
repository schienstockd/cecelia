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

export interface VersionChipOption {
  value: string   // `_latest` sentinel | `v1` | `v2` | … — the chip's internal value
  label: string   // matches `value` — shown on the chip
}

/**
 * The chip value that stands for "follow _latest" — the DEFAULT pick and the one the strip leads
 * with. Distinct from the empty string (which means "no chip selected", a state the strip never
 * enters once versions exist). Consumers translate this to `null` when writing `params.version`,
 * since the backend + chain reader treat `null` and missing identically.
 */
export const VERSION_LATEST_CHIP = '_latest'

/**
 * Chip options for the P6 version picker (`docs/todo/VN_VERSIONING_PLAN.md` → P6). Numeric-first
 * sort matches the backend's `/api/versions` response (also served by `fetchVersions`); the
 * `_latest` chip leads the strip so the default is a click away.
 *
 * Empty when a value_name has fewer than two versions on disk: 0 or 1 leaves nothing to pick, and
 * the picker hides itself rather than render a chip that only has one option. Every unfamiliar
 * label ("draft", …) tails the numeric run in stable order — same rule the API uses.
 */
export function versionChipOptions(versions: string[]): VersionChipOption[] {
  if (!versions || versions.length < 2) return []
  const parseN = (v: string): number | null =>
    v.startsWith('v') ? (Number.isInteger(Number(v.slice(1))) ? Number(v.slice(1)) : null) : null
  const numeric = versions.filter(v => parseN(v) !== null)
                          .sort((a, b) => (parseN(a) ?? 0) - (parseN(b) ?? 0))
  const other   = versions.filter(v => parseN(v) === null).sort()
  return [
    { value: VERSION_LATEST_CHIP, label: VERSION_LATEST_CHIP },
    ...numeric.map(v => ({ value: v, label: v })),
    ...other.map(v =>   ({ value: v, label: v })),
  ]
}

/** True when `pinnedVersion` (from the form's `params.version`) is `_latest`. Wraps the
 *  null-or-missing check so a caller doesn't restate the empty-string / undefined / null tri-check. */
export const isVersionLatest = (pinnedVersion: string | null | undefined): boolean =>
  !pinnedVersion || pinnedVersion === VERSION_LATEST_CHIP

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
