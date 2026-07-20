// Grouped population picker (SPATIAL_REGIONS_PLAN.md Decision 14). The backend `/api/plots/populations`
// tags every population with `granularity` ("cell" | "track") and `category` ("gated" | "clustered" |
// "region" | "tracked" | "aggregated") — derived once, server-side, from (popType, path). This helper
// turns the per-segmentation groups into ordered, labelled chip groups (*"Cells · Gated"*, …) so the
// user sees WHAT they are choosing (cells vs tracks; gated vs clustered vs region) instead of one flat
// undifferentiated strip. Pure/presentational — selection stays keyed on the `value` string.

export interface PopOption { label: string; value: string; colour?: string }
export interface PopGroupDef { title: string; opts: PopOption[] }

export interface RawPop {
  path: string; name: string; colour?: string
  popType?: string; granularity?: string; category?: string
}
export interface RawGroup { valueName: string; populations: RawPop[] }

const GRAN_LABEL: Record<string, string> = { cell: 'Cells', track: 'Tracks' }
const CAT_LABEL: Record<string, string> = {
  gated: 'Gated', clustered: 'Clustered', region: 'Regions', tracked: 'Tracked', aggregated: 'Aggregated',
}
// header order: cells before tracks; within each, gated → clustered → region → tracked → aggregated
const ORDER = [
  'cell:gated', 'cell:clustered', 'cell:region', 'cell:aggregated',
  'track:gated', 'track:tracked', 'track:clustered', 'track:aggregated',
]

function optOf(g: RawGroup, p: RawPop): PopOption {
  const value = `${g.valueName}${p.path}`              // "A" + "/_tracked" → "A/_tracked"
  const label = p.path === '/' ? `${g.valueName} · all` : value   // backend all-cells root
  return { label, value, colour: p.colour }
}

/**
 * Group tagged populations under "<granularity> · <category>" headers, in a stable order.
 * Unknown/untagged pops default to cell·gated; any unexpected combination is appended after the
 * known ones (never silently dropped). Empty groups are omitted.
 */
export function groupPopulations(groups: RawGroup[]): PopGroupDef[] {
  const byKey = new Map<string, PopOption[]>()
  for (const g of groups) {
    for (const p of g.populations) {
      const key = `${p.granularity ?? 'cell'}:${p.category ?? 'gated'}`
      if (!byKey.has(key)) byKey.set(key, [])
      byKey.get(key)!.push(optOf(g, p))
    }
  }
  const out: PopGroupDef[] = []
  const seen = new Set<string>()
  const emit = (key: string) => {
    const opts = byKey.get(key)
    if (!opts || opts.length === 0) return
    const [gran, cat] = key.split(':')
    out.push({ title: `${GRAN_LABEL[gran] ?? gran} · ${CAT_LABEL[cat] ?? cat}`, opts })
    seen.add(key)
  }
  for (const k of ORDER) emit(k)
  for (const k of byKey.keys()) if (!seen.has(k)) emit(k)   // any unforeseen combo, appended
  return out
}
