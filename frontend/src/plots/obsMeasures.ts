// Dynamic MEASURE discovery over obs columns — how a plot offers measures whose names are not knowable
// when the spec is written.
//
// `dataSource.measureOptions` is a static list, and `measuresFromData` discovers from the **var**
// columns (morphology, for segmentation QC). Neither can express the spatial readouts, which are **obs**
// columns whose names embed the RUN:
//     flow.cell.contact#flow.T_qc          ← target population(s)
//     flow.cell.min_distance#flow.T_qc     ← µm to the nearest target
//     flow.cell.is.aggregate               ← per pop type
//     spatial.comp.B_qc__tracked.immune    ← basis population + region-run suffix
// So the plot must ask the DATA what it has. `SummaryPanel` already loads the selected image's obs
// columns for exactly this kind of narrowing; this adds the pattern-matched discovery on top.
//
// EXCLUSIONS MATTER AS MUCH AS MATCHES. `contact_id#…` and `aggregate.id` sit right beside the real
// measures and share their prefixes, but they are LABEL IDENTIFIERS — the nearest cell's label, the
// aggregate's number. Averaging them is meaningless, and offering them invites a plot of nonsense, so
// they are filtered out rather than left to the user to avoid.

/** A measure family a spec can discover from obs. `match` is a plain substring (not a regex — spec
 *  JSON should not carry regex syntax); `label` renames the column for display. */
export interface ObsMeasurePattern {
  match: string
  label?: string
}

/** The four column lists `GET /api/gating/channels` returns for ONE (image, segmentation). */
export interface ColumnSets {
  columns: string[]           // var: morphology + intensity
  channels: string[]          // var: the intensity subset
  obsColumns: string[]        // obs: live.cell.*, cluster ids, the spatial readouts
  temporalColumns: string[]   // obsm temporal col(s) — groupable, not in obs
}

/**
 * The distinct segmentations a set of plot targets spans, in first-ticked order.
 *
 * Columns must be discovered across ALL of them, not just the first. The spatial readouts are named
 * for their TARGET (`…min_distance#live.T_qc_tracked` lives on B's h5ad, `…#live.B_qc_tracked` on T's),
 * so reading one segmentation made the measure list depend on WHICH POPULATION WAS TICKED FIRST:
 * tick B then T and "distance to B" was missing; tick T then B and "distance to T" was.
 */
export function distinctValueNames(targets: { valueName: string }[]): string[] {
  const out: string[] = []
  for (const t of targets) if (t.valueName && !out.includes(t.valueName)) out.push(t.valueName)
  return out
}

/** Union the per-segmentation column lists, de-duplicated, preserving first-seen order. */
export function mergeColumnSets(parts: Partial<ColumnSets>[]): ColumnSets {
  const key = (k: keyof ColumnSets) => [...new Set(parts.flatMap(p => p[k] ?? []))]
  return {
    columns: key('columns'), channels: key('channels'),
    obsColumns: key('obsColumns'), temporalColumns: key('temporalColumns'),
  }
}

// Columns that LOOK like a measure by prefix but identify something. Checked after the include match.
const ID_LIKE = /(?:_id#|\.id$|\.id#)/

export function isIdLikeColumn(col: string): boolean {
  return ID_LIKE.test(col)
}

/**
 * The obs columns a spec's patterns discover, in the order the patterns are declared (so a spec
 * controls which family a user sees first), alphabetical within a family. Never returns an id-like
 * column, and never the same column twice even if two patterns match it.
 */
export function discoverObsMeasures(obsCols: string[], patterns: ObsMeasurePattern[]): string[] {
  const out: string[] = []
  const seen = new Set<string>()
  for (const p of patterns) {
    const hits = obsCols
      .filter(c => c.includes(p.match) && !isIdLikeColumn(c) && !seen.has(c))
      .sort((a, b) => a.localeCompare(b))
    for (const h of hits) { seen.add(h); out.push(h) }
  }
  return out
}

/**
 * Display label for a discovered column: strip the pattern's own prefix so the picker shows the part
 * that varies (the target population, the region basis) rather than repeating the family on every row.
 *   flow.cell.min_distance#flow.T_qc   → "distance to flow.T_qc"
 *   spatial.comp.B_qc__tracked.immune  → "composition B_qc__tracked.immune"
 * Falls back to the raw column when no pattern claims it, so a label is never lost.
 */
export function obsMeasureLabel(col: string, patterns: ObsMeasurePattern[]): string {
  for (const p of patterns) {
    const i = col.indexOf(p.match)
    if (i < 0) continue
    const rest = col.slice(i + p.match.length).replace(/^[#.]/, '')
    if (!p.label) return col
    return rest ? `${p.label} ${rest}` : p.label
  }
  return col
}
