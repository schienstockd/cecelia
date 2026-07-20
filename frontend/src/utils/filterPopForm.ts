// Pure helpers for the filter-population form (PopulationManager, Decision 15). Logic lives here
// (not the SFC) so the value parsing + predicate formatting are unit-tested (docs/DEV.md).

export interface FilterCond { measure: string; fun: string; values: unknown }

// Parse the values box for a condition. `in` → comma-separated list; every other fun → a single
// value. Each token is coerced to a number when it parses cleanly, else kept as a string (so
// categorical filters like region IDs or hmm.state labels work either way).
export function parseFilterValues(fun: string, raw: string): unknown {
  const toNum = (s: string) => {
    const n = Number(s)
    return s !== '' && Number.isFinite(n) ? n : s
  }
  const parts = raw.split(',').map(s => s.trim()).filter(s => s.length)
  return fun === 'in' ? parts.map(toNum) : (parts.length ? toNum(parts[0]) : '')
}

// One-line human summary of a filter pop's predicate (for the list badge tooltip): the single
// measure/fun/values, or the AND-ed compound conditions. `label` maps a raw measure to its display name.
export function filterSummary(
  filter: { measure?: string; fun?: string; values?: unknown; conditions?: FilterCond[] } | undefined,
  label: (m: string) => string,
): string {
  if (!filter) return ''
  const conds = filter.conditions?.length
    ? filter.conditions
    : [{ measure: filter.measure ?? '', fun: filter.fun ?? '', values: filter.values }]
  const one = (c: FilterCond) =>
    `${label(c.measure)} ${c.fun} ${Array.isArray(c.values) ? c.values.join(',') : c.values}`
  return conds.filter(c => c.measure).map(one).join('  AND  ')
}
