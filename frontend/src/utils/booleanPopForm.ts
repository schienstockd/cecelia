// Pure helpers for the boolean-population form (PopulationManager, Decision 16). Logic lives here
// (not the SFC) so the summary + the validity rule are unit-tested (docs/DEV.md).
//
// A boolean population's membership is a set operation over OTHER populations: the included terms
// combined with AND or OR, minus every excluded term ("nuc-GFP+ OR mem-TOM+"; "both, but NOT
// CD169+"). No included terms at all means "everything in the parent except the excluded ones" —
// the plain not-gate.

export interface BooleanSpec { op: 'and' | 'or'; pops: string[]; not: string[] }

// One term of the form: a population and whether it is included or excluded.
export interface BooleanTerm { path: string; negate: boolean }

export const termsToSpec = (op: 'and' | 'or', terms: BooleanTerm[]): BooleanSpec => ({
  op,
  pops: terms.filter(t => t.path && !t.negate).map(t => t.path),
  not: terms.filter(t => t.path && t.negate).map(t => t.path),
})

export const specToTerms = (spec: BooleanSpec | undefined): BooleanTerm[] => [
  ...(spec?.pops ?? []).map(path => ({ path, negate: false })),
  ...(spec?.not ?? []).map(path => ({ path, negate: true })),
]

// A combination needs at least one term; anything else the server would reject anyway.
export const booleanSpecValid = (spec: BooleanSpec) => spec.pops.length + spec.not.length > 0

// One-line human summary for the list badge's tooltip: "GFP+ or TOM+", "GFP+ and TOM+, not CD169+",
// "not CD169+". `label` maps a population path to its display name (usually the leaf name).
export function booleanSummary(spec: BooleanSpec | undefined, label: (p: string) => string): string {
  if (!spec) return ''
  const inc = (spec.pops ?? []).map(label).join(spec.op === 'or' ? ' or ' : ' and ')
  const exc = (spec.not ?? []).map(label).join(', ')
  if (!inc) return exc ? `not ${exc}` : ''
  return exc ? `${inc}, not ${exc}` : inc
}
