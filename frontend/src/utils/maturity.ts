// Feature maturity as data — pure, so the checks run here and `MaturityMap.vue` / `GuideMap.vue` only
// render it (docs/todo/FEATURE_MATURITY_PLAN.md). Joins three sources and restates none: the guides'
// records (`lib/guides/maturityRecords.ts`), the unguided list (`lib/guides/unguided.ts`), and the
// Coming soon flags that already exist (task JSON `comingSoon`, nav `soon`).
//
// A feature is keyed by what it covers: sidebar routes ('/gate') and task fun_names ('gate.manual_gate').
// A guide covers its map page (`mapData.ts` → guide), the tasks it runs (`awaitTask.fun`) and its
// record's `covers`; an unguided feature covers its record's `covers`. A task that is a step of a
// covered composite is covered through it (the composite's tier is what a user runs).

import { NAV_GROUPS } from '../lib/navGroups'
import { MAP } from '../lib/guides/mapData'
import { GUIDES, GROUP_ORDER } from '../lib/guides'
import { funsOf } from '../lib/guides/guideText'
import { UNGUIDED } from '../lib/guides/unguided'
import type { GuideDef, Maturity, Tier } from '../lib/guides/types'

/** The slice of a task spec these checks read — the JSON on disk and `TaskDef` both fit. */
export interface TaskSpecLite {
  fun_name: string
  label?: string
  comingSoon?: boolean
  comingSoonNote?: string
  composite?: string[]
}

export interface MaturityOwner {
  kind: 'guide' | 'unguided'
  id: string
  title: string
  group: string
  maturity: Maturity
}

const guideOwner = (g: GuideDef): MaturityOwner =>
  ({ kind: 'guide', id: g.id, title: g.title, group: g.group, maturity: g.maturity })

export function owners(): MaturityOwner[] {
  return [
    ...GUIDES.map(guideOwner),
    ...UNGUIDED.map(u => ({ kind: 'unguided' as const, id: u.id, title: u.title, group: u.group, maturity: u.maturity })),
  ]
}

/** The routes and fun_names one owner covers. */
export function coversOf(o: MaturityOwner): string[] {
  const keys = [...(o.maturity.covers ?? [])]
  if (o.kind === 'guide') {
    const g = GUIDES.find(x => x.id === o.id)!
    keys.push(...funsOf(g))
    keys.push(...Object.entries(MAP).filter(([, e]) => e.guide === o.id).map(([r]) => r))
  }
  return [...new Set(keys)]
}

/** key → every owner claiming it. More than one is a test failure (one tier per feature). */
export function coverIndex(): Map<string, MaturityOwner[]> {
  const idx = new Map<string, MaturityOwner[]>()
  for (const o of owners()) for (const k of coversOf(o)) idx.set(k, [...(idx.get(k) ?? []), o])
  return idx
}

/** Sidebar routes flagged `soon` — Coming soon, derived. */
export const soonRoutes = (): string[] => NAV_GROUPS.flatMap(g => g.items.filter(i => i.soon).map(i => i.to))

/** Sidebar routes and tasks with no tier from anywhere: not covered, not flagged Coming soon, and (for a
 *  task) not a step of a covered composite. Must be empty. */
export function uncovered(routes: string[], tasks: TaskSpecLite[]): string[] {
  const idx = coverIndex()
  const soon = new Set(soonRoutes())
  const byFun = new Map(tasks.map(t => [t.fun_name, t]))
  const covered = (fun: string, seen = new Set<string>()): boolean => {
    if (idx.has(fun) || byFun.get(fun)?.comingSoon) return true
    if (seen.has(fun)) return false
    seen.add(fun)
    return tasks.some(p => p.composite?.includes(fun) && covered(p.fun_name, seen))
  }
  return [
    ...routes.filter(r => !idx.has(r) && !soon.has(r)),
    ...tasks.filter(t => !covered(t.fun_name)).map(t => t.fun_name),
  ]
}

/** Keys flagged Coming soon AND given a tier by a record — two sources for one fact. Must be empty. */
export function flaggedAndTiered(tasks: TaskSpecLite[]): string[] {
  const idx = coverIndex()
  const flagged = [...soonRoutes(), ...tasks.filter(t => t.comingSoon).map(t => t.fun_name)]
  return flagged.filter(k => idx.has(k)).map(k => `${k} ← ${idx.get(k)!.map(o => o.id).join(', ')}`)
}

/** A sidebar page's tier: Coming soon from its flag, else its map guide's, else the unguided record
 *  covering it. Null when nothing tiers it (the test keeps that from shipping). */
export function pageTier(route: string): Tier | null {
  if (soonRoutes().includes(route)) return 'comingSoon'
  return coverIndex().get(route)?.[0]?.maturity.tier ?? null
}

/** A tier the user sees a badge for — every tier but exempt (Decision 9). */
export const badged = (t: Tier | null | undefined): t is Exclude<Tier, 'exempt'> => !!t && t !== 'exempt'

// ── The map's maturity rows ────────────────────────────────────────────────────────────────────────

export interface MaturityRow {
  id: string
  title: string
  guided: boolean
  tier: Tier
  maturity: Maturity | null     // null for a Coming soon row derived from a flag
  note?: string                 // a flagged task's comingSoonNote
}

export interface MaturityGroup { heading: string; rows: MaturityRow[] }

/** Split groups into `n` fixed columns in order, filling each to about 1/n of the rows. Fixed, so expanding
 *  a row only pushes the rows beneath it — CSS `columns` rebalanced and moved rows between columns. */
export function splitColumns<T extends { rows: unknown[] }>(groups: T[], n = 2): T[][] {
  const total = groups.reduce((k, g) => k + g.rows.length + 1, 0)   // +1: the group heading
  const cols: T[][] = Array.from({ length: n }, () => [])
  let c = 0, used = 0
  for (const g of groups) {
    const size = g.rows.length + 1
    if (c < n - 1 && used > 0 && used + size / 2 > (total / n) * (c + 1)) c++
    cols[c].push(g); used += size
  }
  return cols
}

/** Every non-exempt feature, grouped like the guide picker: guides first, then "no guide yet", then
 *  a trailing Coming soon group derived from the flags (no record carries that tier). */
export function maturityGroups(tasks: TaskSpecLite[]): MaturityGroup[] {
  const all = owners().filter(o => badged(o.maturity.tier))
  const out: MaturityGroup[] = []
  for (const heading of GROUP_ORDER) {
    const rows = all.filter(o => o.group === heading)
      .sort((a, b) => Number(a.kind === 'unguided') - Number(b.kind === 'unguided'))
      .map(o => ({ id: `${o.kind}:${o.id}`, title: o.title, guided: o.kind === 'guide', tier: o.maturity.tier, maturity: o.maturity }))
    if (rows.length) out.push({ heading, rows })
  }
  const soon: MaturityRow[] = [
    ...tasks.filter(t => t.comingSoon).map(t => ({
      id: `task:${t.fun_name}`, title: t.label ?? t.fun_name, guided: false, tier: 'comingSoon' as const,
      maturity: null, note: t.comingSoonNote,
    })),
    ...NAV_GROUPS.flatMap(g => g.items.filter(i => i.soon)).map(i => ({
      id: `page:${i.to}`, title: i.label, guided: false, tier: 'comingSoon' as const, maturity: null,
    })),
  ]
  if (soon.length) out.push({ heading: 'Coming soon', rows: soon })
  return out
}

// ── Evidence: does the named test exist, and is it switched on? ────────────────────────────────────

export interface TestLookup { found: boolean; skipped: string | null }

const esc = (s: string) => s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')

/** Find a test by name in a test file's source and say whether it is marked skipped or broken.
 *  Julia: `@testset "name"` (its block must hold no `@test_skip`/`@test_broken`). Python: `def name(`
 *  (no unconditional `skip`/`xfail` decorator on it or its class, no module-level skip mark — a
 *  `skipif` on a missing fixture is fine). TypeScript: `it|test|describe('name'` (not `.skip`/`.todo`). */
export function findTest(path: string, source: string, name: string): TestLookup {
  const lines = source.split('\n')
  if (path.endsWith('.jl')) {
    const i = lines.findIndex(l => new RegExp(`@testset\\s+"${esc(name)}"`).test(l))
    if (i < 0) return { found: false, skipped: null }
    const indent = lines[i].match(/^\s*/)![0].length
    const end = lines.findIndex((l, j) => j > i && /^\s*end\b/.test(l) && l.match(/^\s*/)![0].length === indent)
    const block = lines.slice(i, end < 0 ? undefined : end).join('\n')
    const m = block.match(/@test_skip|@test_broken/)
    return { found: true, skipped: m ? m[0] : null }
  }
  if (path.endsWith('.py')) {
    const i = lines.findIndex(l => new RegExp(`^\\s*(async\\s+)?def\\s+${esc(name)}\\(`).test(l))
    if (i < 0) return { found: false, skipped: null }
    const SKIP = /@(pytest\.mark\.(skip|xfail)\b(?!if)|unittest\.(skip|expectedFailure)\b(?!If|Unless))/
    const decoratorsAbove = (at: number) => {
      const out: string[] = []
      for (let j = at - 1; j >= 0 && /^\s*@/.test(lines[j]); j--) out.push(lines[j])
      return out
    }
    const indent = lines[i].match(/^\s*/)![0].length
    let cls = -1
    for (let j = i - 1; j >= 0 && indent > 0; j--) {
      if (/^class\s/.test(lines[j])) { cls = j; break }
    }
    const marks = [...decoratorsAbove(i), ...(cls >= 0 ? decoratorsAbove(cls) : [])]
    const hit = marks.find(l => SKIP.test(l)) ?? lines.find(l => /^pytestmark\s*=.*pytest\.mark\.(skip|xfail)\b(?!if)/.test(l))
    return { found: true, skipped: hit ? hit.trim() : null }
  }
  const call = new RegExp(`\\b(it|test|describe)(\\.(skip|todo|fails|only))?\\(\\s*['"\`]${esc(name)}['"\`]`)
  const m = source.match(call)
  if (!m) return { found: false, skipped: null }
  return { found: true, skipped: m[3] === 'skip' || m[3] === 'todo' || m[3] === 'fails' ? `${m[1]}.${m[3]}` : null }
}
