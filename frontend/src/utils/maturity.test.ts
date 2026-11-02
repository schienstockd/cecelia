import { describe, it, expect } from 'vitest'
import { NAV_GROUPS } from '../lib/navGroups'
import { GUIDES, GROUP_ORDER } from '../lib/guides'
import { GUIDE_MATURITY } from '../lib/guides/maturityRecords'
import { UNGUIDED } from '../lib/guides/unguided'
import { CALL_FOR_DATASETS } from '../lib/callForDatasets'
import {
  owners, coverIndex, uncovered, flaggedAndTiered, pageTier, maturityGroups, findTest, badged,
  type TaskSpecLite,
} from './maturity'

// FEATURE_MATURITY_PLAN → *Tests*: every page and task has a tier, every Tested claim's evidence is
// real and switched on, every dataset ask exists.

const ROUTES = NAV_GROUPS.flatMap(g => g.items.map(i => i.to))

// The registered tasks, read the way taskDiscovery.test.ts reads them: the JSON specs on disk (the
// registry in task_registry.jl maps 1:1 onto these). `fragments/` are param snippets, `testTasks.*`
// exist only for the test suite, and a spec with no fun_name (tracking/cell_config.json) is btrack's
// config, not a task.
const SPECS = import.meta.glob('../../../app/src/tasks/**/*.json', { eager: true, import: 'default' }) as
  Record<string, unknown>
const TASKS: TaskSpecLite[] = Object.entries(SPECS)
  .filter(([p, s]) => !p.includes('/fragments/') && s && !Array.isArray(s) && typeof (s as TaskSpecLite).fun_name === 'string')
  .map(([, s]) => s as TaskSpecLite)
  .filter(t => !t.fun_name.startsWith('testTasks.'))
const FUNS = new Set(TASKS.map(t => t.fun_name))

// Every test file evidence may cite, as text, keyed repo-relative. Vite keys this directory's own files
// './x' and the rest of src '../x' (and leaves this file out of its own glob).
const RAW = import.meta.glob([
  '../../../app/test/suite/*.jl', '../../../api/test/suite/*.jl',
  '../../../python/cecelia/tests/*.py', '../../../mcp/tests/*.py',
  '../**/*.test.ts', './*.test.ts',
], { query: '?raw', import: 'default', eager: true }) as Record<string, string>
const repoPath = (p: string) => p.startsWith('../../../') ? p.slice(9)
  : p.startsWith('./') ? `frontend/src/utils/${p.slice(2)}` : `frontend/src/${p.slice(3)}`
const SOURCES = new Map(Object.entries(RAW).map(([p, s]) => [repoPath(p), s]))

describe('every feature has a tier (Decision 5)', () => {
  it('reads the task specs and the test sources', () => {
    expect(TASKS.length).toBeGreaterThan(50)
    expect(FUNS.has('segment.cellposeMeasure')).toBe(true)
    expect(SOURCES.has('app/test/suite/gating.jl')).toBe(true)
    expect(SOURCES.has('frontend/src/utils/guideMap.test.ts')).toBe(true)
  })
  it('every guide carries its own record from maturityRecords.ts, and every record has its guide', () => {
    for (const g of GUIDES) expect(g.maturity, g.id).toBe((GUIDE_MATURITY as Record<string, unknown>)[g.id])
    expect(Object.keys(GUIDE_MATURITY).sort()).toEqual(GUIDES.map(g => g.id).sort())
  })
  it('every sidebar page and every registered task is taught by a guide or listed as unguided', () => {
    expect(uncovered(ROUTES, TASKS)).toEqual([])
  })
  it('nothing is tiered twice', () => {
    const twice = [...coverIndex()].filter(([, os]) => os.length > 1).map(([k, os]) => `${k}: ${os.map(o => o.id).join(', ')}`)
    expect(twice).toEqual([])
  })
  it('every covered key is a real sidebar page or task', () => {
    expect([...coverIndex().keys()].filter(k => !ROUTES.includes(k) && !FUNS.has(k))).toEqual([])
  })
  it('Coming soon comes from the flags only — a flagged feature has no record', () => {
    expect(flaggedAndTiered(TASKS)).toEqual([])
    expect(TASKS.some(t => t.comingSoon)).toBe(true)
  })
  it('unguided features have unique ids, a guide-picker group, and are not guides', () => {
    const ids = UNGUIDED.map(u => u.id)
    expect(new Set(ids).size).toBe(ids.length)
    for (const u of UNGUIDED) expect(GROUP_ORDER as readonly string[], u.id).toContain(u.group)
    expect(ids.filter(id => GUIDES.some(g => g.id === id))).toEqual([])
  })
  it('every sidebar page has a tier', () => {
    expect(ROUTES.filter(r => pageTier(r) === null)).toEqual([])
  })
})

describe('evidence is real', () => {
  const all = owners()
  it('every evidence path exists and names a test in it', () => {
    const bad: string[] = []
    for (const o of all) for (const e of o.maturity.evidence ?? []) {
      const src = SOURCES.get(e.path)
      if (src === undefined) bad.push(`${o.id}: no file ${e.path}`)
      else if (!findTest(e.path, src, e.test).found) bad.push(`${o.id}: no test "${e.test}" in ${e.path}`)
    }
    expect(bad).toEqual([])
  })
  // Deleting or disabling the evidence fails CI until the tier drops or the evidence is fixed.
  it('every Tested record has evidence, none of it skipped or broken', () => {
    const bad: string[] = []
    for (const o of all.filter(x => x.maturity.tier === 'tested')) {
      if (!o.maturity.evidence?.length) bad.push(`${o.id}: Tested with no evidence`)
      for (const e of o.maturity.evidence ?? []) {
        const s = findTest(e.path, SOURCES.get(e.path) ?? '', e.test).skipped
        if (s) bad.push(`${o.id}: ${e.test} is marked ${s}`)
      }
    }
    expect(bad).toEqual([])
  })
  it('every dataset ask exists in callForDatasets.ts', () => {
    const ids = new Set(CALL_FOR_DATASETS.map(a => a.id))
    expect(all.filter(o => o.maturity.datasetAsk && !ids.has(o.maturity.datasetAsk)).map(o => o.id)).toEqual([])
  })
  // A "Needs data" line with no Share data button is a need nobody can answer from the app.
  it('every need has a dataset ask, or says why it can’t have one', () => {
    expect(all.filter(o => o.maturity.needs && !o.maturity.datasetAsk && !o.maturity.noAsk?.trim()).map(o => o.id))
      .toEqual([])
    expect(all.filter(o => (o.maturity.datasetAsk || o.maturity.noAsk) && !o.maturity.needs).map(o => o.id)).toEqual([])
    expect(all.filter(o => o.maturity.datasetAsk && o.maturity.noAsk).map(o => o.id)).toEqual([])
  })
  it('stable is reserved, and an exempt record claims nothing', () => {
    expect(all.filter(o => o.maturity.tier === 'stable').map(o => o.id)).toEqual([])
    for (const o of all.filter(x => x.maturity.tier === 'exempt')) {
      expect([o.maturity.evidence, o.maturity.next, o.maturity.needs, o.maturity.datasetAsk, o.maturity.noAsk], o.id)
        .toEqual([undefined, undefined, undefined, undefined, undefined])
    }
  })
  it('keeps the map’s lines short (docs/ui/COPY.md)', () => {
    const long: string[] = []
    const check = (id: string, s: string | undefined, max: number) => {
      if (s && (s.length > max || s.endsWith('.'))) long.push(`${id}: ${s}`)
    }
    for (const o of all) {
      const m = o.maturity
      check(o.id, m.testedOn, 70)
      check(o.id, m.needs, 90)
      for (const e of m.evidence ?? []) check(o.id, e.failsIf, 110)
      for (const n of m.next ?? []) check(o.id, n.text, 110)
      expect((m.next ?? []).length, o.id).toBeLessThanOrEqual(3)
    }
    expect(long).toEqual([])
  })
})

describe('findTest', () => {
  const JL = '@testset "Gates" begin\n    @test true\nend\n@testset "Other" begin\n    @test_broken false\nend\n'
  it('finds a Julia testset and reads a skip inside its block only', () => {
    expect(findTest('a.jl', JL, 'Gates')).toEqual({ found: true, skipped: null })
    expect(findTest('a.jl', JL, 'Other')).toEqual({ found: true, skipped: '@test_broken' })
    expect(findTest('a.jl', JL, 'Missing').found).toBe(false)
    const nested = '@testset "Outer" begin\n    @testset "Inner" begin\n        @test_skip 1 == 2\n    end\nend\n'
    expect(findTest('a.jl', nested, 'Inner').skipped).toBe('@test_skip')
  })
  it('reads a Python skip or xfail on the test or its class, but not a conditional skipif', () => {
    const py = (deco: string, cls = '') =>
      `import pytest\n${cls}class T:\n    ${deco}\n    def test_x(self):\n        pass\n`
    expect(findTest('t.py', py('@pytest.mark.skipif(not HAVE, reason="fixture")'), 'test_x').skipped).toBeNull()
    expect(findTest('t.py', py('@pytest.mark.skip(reason="later")'), 'test_x').skipped).toMatch(/skip/)
    expect(findTest('t.py', py('@pytest.mark.xfail'), 'test_x').skipped).toMatch(/xfail/)
    expect(findTest('t.py', py('@unittest.skip("x")'), 'test_x').skipped).toMatch(/skip/)
    expect(findTest('t.py', py('@unittest.skipIf(True, "x")'), 'test_x').skipped).toBeNull()
    expect(findTest('t.py', py('', '@unittest.skip("all")\n'), 'test_x').skipped).toMatch(/skip/)
    expect(findTest('t.py', 'import pytest\npytestmark = pytest.mark.skip\ndef test_x():\n    pass\n', 'test_x').skipped)
      .toMatch(/pytestmark/)
    expect(findTest('t.py', py(''), 'test_y').found).toBe(false)
  })
  it('reads it.skip / it.todo in TypeScript', () => {
    expect(findTest('a.test.ts', "it('works', () => {})", 'works')).toEqual({ found: true, skipped: null })
    expect(findTest('a.test.ts', "it.skip('works', () => {})", 'works').skipped).toBe('it.skip')
    expect(findTest('a.test.ts', "describe.todo('works')", 'works').skipped).toBe('describe.todo')
  })
})

describe('the map’s maturity rows', () => {
  const groups = maturityGroups(TASKS)
  const rows = groups.flatMap(g => g.rows)
  it('shows no exempt feature (Decision 9)', () => {
    expect(rows.filter(r => !badged(r.tier))).toEqual([])
    expect(rows.some(r => r.id === 'guide:use-the-blackboard')).toBe(false)
  })
  it('lists each guide before the unguided features of its group', () => {
    for (const g of groups.filter(x => x.heading !== 'Coming soon')) {
      const firstUnguided = g.rows.findIndex(r => !r.guided)
      if (firstUnguided >= 0) expect(g.rows.slice(firstUnguided).every(r => !r.guided), g.heading).toBe(true)
    }
    expect(rows.find(r => r.id === 'unguided:phenotype')?.guided).toBe(false)
  })
  it('derives Coming soon from the task flag, with its note', () => {
    const soon = groups.find(g => g.heading === 'Coming soon')!
    expect(soon.rows.map(r => r.id)).toContain('task:editImages.register')
    expect(soon.rows.every(r => r.maturity === null && r.tier === 'comingSoon')).toBe(true)
  })
  it('gives a page its guide’s tier, an unguided page its record’s', () => {
    expect(pageTier('/gate')).toBe('beta')
    expect(pageTier('/phenotype')).toBe('experimental')
    expect(pageTier('/tasks')).toBe('exempt')
  })
})


describe('splitColumns — groups stay in a fixed column', () => {
  it('keeps order, never splits a group, and balances by rows', async () => {
    const { splitColumns } = await import('./maturity')
    const g = (h: string, n: number) => ({ heading: h, rows: Array(n).fill(0) })
    const groups = [g('a', 4), g('b', 18), g('c', 9), g('d', 9), g('e', 6), g('f', 4)]
    const cols = splitColumns(groups)
    expect(cols.flat().map(x => x.heading)).toEqual(['a', 'b', 'c', 'd', 'e', 'f'])
    const size = (c: typeof groups) => c.reduce((k, x) => k + x.rows.length + 1, 0)
    expect(Math.abs(size(cols[0]) - size(cols[1]))).toBeLessThanOrEqual(19)
    expect(cols[0].length).toBeGreaterThan(0)
    expect(cols[1].length).toBeGreaterThan(0)
    expect(splitColumns([g('only', 3)])).toEqual([[g('only', 3)], []])
  })
})
