import { describe, it, expect } from 'vitest'
import { GUIDES, GROUP_ORDER, guideById, guidesByGroup } from './index'
import { PREREQ } from './prereqs'
import { MODULE_TASK_GUIDES } from './moduleTask'
import { anchorSelector, NAV_PREFIX } from '../../utils/guideAnchor'
import type { GuideCtx } from './types'

// THE ratchet for the guide system (plan D4). A guide's step points at a control by anchor id; if the
// control is renamed or the attribute dropped, the guide silently stops working — and only for the one
// user being onboarded, who will not report it. So every anchor id in the catalogue is asserted to
// exist in the source. Renaming a button now fails CI instead.
//
// The rest of this file pins the invariants that make the catalogue safe to extend: prereqs are pure,
// steps say something, and no step can be both silent and ungated.

// Source is loaded through Vite's `?raw` glob, the same way `uiCopy.test.ts` reads every SFC — it
// needs no `@types/node` (the app tsconfig deliberately ships only `vite/client`) and it resolves
// relative to the project root rather than to this file's location.
const SFC = import.meta.glob('/src/**/*.vue', { query: '?raw', import: 'default', eager: true }) as
  Record<string, string>
const MAIN_TS = import.meta.glob('/src/main.ts', { query: '?raw', import: 'default', eager: true }) as
  Record<string, string>

const SOURCE = Object.values(SFC).join('\n')
const mainTs = Object.values(MAIN_TS)[0] ?? ''

// Every anchor a step can point at, including the ones only a `reveal` uses.
function anchorIds(): { id: string; where: string }[] {
  const out: { id: string; where: string }[] = []
  for (const g of GUIDES) {
    g.steps.forEach((s, i) => {
      const where = `${g.id} step ${i + 1}`
      if (s.anchor) out.push({ id: s.anchor, where })
      // a step may declare several reveal causes (plan D5) — every one of their anchors counts
      for (const r of s.reveal ? (Array.isArray(s.reveal) ? s.reveal : [s.reveal]) : []) {
        if (r.anchor) out.push({ id: r.anchor, where: `${where} (reveal)` })
      }
    })
  }
  return out
}

describe('guide anchors exist in the source', () => {
  it('every data-guide anchor a step points at is present in the markup', () => {
    const missing = anchorIds()
      .filter(a => !a.id.startsWith(NAV_PREFIX))
      .filter(a => !SOURCE.includes(`data-guide="${a.id}"`))
      .map(a => `${a.id} (${a.where})`)
    expect(missing).toEqual([])
  })

  it('every nav: anchor names a route the app actually registers', () => {
    // main.ts is the one route table; a guide pointing at /import (which was renamed to
    // /manage-images) would otherwise sit there waiting for a click on a link that does not exist.
    const bad = anchorIds()
      .filter(a => a.id.startsWith(NAV_PREFIX))
      .map(a => ({ ...a, path: a.id.slice(NAV_PREFIX.length) }))
      .filter(a => !mainTs.includes(`path: '${a.path}'`))
      .map(a => `${a.path} (${a.where})`)
    expect(bad).toEqual([])
  })

  it('a route a step declares is a real route too', () => {
    const bad: string[] = []
    for (const g of GUIDES) {
      g.steps.forEach((s, i) => {
        if (s.route && !mainTs.includes(`path: '${s.route}'`)) bad.push(`${g.id} step ${i + 1}: ${s.route}`)
      })
    }
    expect(bad).toEqual([])
  })

  it('builds a selector for both anchor schemes', () => {
    expect(anchorSelector('segment.run')).toBe('[data-guide="segment.run"]')
    expect(anchorSelector('nav:/manage-images')).toBe('a[href="#/manage-images"]')
  })
})

// The bug this exists for: `BehaviourModule` passes `module="behaviourAnalysis"` to ModuleLayout and
// `module="behaviour"` to TaskRunner. ModuleLayout owns the image-table selection, so a guide reading
// the runner's key gets a permanently EMPTY selection — the "tick the images" gate never fires and the
// user has to click past a step that looks broken. Nothing else would catch it: both strings exist in
// the codebase, the types are satisfied, and the guide otherwise works.
describe('a builder guide reads the selection scope its page actually uses', () => {
  it('matches <ModuleLayout module="…"> in the page SFC for every route', () => {
    const wrong: string[] = []
    for (const g of MODULE_TASK_GUIDES) {
      // route → component path, from the one route table
      const routeRe = new RegExp(`path:\\s*'${g.route}'[^}]*?import\\('\\.(/modules/[^']+)'\\)`)
      const m = mainTs.match(routeRe)
      if (!m) { wrong.push(`${g.id}: no route ${g.route} in main.ts`); continue }
      const src = SFC[`/src${m[1]}`]
      if (!src) { wrong.push(`${g.id}: no SFC at /src${m[1]}`); continue }
      // the FIRST ModuleLayout module= on the page is the one that scopes the selection
      const layout = src.match(/<ModuleLayout[^>]*?\bmodule="([^"]+)"/)
      if (!layout) { wrong.push(`${g.id}: page has no <ModuleLayout module="…">`); continue }
      if (layout[1] !== g.selectionModule) {
        wrong.push(`${g.id}: selectionModule '${g.selectionModule}' but page uses '${layout[1]}'`)
      }
    }
    expect(wrong).toEqual([])
  })
})

describe('the catalogue is well formed', () => {
  it('ids are unique', () => {
    const ids = GUIDES.map(g => g.id)
    expect(new Set(ids).size).toBe(ids.length)
  })

  it('every guide has steps, and every step says something', () => {
    for (const g of GUIDES) {
      expect(g.steps.length, `${g.id} has no steps`).toBeGreaterThan(0)
      g.steps.forEach((s, i) => {
        expect(s.text.trim().length, `${g.id} step ${i + 1} has no text`).toBeGreaterThan(0)
      })
    }
  })

  it('puts every guide in a known group, so none hides in an unlabelled section', () => {
    const unknown = GUIDES.filter(g => !(GROUP_ORDER as readonly string[]).includes(g.group))
    expect(unknown.map(g => `${g.id}: ${g.group}`)).toEqual([])
  })

  it('groups render in sidebar order and lose nothing', () => {
    const grouped = guidesByGroup()
    expect(grouped.map(x => x.group)).toEqual(
      GROUP_ORDER.filter(gr => GUIDES.some(g => g.group === gr)))
    expect(grouped.flatMap(x => x.guides).length).toBe(GUIDES.length)
  })

  it('every fixGuide points at a guide that exists and is not itself', () => {
    for (const g of GUIDES) {
      for (const p of g.prereqs) {
        if (!p.fixGuide) continue
        expect(guideById(p.fixGuide), `${g.id} prereq ${p.id} → ${p.fixGuide}`).toBeTruthy()
        expect(p.fixGuide).not.toBe(g.id)
      }
    }
  })

  // Copy budget (docs/UI.md → Guide copy): one sentence plus at most four imperative lines. The tooltip
  // budget is 90 chars and a guide step legitimately needs more, but "more" is not "unbounded".
  it('keeps step copy within the guide budget', () => {
    const over: string[] = []
    for (const g of GUIDES) {
      g.steps.forEach((s, i) => {
        const at = `${g.id} step ${i + 1}`
        if (s.text.length > 140) over.push(`${at} text ${s.text.length} chars`)
        if ((s.bullets?.length ?? 0) > 4) over.push(`${at} has ${s.bullets!.length} bullets`)
        for (const b of s.bullets ?? []) {
          if (b.length > 110) over.push(`${at} bullet ${b.length} chars`)
        }
      })
    }
    expect(over).toEqual([])
  })
})

// The tip↔guide link (plan D7): a tip card's "Show me" button hands its topic to the guide. The point
// of the link is that a topic is described in ONE place, so a dangling id would silently drop the
// button and leave the tip as the only explanation — exactly the drift the single catalogue avoids.
describe('the What\'s New tips link to real guides', () => {
  it('every tip guideId resolves', async () => {
    const { TIPS } = await import('../tips')
    const dangling = TIPS
      .filter(t => t.guideId)
      .filter(t => !guideById(t.guideId!))
      .map(t => `${t.id} → ${t.guideId}`)
    expect(dangling).toEqual([])
  })
})

describe('prerequisites are pure predicates over the snapshot', () => {
  const ctx = (over: Partial<GuideCtx> = {}): GuideCtx => ({
    route: '/manage-images',
    hasProject: false,
    setUid: null,
    setCount: 0,
    images: [],
    napariImageUid: null,
    selection: () => [],
    rightPanelCollapsed: false,
    viewerPanelOpen: false,
    anchorValue: () => null,
    anchorExists: () => false,
    anchorReachable: () => false,
    ...over,
  })
  const img = (over: Record<string, unknown> = {}) =>
    ({ uid: 'i1', name: 'a.czi', status: 'done', ...over }) as GuideCtx['images'][number]

  it('answers false on an empty app without throwing', () => {
    for (const g of GUIDES) {
      for (const p of g.prereqs) expect(typeof p.ok(ctx())).toBe('boolean')
    }
  })

  it('projectOpen follows the project', () => {
    expect(PREREQ.projectOpen.ok(ctx())).toBe(false)
    expect(PREREQ.projectOpen.ok(ctx({ hasProject: true }))).toBe(true)
  })

  it('imageImported needs a CONVERTED image, not merely a row', () => {
    expect(PREREQ.imageImported.ok(ctx({ images: [img({ status: 'converting' })] }))).toBe(false)
    expect(PREREQ.imageImported.ok(ctx({ images: [img()] }))).toBe(true)
  })

  it('timeSeries needs more than one frame', () => {
    expect(PREREQ.timeSeries.ok(ctx({ images: [img({ sizeT: 1 })] }))).toBe(false)
    expect(PREREQ.timeSeries.ok(ctx({ images: [img({ sizeT: 40 })] }))).toBe(true)
  })

  it('segmented reads the label sets', () => {
    expect(PREREQ.segmented.ok(ctx({ images: [img({ labels: {} })] }))).toBe(false)
    expect(PREREQ.segmented.ok(ctx({ images: [img({ labels: { default: ['x.zarr'] } })] }))).toBe(true)
  })

  it('tracked reads the run log, and ignores a FAILED tracking run', () => {
    const ran = (fun: string, status?: string) => ({ fun, status, at: '2026-01-01T00:00:00' })
    expect(PREREQ.tracked.ok(ctx({ images: [img({ runLog: [ran('segment.cellpose')] })] }))).toBe(false)
    expect(PREREQ.tracked.ok(ctx({
      images: [img({ runLog: [ran('tracking.bayesian_tracking', 'failed')] })],
    }))).toBe(false)
    expect(PREREQ.tracked.ok(ctx({
      images: [img({ runLog: [ran('tracking.bayesian_tracking')] })],
    }))).toBe(true)
  })
})
