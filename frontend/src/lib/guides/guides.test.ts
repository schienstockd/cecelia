import { describe, it, expect } from 'vitest'
import { GUIDES, GROUP_ORDER, guideById, guidesByGroup, RECIPES, isWanted } from './index'
import { recipeRequestUrl } from '../links'
import { PREREQ } from './prereqs'
import { TASK_RUN_USES } from './moduleTask'
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

// The recipes module read as text, for the "no task names in here" check below — same `?raw` idiom.
const RECIPES_RAW = import.meta.glob('/src/lib/guides/recipes.ts', { query: '?raw', import: 'default', eager: true }) as
  Record<string, string>

const SOURCE = Object.values(SFC).join('\n')
const RECIPES_SRC = Object.values(RECIPES_RAW)[0] ?? ''
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
describe('the task-run block reads the selection scope its page actually uses', () => {
  it('matches <ModuleLayout module="…"> in the page SFC for every route', () => {
    const wrong: string[] = []
    expect(TASK_RUN_USES.length).toBeGreaterThan(0)     // the loop below must not pass vacuously
    for (const g of TASK_RUN_USES) {
      // route → component path, from the one route table
      const routeRe = new RegExp(`path:\\s*'${g.route}'[^}]*?import\\('\\.(/modules/[^']+)'\\)`)
      const m = mainTs.match(routeRe)
      if (!m) { wrong.push(`${g.taskKey}: no route ${g.route} in main.ts`); continue }
      const src = SFC[`/src${m[1]}`]
      if (!src) { wrong.push(`${g.taskKey}: no SFC at /src${m[1]}`); continue }
      // the FIRST ModuleLayout module= on the page is the one that scopes the selection
      const layout = src.match(/<ModuleLayout[^>]*?\bmodule="([^"]+)"/)
      if (!layout) { wrong.push(`${g.taskKey}: page has no <ModuleLayout module="…">`); continue }
      if (layout[1] !== g.selectionModule) {
        wrong.push(`${g.taskKey} on ${g.route}: selectionModule '${g.selectionModule}' but page uses '${layout[1]}'`)
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

  // The orientation tour is the one guide that runs BEFORE the user has anything, and it is started
  // automatically on first launch (App.vue) — on an empty project, with no images and often no
  // project at all. A prereq on it would make the picker declare the welcome tour blocked for exactly
  // the person it was written for, and the "Show me" button on the about card would open a row of
  // amber warnings. So: no prereqs, and no step may point at data-dependent chrome.
  it('the orientation tour needs nothing and points only at app chrome', () => {
    const tour = guideById('find-your-way-around')
    expect(tour, 'the tour the about card and App.vue both name must exist').toBeTruthy()
    expect(tour!.prereqs).toEqual([])

    // Anchors whose element only renders once there is data. `nav:` and the header/sidebar/console
    // anchors are always in the shell, so anything in these families is the failure being pinned.
    const DATA_DEPENDENT = ['images.', 'popmanager.', 'board.', 'set.', 'task.', 'notebooks.', 'viewer.']
    const bad: string[] = []
    tour!.steps.forEach((s, i) => {
      for (const a of [s.anchor, ...(s.reveal ? (Array.isArray(s.reveal) ? s.reveal : [s.reveal]) : []).map(r => r.anchor)]) {
        if (a && DATA_DEPENDENT.some(p => a.startsWith(p))) bad.push(`step ${i + 1}: ${a}`)
      }
    })
    expect(bad).toEqual([])
  })

  // 'Start' has no sidebar counterpart, so nothing else would notice it being dropped from
  // GROUP_ORDER — and a group not in that list renders unlabelled at the BOTTOM of the picker, which
  // is the worst possible place for the orientation tour.
  it('puts the Start group first, so the tour is the first thing in the picker', () => {
    expect(GROUP_ORDER[0]).toBe('Start')
    expect(guidesByGroup()[0]?.guides[0]?.id).toBe('find-your-way-around')
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

// Recipes (plan D1/D9): a list of existing guides with a reason attached. The catalogue ratchets above
// keep a GUIDE honest; these keep a recipe from pointing at something that isn't there, and from
// shipping half-written — a row with neither steps nor the request link reads as a recipe that exists
// and does nothing.
describe('recipes compose real guides', () => {
  it('every step names a guide in the catalogue', () => {
    const dangling: string[] = []
    for (const r of RECIPES) {
      for (const s of r.steps ?? []) {
        if (!guideById(s.guide)) dangling.push(`${r.id} → ${s.guide}`)
      }
    }
    expect(dangling).toEqual([])
  })

  it('ids are unique, and distinct from the guide ids they compose', () => {
    const ids = RECIPES.map(r => r.id)
    expect(new Set(ids).size).toBe(ids.length)
    // A recipe and a guide are two different things behind the same Start button; sharing an id would
    // make `recipeById`/`guideById` disagree about what the user clicked.
    expect(ids.filter(id => guideById(id))).toEqual([])
  })

  it('is either written or wanted, never neither and never both', () => {
    for (const r of RECIPES) {
      if (isWanted(r)) {
        expect(r.steps, `${r.id} is wanted but carries steps`).toBeUndefined()
      } else {
        expect(r.steps.length, `${r.id} has no steps`).toBeGreaterThan(0)
        expect(r.whenThisIsYou.trim().length, `${r.id} has no recognition line`).toBeGreaterThan(0)
        for (const s of r.steps) {
          expect(s.why.trim().length, `${r.id} → ${s.guide} has no reason`).toBeGreaterThan(0)
        }
      }
    }
  })

  // Same discipline as the guide copy budget: one line each. `whenThisIsYou` is a recognition test and
  // a `why` states one fork — either one running long means it has become a summary of the steps,
  // which is what the guides themselves are for.
  it('keeps recipe copy to one line', () => {
    const over: string[] = []
    for (const r of RECIPES) {
      if (isWanted(r)) continue
      if (r.whenThisIsYou.length > 100) over.push(`${r.id} whenThisIsYou ${r.whenThisIsYou.length} chars`)
      for (const s of r.steps) {
        if (s.why.length > 110) over.push(`${r.id} → ${s.guide} why ${s.why.length} chars`)
      }
    }
    expect(over).toEqual([])
  })

  // A recipe names guides, never task functions — those live in the guide definitions, where the Julia
  // ratchet (`app/test/suite.jl` → "guide catalogue names real tasks") can check each against the task
  // registry. It reads this whole directory as one blob, so a task name pasted in here would be
  // counted as a guide's and break the pairing.
  it('keeps task names out of the recipes module', () => {
    expect(RECIPES_SRC.length).toBeGreaterThan(0)
    expect(RECIPES_SRC).not.toMatch(/funName:\s*'/)
    expect(RECIPES_SRC).not.toMatch(/taskKey:\s*'/)
  })

  // The request rows are the one place the app admits a gap, so the link has to reach the form that
  // asks for what would close it — what they image, and an example image (plan D9).
  it('a request link points at the recipe issue form, prefilled', () => {
    const url = recipeRequestUrl('Large multiplex images')
    expect(url).toContain('/issues/new?')
    expect(url).toContain('template=recipe_request.yml')
    expect(url).toContain('title=Recipe%3A+Large+multiplex+images')
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

  // The bug this pins: it read `status === 'done'`, a hand-rolled second definition of "imported".
  // `status` is the transient conversion-job state, so a project full of long-since-converted images
  // reported the prereq as MISSING and every guide looked blocked. The canonical answer is
  // `isImported` — does the image HAVE a converted file — which is what the image table uses to decide
  // whether the napari eye is enabled.
  it('imageImported asks whether a CONVERTED file exists, not what status says', () => {
    expect(PREREQ.imageImported.ok(ctx({ images: [img()] }))).toBe(false)          // registered only
    expect(PREREQ.imageImported.ok(ctx({ images: [img({ filepaths: {} })] }))).toBe(false)
    // converted → has a filepath, regardless of what `status` happens to hold
    expect(PREREQ.imageImported.ok(ctx({
      images: [img({ status: 'pending', filepaths: { default: 'ccidImage.ome.zarr' } })],
    }))).toBe(true)
  })

  it('timeSeries needs more than one frame', () => {
    expect(PREREQ.timeSeries.ok(ctx({ images: [img({ sizeT: 1 })] }))).toBe(false)
    expect(PREREQ.timeSeries.ok(ctx({ images: [img({ sizeT: 40 })] }))).toBe(true)
  })

  it('segmented reads the label sets', () => {
    expect(PREREQ.segmented.ok(ctx({ images: [img({ labels: {} })] }))).toBe(false)
    expect(PREREQ.segmented.ok(ctx({ images: [img({ labels: { default: ['x.zarr'] } })] }))).toBe(true)
  })

  // The regression this encodes: the prereq used to scan the run log for `tracking.*`, so a project
  // migrated from the R version — tracks on disk, no `tracking.*` entry ever recorded — was told it
  // "needs a tracked image" (Dominik, 4kS67f). Provenance is not state.
  it('tracked reads the tracks on disk, NOT the run log', () => {
    const ran = (fun: string) => ({ fun, at: '2026-01-01T00:00:00' })
    expect(PREREQ.tracked.ok(ctx({ images: [img({ trackValueNames: [] })] }))).toBe(false)
    expect(PREREQ.tracked.ok(ctx({ images: [img({ trackValueNames: ['B', 'T'] })] }))).toBe(true)
    // migrated data: sidecars present, run log has no tracking entry at all
    expect(PREREQ.tracked.ok(ctx({
      images: [img({ trackValueNames: ['B'], runLog: [ran('clustTracks.cluster')] })],
    }))).toBe(true)
    // and the converse — a run log saying tracking ran, with no track table to show for it
    expect(PREREQ.tracked.ok(ctx({
      images: [img({ trackValueNames: [], runLog: [ran('tracking.bayesian_tracking')] })],
    }))).toBe(false)
  })
})
