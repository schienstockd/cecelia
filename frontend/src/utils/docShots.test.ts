// The user docs' screenshots (user-docs/shots/: one ShotSet per page), checked on every PR so a broken
// shot fails CI rather than the next capture: the registry passes checkShotSets (the same check
// scripts/docshots/capture.mjs runs first), every data-guide id a shot names exists in the app, and the
// figure geometry (figure.ts) that compose.mjs draws.
import { describe, expect, it } from 'vitest'
import { SHOT_SETS } from '../../../user-docs/shots/index.ts'
import { checkShotSets, fillFacts, guideIds, imageFile, shotAnchors, shotThemes, type Shot, type ShotSet } from '../../../user-docs/shots/spec.ts'
import { IN_APP_SHOTS } from '../../../user-docs/shots/in-app.ts'
import { TIPS } from '../lib/tips'
import { FEATURE_CARDS, featureFor } from '../lib/whatsNew'
import { figureGeometry, labelBox, type ShotMeta } from '../../../user-docs/shots/figure.ts'

// The app's source, read the way guides.test.ts reads it: a shot's data-guide id must exist there, so a
// renamed control fails CI instead of the next capture.
const SFC = import.meta.glob('/src/**/*.vue', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
const SOURCE = Object.values(SFC).join('\n')

const set = (over: Partial<ShotSet> = {}): ShotSet => ({ id: 'x', page: 'x.md', themes: ['light'], shots: [], ...over })
// what compose.mjs wrote: the committed images and their widths
const IMAGES = Object.keys(import.meta.glob('../../../user-docs/images/*.{png,jpg}')).map(p => p.split('/').pop()!)
const shot = (over: Partial<Shot> = {}): Shot => ({ id: 's1', crop: ['task.run'], ...over })

describe('doc shots', () => {
  it('every registered shot set passes the checks', () => {
    expect(checkShotSets(SHOT_SETS)).toEqual([])
  })

  it('every data-guide id a shot names exists in the app source', () => {
    const ids = guideIds(SHOT_SETS.flatMap(s => s.shots.flatMap(shotAnchors)))
    const missing = ids.filter(id => !SOURCE.includes(`data-guide="${id}"`) && !SOURCE.includes(`guide: '${id}'`))
    expect(missing).toEqual([])
  })

  it('refuses a duplicate shot id, a non-anchor and a set id that is not kebab-case', () => {
    const problems = checkShotSets([set({ id: 'A b', shots: [shot(), shot({ crop: ['Run button'] })] })]).map(i => i.problem)
    expect(problems).toEqual([
      'id must be kebab-case',
      'shot id "s1" is used twice',
      'shot "s1": "Run button" is not an anchor (data-guide id, nav:/route or css:…)',
    ])
    expect(shotAnchors(shot({ actions: [{ click: 'text:Kiwi' }, { click: 'prefs.kiwi' }], point: { anchor: 'x.y', label: 'l' } })))
      .toEqual(['task.run', 'x.y', 'prefs.kiwi'])
    expect(guideIds(['a.b@@2', 'css:.cc-modal', 'nav:/segment', 'a.b'])).toEqual(['a.b'])
  })

  // A shot names fixture keys, never a copy's random uid: one the fixture doesn't set up would open
  // nothing (or someone's real project) at capture time.
  it('refuses a profile, project or image the fixture does not set up', () => {
    const fixture = { projects: { p: { source: 'SRC', images: { img1: 'A' }, name: 'n' } }, profiles: [{ name: 'sam', ai: true }] }
    const ok = { id: 's', crop: ['x.y'], profile: 'sam', project: 'p', image: 'img1' }
    expect(checkShotSets([set({ fixture, shots: [ok, { ...ok, id: 's0', profile: 'default' }] })])).toEqual([])
    const problems = checkShotSets([set({ fixture, shots: [
      { ...ok, profile: 'alex' }, { ...ok, id: 's2', project: 'q' }, { ...ok, id: 's3', image: 'img9' },
    ] })]).map(i => i.problem)
    expect(problems).toEqual([
      'shot "s" runs as profile "alex", which the fixture does not create',
      'shot "s2" opens project "q", which the fixture does not copy',
      'shot "s2" focuses image "img1", which its fixture project does not copy',
      'shot "s3" focuses image "img9", which its fixture project does not copy',
    ])
  })

  // A number the page cites is the fixture's: an `expect` names it by fact, and capture checks the
  // pixels show the same number.
  it('refuses an expect citing a fact the fixture does not measure', () => {
    const fixture = { projects: { p: { source: 'S', images: { i: 'I' }, name: 'n', gates: [
      { image: 'i', name: 'G', x: 'a', y: 'b', xRange: [null, null] as [null, null], yRange: [null, null] as [null, null] },
      { image: 'i', name: 'qc', segmentation: 'T', fromSource: true as const }] } } }
    const exp = (text: string) => set({ fixture, shots: [shot({ expect: [{ anchor: 'x.y', text }] })] })
    expect(checkShotSets([exp('{{p.i.planes}} {{p.i.G.count}} {{p.i.T_qc.pct}} {{p.i.T.tracks}} {{p.i.interval}}')])).toEqual([])
    expect(checkShotSets([exp('{{p.j.planes}} {{p.i.H.count}} {{p.i.depth}} {{p.i.B.tracks}}')]).map(i => i.problem)).toEqual([
      '{{p.j.planes}}: the fixture copies no image "j" in project "p"',
      'shot "s1": {{p.i.H.count}} is not a fact the fixture measures (name, planes, channels, timepoints, interval, <gate>.count / .pct, or <segmentation>.tracks)',
      'shot "s1": {{p.i.depth}} is not a fact the fixture measures (name, planes, channels, timepoints, interval, <gate>.count / .pct, or <segmentation>.tracks)',
      'shot "s1": {{p.i.B.tracks}} is not a fact the fixture measures (name, planes, channels, timepoints, interval, <gate>.count / .pct, or <segmentation>.tracks)',
    ])
    // a caption is checked the same way
    expect(checkShotSets([set({ fixture, shots: [shot({ caption: '{{p.i.name}} {{p.i.depth}}' })] })]).map(i => i.problem))
      .toEqual(['shot "s1": {{p.i.depth}} is not a fact the fixture measures (name, planes, channels, timepoints, interval, <gate>.count / .pct, or <segmentation>.tracks)'])
    expect(fillFacts('{{p.i.planes}} z-planes, {{p.i.G.pct}}%', { 'p.i.planes': 26 }))
      .toEqual({ text: '26 z-planes, ?%', missing: ['{{p.i.G.pct}}'] })
  })
})

// Every shot sets the app's scheme; one page never mixes schemes, and the app shows its user's.
describe('doc shots: colour schemes', () => {
  it('refuses a set with no scheme, an unknown or repeated one, and an app set missing one', () => {
    const problems = (over: Partial<ShotSet>) => checkShotSets([set(over)]).map(i => i.problem)
    expect(problems({ themes: [] })).toEqual(['names no colour scheme (themes): every shot sets one explicitly'])
    expect(problems({ themes: ['light', 'light'] })).toEqual(['a colour scheme is listed twice'])
    expect(problems({ themes: ['sepia' as 'light'] })).toEqual(['"sepia" is not a colour scheme'])
    expect(problems({ page: 'app', themes: ['dark'] })).toEqual(
      ['an app set is captured in every colour scheme, so the app shows its user\'s and never mixes them'])
  })

  // A picture of the image has no app chrome, so one file serves every scheme; it may not crop to more.
  it('captures a picture once and keeps it to the canvas', () => {
    const pic = shot({ viewer: {}, crop: ['css:canvas'] })
    const s = set({ themes: ['dark', 'light'] })
    expect(shotThemes(s, pic)).toEqual(['dark'])
    expect([imageFile(pic, 'dark'), imageFile(shot(), 'light'), imageFile(shot(), 'dark')]).toEqual(['s1.jpg', 's1.png', 's1.dark.png'])
    expect(checkShotSets([set({ shots: [shot({ viewer: {}, crop: ['css:canvas', 'task.run'] })] })]).map(i => i.problem))
      .toEqual(['shot "s1" is a picture of the image: it crops to css:canvas alone and points at nothing, so no app chrome (which has a scheme) is in it'])
  })

  // The check that fails a set whose images disagree on scheme: every shot has an image in each of
  // its set's schemes (compose.mjs refuses a capture in the wrong one).
  it('every shot of every set is composed in each of its schemes', () => {
    const missing = SHOT_SETS.flatMap(s => s.shots.flatMap(x => shotThemes(s, x).map(t => imageFile(x, t))))
      .filter(f => !IMAGES.includes(f))
    expect(missing).toEqual([])
  })

  it('the app names only in-app shots', () => {
    const ids = new Set(IN_APP_SHOTS.shots.map(s => s.id))
    const named = [...TIPS.map(t => t.shot?.id), ...FEATURE_CARDS.map(f => f.shot.id)].filter((x): x is string => !!x)
    expect(named.filter(id => !ids.has(id))).toEqual([])
  })
})

describe("What's new feature cards", () => {
  const cards = [{ pr: 30, title: 'B', description: '', shot: { id: 'b' } }, { pr: 20, title: 'A', description: '', shot: { id: 'a' } }]
  it('the update shows the newest feature it brings, by merged PR or as cited in the notes', () => {
    expect(featureFor([20, 30], '', cards)?.title).toBe('B')
    expect(featureFor([], 'Fixes (#20)', cards)?.title).toBe('A')
    expect(featureFor([21], '#200', cards)).toBeNull()
  })
})

describe('doc figures', () => {
  const vp = { width: 1440, height: 900 }
  const meta = (over: Partial<ShotMeta> = {}): ShotMeta => ({
    id: 'f', viewport: vp, crop: ['panel'], point: { anchor: 'btn', label: 'Run it' },
    boxes: { panel: { x: 100, y: 100, width: 400, height: 300 }, btn: { x: 420, y: 350, width: 60, height: 24 } }, ...over,
  })

  it('crops to the panel, the pointer target and its label, at the app\'s own size', () => {
    const f = figureGeometry(meta())
    expect(f.k).toBe(1)
    expect(f.legible).toBe(true)
    // the label sits right of the button, outside the panel, so the crop grows to include it
    expect(f.label!.x + f.label!.width).toBeLessThanOrEqual(f.w)
    expect(f.x0).toBe(92)
    expect(f.target).toEqual({ x: 328, y: 258, width: 60, height: 24 })
  })

  it('never puts the label on its target and keeps it inside the window', () => {
    const edge = { x: 1400, y: 10, width: 30, height: 20 }       // far right: no room on the right
    const l = labelBox(edge, 'A long label for the edge', vp)
    expect(l.x + l.width).toBeLessThanOrEqual(vp.width)
    expect(l.x + l.width <= edge.x || l.y >= edge.y + edge.height).toBe(true)
  })

  it('draws the ring alone when the pointer has no label', () => {
    const f = figureGeometry(meta({ point: { anchor: 'btn' } }))
    expect(f.target).not.toBeNull()
    expect([f.label, f.arrow]).toEqual([null, null])
    expect(f.x0 + f.w).toBe(508)                                   // the panel's edge + pad: nothing grew it
  })

  // A shot of a narrow panel puts the label below or above, so the crop stays inside the panel.
  it('puts the label on the side the shot asks for', () => {
    const t = { x: 400, y: 300, width: 60, height: 24 }
    const below = labelBox(t, 'Here', vp, 'bottom'), above = labelBox(t, 'Here', vp, 'top')
    expect(below.y).toBeGreaterThanOrEqual(t.y + t.height)
    expect(above.y + above.height).toBeLessThanOrEqual(t.y)
  })

  // A slot of fixed shape (a tip's 2:1 frame) trims the crop about its centre; a picture has no margin.
  it('trims to the slot shape and fits the slot width', () => {
    const m = meta({ crop: ['wide'], point: null, boxes: { wide: { x: 100, y: 100, width: 800, height: 600 } } })
    const f = figureGeometry(m, { aspect: 2, column: 400, pad: 0 })
    expect([f.x0, f.w]).toEqual([101, 798])
    expect(f.w / f.h).toBeCloseTo(2)
    expect(f.w * f.k).toBeCloseTo(400)
    expect(figureGeometry(m, { pad: -5 }).x0).toBe(106)
  })

  it('flags a crop so wide its text shows too small', () => {
    const f = figureGeometry(meta({ crop: ['wide'], point: null, boxes: { wide: { x: 0, y: 0, width: 1440, height: 200 } } }))
    expect(f.legible).toBe(false)
    expect(f.k).toBeLessThan(1)
  })
})
