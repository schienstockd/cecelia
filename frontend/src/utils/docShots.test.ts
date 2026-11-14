// The user docs' screenshots (user-docs/shots/: one ShotSet per page), checked on every PR so a broken
// shot fails CI rather than the next capture: the registry passes checkShotSets (the same check
// scripts/docshots/capture.mjs runs first), every data-guide id a shot names exists in the app, and the
// figure geometry (figure.ts) that compose.mjs draws.
import { describe, expect, it } from 'vitest'
import { SHOT_SETS } from '../../../user-docs/shots/index.ts'
import { checkShotSets, fillFacts, guideIds, shotAnchors, type Shot, type ShotSet } from '../../../user-docs/shots/spec.ts'
import { figureGeometry, labelBox, type ShotMeta } from '../../../user-docs/shots/figure.ts'

// The app's source, read the way guides.test.ts reads it: a shot's data-guide id must exist there, so a
// renamed control fails CI instead of the next capture.
const SFC = import.meta.glob('/src/**/*.vue', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
const SOURCE = Object.values(SFC).join('\n')

const set = (over: Partial<ShotSet> = {}): ShotSet => ({ id: 'x', page: 'x.md', shots: [], ...over })
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
      '{{p.i.H.count}}: not a fact the fixture measures (planes, channels, timepoints, interval, <gate>.count / .pct, or <segmentation>.tracks)',
      '{{p.i.depth}}: not a fact the fixture measures (planes, channels, timepoints, interval, <gate>.count / .pct, or <segmentation>.tracks)',
      '{{p.i.B.tracks}}: not a fact the fixture measures (planes, channels, timepoints, interval, <gate>.count / .pct, or <segmentation>.tracks)',
    ])
    expect(fillFacts('{{p.i.planes}} z-planes, {{p.i.G.pct}}%', { 'p.i.planes': 26 }))
      .toEqual({ text: '26 z-planes, ?%', missing: ['{{p.i.G.pct}}'] })
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

  it('flags a crop so wide its text shows too small', () => {
    const f = figureGeometry(meta({ crop: ['wide'], point: null, boxes: { wide: { x: 0, y: 0, width: 1440, height: 200 } } }))
    expect(f.legible).toBe(false)
    expect(f.k).toBeLessThan(1)
  })
})
