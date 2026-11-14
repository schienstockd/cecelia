// The user booklets' frame rules (user-docs/booklets/spec.ts → checkBooklets), run over the real
// registry so CI catches a broken booklet before anyone builds the PDFs. The build
// (scripts/booklets/build.mjs) runs the same check; this is the copy that runs on every PR.
import { describe, expect, it } from 'vitest'
import { BOOKLETS } from '../../../user-docs/booklets/index.ts'
import { bookletNumbers, checkBooklets, fillFacts, guideIds, recipeStep, recipeTitle, shotAnchors, type Booklet, type Shot } from '../../../user-docs/booklets/spec.ts'
import { figureGeometry, labelBox, MM_PER_PX, type ShotMeta } from '../../../user-docs/booklets/figure.ts'

// The app's source, read the way guides.test.ts reads it: a shot's data-guide id must exist there, so a
// renamed control fails CI instead of the next capture.
const SFC = import.meta.glob('/src/**/*.vue', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
const SOURCE = Object.values(SFC).join('\n')

const minimal = (over: Partial<Booklet> = {}): Booklet => ({
  id: 'x', series: 'C', title: 'X', audience: 'a', get: 'g', need: ['n'], endsAt: 'e',
  intro: [{ kind: 'para', text: 'p' }], steps: [], askAi: null, ...over,
})

describe('user booklets', () => {
  it('every registered booklet passes the frame rules', () => {
    expect(checkBooklets(BOOKLETS)).toEqual([])
  })

  it('numbers are generated per series in registry order', () => {
    const n = bookletNumbers([minimal({ id: 'a', series: 'B' }), minimal({ id: 'b' }), minimal({ id: 'c', series: 'B' })])
    expect([...n.values()]).toEqual(['B1', 'C1', 'B2'])
  })

  it('refuses an empty "If it goes wrong", a dangling Next and a B-series booklet without its frame', () => {
    const step = { title: 't', do: 'd', see: 's', ifWrong: ' ' }
    const problems = checkBooklets([
      minimal({ id: 'c', steps: [step], next: 'nope' }),
      minimal({ id: 'b', series: 'B', askAi: null }),
    ]).map(i => `${i.booklet}: ${i.problem}`)
    expect(problems).toEqual([
      'c: step 1: "ifWrong" is empty',
      'b: B-series booklet has no "Before you image" rows',
      'b: B-series booklets are showcases (plan → D6)',
      'b: B-series booklet does not name its sample',
      'b: only C-series booklets may drop the Ask AI box',
      'c: "next" names unknown booklet "nope"',
    ])
  })

  // D6: a showcase hands the doing to the in-app guide, so it has no steps, names a real recipe, says
  // its limits and shows 3–4 things; a guide never carries a showcase body.
  it('holds a showcase to its own frame', () => {
    const shows = (n: number) => Array.from({ length: n }, (_, i) => ({ title: `s${i}`, text: 't' }))
    const body = { outcome: { shot: 'o', text: 't' }, time: 't', shows: shows(3), limits: ['l'], recipe: 'few-marker-tissue' }
    const sc = (over: Partial<Booklet> = {}) => minimal({
      id: 'b', series: 'B', kind: 'showcase', sample: 's', beforeYouImage: [{ setting: 's', limits: 'l', shows: 'w' }],
      askAi: { prompt: 'p', where: 'w' }, showcase: body, shots: [{ id: 'o', crop: ['x.y'] }], ...over })
    expect(checkBooklets([sc()])).toEqual([])
    expect(recipeTitle('few-marker-tissue')).toBe('Few-marker tissue')       // what Do it prints
    const problems = checkBooklets([
      sc({ steps: [{ title: 't', do: 'd', see: 's', ifWrong: 'w' }] }),
      sc({ id: 'b2', showcase: { ...body, shows: shows(5), limits: [], recipe: 'nope' }, shots: [] }),
      minimal({ id: 'g', showcase: body }),
    ]).map(i => `${i.booklet}: ${i.problem}`)
    expect(problems).toEqual([
      'b: a showcase has no steps: the doing is the in-app guide',
      'b2: a showcase shows 3 or 4 things, not 5',
      'b2: a showcase names its limits',
      'b2: showcase names unknown recipe "nope"',
      'g: a guide has no "showcase" body',
      'b2: showcase names shot "o", which this booklet does not define',
      'g: showcase names shot "o", which this booklet does not define',
    ])
  })

  // A show quotes its recipe step, so renaming or dropping the step breaks the build, not the booklet.
  it('a show names a step its recipe has', () => {
    expect(recipeStep('few-marker-tissue', 'gate-populations')?.why).toMatch(/debris/)
    expect(recipeStep('few-marker-tissue', 'Detect aggregates')).toBeDefined()   // a page step, by label
    const body = { outcome: { shot: 'o', text: 't' }, time: 't', limits: ['l'], recipe: 'few-marker-tissue',
      shows: [{ title: 'a', step: 'gate-populations' }, { title: 'b', step: 'track-cells' }, { title: 'c' }] }
    const problems = checkBooklets([minimal({ id: 'b', series: 'B', kind: 'showcase', sample: 's', steps: [],
      beforeYouImage: [{ setting: 's', limits: 'l', shows: 'w' }], askAi: { prompt: 'p', where: 'w' },
      showcase: body, shots: [{ id: 'o', crop: ['x.y'] }] })]).map(i => i.problem)
    expect(problems).toEqual([
      'show 2 names step "track-cells", which recipe "few-marker-tissue" does not have',
      'show 3 has neither a recipe step nor text',
    ])
  })

  // One sample, one set of counts: a number a caption cites is the fixture's, measured on the copy.
  it('refuses a typed count and a fact the fixture does not measure', () => {
    const fixture = { projects: { p: { source: 'S', images: { i: 'I' }, name: 'n',
      gates: [{ image: 'i', name: 'G', x: 'a', y: 'b', xRange: [null, null] as [null, null], yRange: [null, null] as [null, null] }] } } }
    const ok = '{{p.i.channels}} channels, {{p.i.planes}} z-planes; {{p.i.G.count}} cells, {{p.i.G.pct}}%'
    expect(checkBooklets([minimal({ fixture, sample: ok })])).toEqual([])
    const problems = checkBooklets([minimal({ fixture, sample: 'a stack of 26 z-planes' }),
      minimal({ id: 'y', fixture, get: '{{p.j.planes}} and {{p.i.H.count}} and {{p.i.depth}}' })]).map(i => `${i.booklet}: ${i.problem}`)
    expect(problems).toEqual([
      'x: "26 z-planes" is typed by hand: cite the fixture\'s number ({{project.image.fact}})',
      'y: {{p.j.planes}}: the fixture copies no image "j" in project "p"',
      'y: {{p.i.H.count}}: not a fact the fixture measures (planes, channels, or <gate>.count / .pct)',
      'y: {{p.i.depth}}: not a fact the fixture measures (planes, channels, or <gate>.count / .pct)',
    ])
    expect(fillFacts(ok, { 'p.i.channels': 6, 'p.i.planes': 26, 'p.i.G.count': 1244 }))
      .toEqual({ text: '6 channels, 26 z-planes; 1244 cells, ?%', missing: ['{{p.i.G.pct}}'] })
  })

  it('every data-guide id a shot names exists in the app source', () => {
    const ids = guideIds(BOOKLETS.flatMap(b => (b.shots ?? []).flatMap(shotAnchors)))
    const missing = ids.filter(id => !SOURCE.includes(`data-guide="${id}"`) && !SOURCE.includes(`guide: '${id}'`))
    expect(missing).toEqual([])
  })

  it('refuses a step naming an undefined shot, a duplicate shot id and a non-anchor', () => {
    const shot = (over: Partial<Shot> = {}): Shot => ({ id: 's1', crop: ['task.run'], ...over })
    const step = { title: 't', do: 'd', see: 's', ifWrong: 'w' }
    const problems = checkBooklets([
      minimal({ id: 'a', steps: [{ ...step, shot: 'nope' }], shots: [shot(), shot({ crop: ['Run button'] })] }),
    ]).map(i => i.problem)
    expect(problems).toEqual([
      'shot id "s1" is used twice',
      'shot "s1": "Run button" is not an anchor (data-guide id, nav:/route or css:…)',
      'step 1 names shot "nope", which this booklet does not define',
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
    expect(checkBooklets([minimal({ id: 'a', fixture, shots: [ok, { ...ok, id: 's0', profile: 'default' }] })])).toEqual([])
    const problems = checkBooklets([minimal({ id: 'a', fixture, shots: [
      { ...ok, profile: 'alex' }, { ...ok, id: 's2', project: 'q' }, { ...ok, id: 's3', image: 'img9' },
    ] })]).map(i => i.problem)
    expect(problems).toEqual([
      'shot "s" runs as profile "alex", which the fixture does not create',
      'shot "s2" opens project "q", which the fixture does not copy',
      'shot "s2" focuses image "img1", which its fixture project does not copy',
      'shot "s3" focuses image "img9", which its fixture project does not copy',
    ])
  })
})

describe('booklet figures', () => {
  const vp = { width: 1440, height: 900 }
  const meta = (over: Partial<ShotMeta> = {}): ShotMeta => ({
    id: 'f', viewport: vp, crop: ['panel'], point: { anchor: 'btn', label: 'Run it' },
    boxes: { panel: { x: 100, y: 100, width: 400, height: 300 }, btn: { x: 420, y: 350, width: 60, height: 24 } }, ...over,
  })

  it('crops to the panel, the pointer target and its label, at the one print scale', () => {
    const f = figureGeometry(meta())
    expect(f.k).toBe(MM_PER_PX)
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

  it('flags a crop so wide its text prints too small', () => {
    const f = figureGeometry(meta({ crop: ['wide'], point: null, boxes: { wide: { x: 0, y: 0, width: 1440, height: 200 } } }))
    expect(f.legible).toBe(false)
    expect(f.k).toBeLessThan(MM_PER_PX)
  })
})
