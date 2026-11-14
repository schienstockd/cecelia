// The screenshots of the user docs (user-docs/*.md): one `ShotSet` per page, each the shots that page
// shows and the fixture they run on. scripts/docshots/ captures them from an ISOLATED instance
// (capture.mjs, after fixture.mjs) and composes each into `user-docs/images/<id>.png` (compose.mjs). How
// to write a shot: README.md beside this file.
//
// Plain TypeScript with no TS-only runtime syntax (no enums, no namespaces), so Node loads it
// directly (type stripping) — the scripts need no bundler.

import type { Side } from '../../frontend/src/utils/anchorPosition.ts'
import { COLOUR_SCHEMES, type ColourScheme } from '../../frontend/src/utils/colourScheme.ts'

/** The facts a page may cite, measured by doc-fixture on the copy (`<set>.facts.json` beside the
 *  captures): `{{<project>.<image>.name}}` (the name the copy shows), `.planes`, `.channels`,
 *  `.timepoints`, `.interval` (seconds per frame); per fixture gate `.<gate>.count` / `.<gate>.pct` (`gateKey`); per tracked segmentation
 *  `.<seg>.tracks`. A shot's `expect` may cite one, so the page's number is the one in the pixels. */
export const FACT_TOKEN = /\{\{([\w-]+)\.([\w-]+)\.([\w-]+)(?:\.(count|pct|tracks))?\}\}/g
const IMAGE_FACTS = ['name', 'planes', 'channels', 'timepoints', 'interval']

/** A fixture gate's fact name: its name, prefixed by its segmentation when that is not `cells`
 *  (every population's segmentation has its own `qc`). */
export const gateKey = (g: FixtureGate): string => (g.segmentation && g.segmentation !== 'cells' ? `${g.segmentation}_${g.name}` : g.name)

/** Where on the page: a `data-guide` id (`prefs.aiFeatures`), `nav:/route` (a sidebar item) — both
 *  through the app's own `anchorSelector` (utils/guideAnchor.ts) — or `css:<selector>` for an element
 *  with no anchor (a modal box). `@@n` picks the n-th match (0-based). Prefer a data-guide id: the
 *  test asserts those exist in the source, so a renamed control fails CI, not the capture. */
export type Anchor = string

/** A user action, run in order before the capture. `text:` clicks the element showing that text. */
export type Action =
  | { click: Anchor | `text:${string}` }
  | { fill: Anchor; text: string }
  | { hover: Anchor }
  | { press: string }
  /** Wait for an anchor to appear (a dialog opening), not a timer. */
  | { waitFor: Anchor }
  /** Size a floating panel (its `storage-key`) before it opens: a reply taller than the default panel
   *  would be cut off. Written where FloatingPanel keeps its geometry. */
  | { panel: string; w: number; h: number; y?: number }
  /** Scroll the anchor's own scroll container until the anchor's bottom is in view. */
  | { scroll: Anchor }
  /** Unfold a fold toggle (`aria-expanded`): clicked only when folded, as a fold's state persists per
   *  profile and a plain click would close one the last shot left open. */
  | { open: Anchor }

/** One screenshot, taken by scripts/docshots/capture.mjs from an ISOLATED instance and composed into a
 *  page's image by scripts/docshots/compose.mjs (crop + pointer drawn on top, so the look can change
 *  without recapturing). */
export interface Shot {
  id: string
  /** The Cecelia profile it runs as (a `fixture.profiles` name); none = `default`. */
  profile?: string
  /** A `fixture.projects` key; none = no project open. */
  project?: string
  /** Image to focus, by its SOURCE uid in the fixture (the copy's uid is looked up), in the page's
   *  selection scope (its ModuleLayout `module`). */
  image?: string
  scope?: string
  route?: string
  /** Shoot the image in the pop-out viewer (opens a headed browser window: WebGPU needs a display).
   *  `plane` = a z-plane with cells; outlines on unless `outlines: false`; `zoom` = wheel steps in at
   *  `at` (fractions of the canvas), so single cells read on the page; `channels` = the ones shown (by
   *  name), every other switched off; `gated` = the gated populations drawn; `mask` = the segmentation
   *  outlined (default `cells`); `tracks` = the segmentations whose tracks are drawn; `time` = the
   *  timepoint (0-based) of a time-lapse; `tail` = how many frames of each track are drawn behind it. */
  viewer?: { plane?: number; outlines?: boolean; gated?: boolean; zoom?: number; at?: [number, number]; channels?: string[];
             mask?: string; tracks?: string[]; time?: number; tail?: number }
  /** The shape its slot needs, width / height (a What's new card's picture is 2): the crop is trimmed
   *  about its centre to it. */
  aspect?: number
  /** Its slot's width in CSS px, when narrower than the set's `column` (one of two side by side). */
  width?: number
  /** A picture's crop pulled in this many CSS px on every side: chrome the canvas runs under (the
   *  viewer's status bar) or the image's own edge. */
  inset?: number
  /** Window height in CSS px when the target is taller than the default 900 (a gate plot). */
  height?: number
  /** What the user does first. A shot shows the state BEFORE the action it illustrates, unless the result is the point. */
  actions?: Action[]
  /** The control the reader acts on, drawn as the pointer with this label. */
  /** No `label` = the ring alone, for a narrow panel where any label would cover its text. */
  point?: { anchor: Anchor; label?: string; side?: Side }
  /** The figure is the union of these boxes (and the pointer's target). */
  crop: Anchor[]
  /** Text blanked before capture: names, ids, times. */
  masks?: Anchor[]
  /** The app's labels the page cites: the capture fails when an anchor's text lacks one. `text` may be a
   *  fixture fact (`{{project.image.fact}}`), so a number on the page is the number in the pixels. */
  expect?: { anchor: Anchor; text: string }[]
  /** The line under the image where a page captions it (the README). Names the sample and its numbers
   *  by fact token (`{{project.image.name}}`), never typed: the caption says what the pixels show. */
  caption?: string
}

/** Where doc-fixture records fixture key → the copy's uids, in the instance's config dir. */
export const FIXTURE_RECORD = 'docshot-fixtures.json'

/** What `pixi run doc-fixture` sets up on the capture instance before any shot: lab images copied
 *  as fresh projects (raw only: no segmentation, gates or tracks; the source is never written) and
 *  profiles. Shots name these by key, never by the copy's random uid. */
export interface Fixture {
  projects?: Record<string, {
    /** Source project uid in the lab's projects dir. */
    source: string
    /** Source image uid → the name the copy shows (no sample ids in the pixels). */
    images: Record<string, string>
    name: string
    /** One Kiwi question about one of these images, asked once on the `default` profile (which uses
     *  the terminal's Claude login) and saved to the Blackboard; asked again only when the copy has
     *  no reply. AI output varies between runs, so shots show this saved reply, never a live turn. */
    kiwi?: { image: string; question: string }
    /** Also copy what segmenting and measuring left (labels, cell table, QC). Never gates. */
    results?: boolean
    /** Source image uid → its channel names, set through the app as a person would. */
    channels?: Record<string, string[]>
    /** Gates recreated through the app's own route (a person's gate, not a ✦ proposal). */
    gates?: FixtureGate[]
  }>
  /** Profiles created on the instance, each owning every fixture project. `ai` turns AI on for it. */
  profiles?: { name: string; ai: boolean }[]
}

/** A gate the fixture recreates on a copy, on segmentation `segmentation` (default `cells`). Either a
 *  box in RAW channel values on logicle axes (`null` = the axis' own end), or `fromSource`: the source
 *  image's own population of that name, its gate or filter as stored, from `gating/<seg>.json` (or
 *  `<seg>__<popType>.json`). Recreated, never copied as a file: a copy carries no one's authorship. */
export type FixtureGate = { image: string; name: string; segmentation?: string } & (
  | { x: string; y: string; xRange: [number | null, number | null]; yRange: [number | null, number | null]; fromSource?: never }
  | { fromSource: true; popType?: 'flow' | 'trackclust' }
)

/** The shots one page shows, and the fixture they run on. */
export interface ShotSet {
  /** Stable id; also where the fixture's copies are recorded. */
  id: string
  /** Where these shots are shown: a page relative to user-docs/ (it links `images/<file>`), or `app`
   *  (What's new cards and tips, lib/docShots.ts). */
  page: string
  /** The colour schemes every shot is captured in: set on the app before each capture and read back,
   *  never inherited from the instance. One image never mixes schemes with another on a page. The
   *  first is the page's default; a README set with both shows the reader's (`<picture>`), and an
   *  `app` set needs every scheme, so the app shows the one its user has. */
  themes: ColourScheme[]
  /** The width its images are shown at, CSS px: a crop wider is scaled down to it (default the GitHub
   *  page column, figure.ts → COLUMN_PX). */
  column?: number
  fixture?: Fixture
  shots: Shot[]
}

/** A shot's capture (and a screenshot's image) in one scheme: `<id>.png` in light, `<id>.<scheme>.png`
 *  otherwise. */
export const shotFile = (id: string, theme: ColourScheme): string => (theme === 'light' ? `${id}.png` : `${id}.${theme}.png`)

/** A picture of the image: a viewer shot, cropped to the canvas. Its pixels are the image's, the same in
 *  every scheme, so it is captured once (the set's first scheme) and composed as one JPEG
 *  (`<id>.jpg`): a lossless 2× microscopy picture is megabytes. */
export const isPicture = (x: Shot): boolean => !!x.viewer
export const shotThemes = (set: ShotSet, x: Shot): ColourScheme[] => (isPicture(x) ? set.themes.slice(0, 1) : set.themes)
/** The composed image a page or the app shows, in one scheme (a picture's one file serves every scheme). */
export const imageFile = (x: Shot, theme: ColourScheme): string => (isPicture(x) ? `${x.id}.jpg` : shotFile(x.id, theme))

export interface ShotIssue { set: string; problem: string }

/** What capture refuses before it starts. Pure, so it is unit-testable. */
export function checkShotSets(sets: ShotSet[]): ShotIssue[] {
  const issues: ShotIssue[] = []
  const ids = new Set<string>(), shotIds = new Set<string>()
  for (const set of sets) {
    const bad = (problem: string) => issues.push({ set: set.id, problem })
    if (ids.has(set.id)) bad('duplicate id')
    ids.add(set.id)
    if (!/^[a-z0-9-]+$/.test(set.id)) bad('id must be kebab-case')
    const fx = set.fixture ?? {}
    if (!set.themes?.length) bad('names no colour scheme (themes): every shot sets one explicitly')
    for (const t of set.themes ?? []) if (!COLOUR_SCHEMES.some(c => c.value === t)) bad(`"${t}" is not a colour scheme`)
    if (new Set(set.themes).size !== set.themes?.length) bad('a colour scheme is listed twice')
    if (set.page === 'app' && COLOUR_SCHEMES.some(c => !set.themes?.includes(c.value)))
      bad('an app set is captured in every colour scheme, so the app shows its user\'s and never mixes them')
    for (const x of set.shots) {
      if (shotIds.has(x.id)) bad(`shot id "${x.id}" is used twice`)
      shotIds.add(x.id)
      if (x.crop.length === 0) bad(`shot "${x.id}" has nothing to crop to`)
      if (x.viewer && (x.crop.join() !== 'css:canvas' || x.point))
        bad(`shot "${x.id}" is a picture of the image: it crops to css:canvas alone and points at nothing, so no app chrome (which has a scheme) is in it`)
      if (x.profile && x.profile !== 'default' && !fx.profiles?.some(p => p.name === x.profile))
        bad(`shot "${x.id}" runs as profile "${x.profile}", which the fixture does not create`)
      if (x.project && !fx.projects?.[x.project]) bad(`shot "${x.id}" opens project "${x.project}", which the fixture does not copy`)
      if (x.image && !(x.project && fx.projects?.[x.project]?.images[x.image]))
        bad(`shot "${x.id}" focuses image "${x.image}", which its fixture project does not copy`)
      for (const a of shotAnchors(x))
        if (!/^(css:.+|nav:\/.*|[a-zA-Z][\w-]*\.[\w.-]+)(@@\d+)?$/.test(a))
          bad(`shot "${x.id}": "${a}" is not an anchor (data-guide id, nav:/route or css:…)`)
      for (const text of [...(x.expect ?? []).map(e => e.text), ...(x.caption ? [x.caption] : [])]) for (const m of text.matchAll(FACT_TOKEN)) {
        const [tok, proj, img, what, stat] = m
        const p = fx.projects?.[proj]
        if (!p?.images[img]) bad(`${tok}: the fixture copies no image "${img}" in project "${proj}"`)
        else if (stat === 'tracks' ? !p.gates?.some(g => g.image === img && g.segmentation === what)
          : stat ? !p.gates?.some(g => g.image === img && gateKey(g) === what) : !IMAGE_FACTS.includes(what))
          bad(`shot "${x.id}": ${tok} is not a fact the fixture measures (${IMAGE_FACTS.join(', ')}, <gate>.count / .pct, or <segmentation>.tracks)`)
      }
    }
  }
  return issues
}

/** `text` with its fact tokens filled from a fixture run's measurements (`<set>.facts.json`, keyed
 *  `project.image.fact`); a token it has no number for is listed in `missing` and printed as "?". */
export function fillFacts(text: string, facts: Record<string, number | string>): { text: string; missing: string[] } {
  const missing: string[] = []
  const out = text.replace(FACT_TOKEN, (tok, ...g: string[]) => {
    const v = facts[g.slice(0, 4).filter(Boolean).join('.')]
    if (v === undefined) { missing.push(tok); return '?' }
    return String(v)
  })
  return { text: out, missing }
}

/** Every anchor a shot uses (actions' `text:` targets are not anchors and are left out). */
export function shotAnchors(x: Shot): Anchor[] {
  const out: Anchor[] = [...x.crop, ...(x.masks ?? []), ...(x.expect ?? []).map(e => e.anchor)]
  if (x.point) out.push(x.point.anchor)
  for (const a of x.actions ?? []) {
    const t = 'click' in a ? a.click : 'fill' in a ? a.fill : 'hover' in a ? a.hover : 'waitFor' in a ? a.waitFor : 'scroll' in a ? a.scroll : 'open' in a ? a.open : null
    if (t && !t.startsWith('text:')) out.push(t)
  }
  return out
}

/** The data-guide ids among `anchors` (no `css:`, no `nav:`, no `@@n`) — what the test looks up in the source. */
export function guideIds(anchors: Anchor[]): string[] {
  return [...new Set(anchors.filter(a => !a.startsWith('css:') && !a.startsWith('nav:')).map(a => a.replace(/@@\d+$/, '')))]
}
