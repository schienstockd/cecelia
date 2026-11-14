// The shape of a user booklet — one declarative entry per booklet, rendered to a PDF by
// scripts/booklets/build.mjs. Why each part exists and what is free vs fixed: STYLE.md beside this
// file, and docs/todo/BOOKLETS_PLAN.md → *The frame*.
//
// Plain TypeScript with no TS-only runtime syntax (no enums, no namespaces), so Node loads it
// directly (type stripping) — the build needs no bundler. Booklet text that the app also shows is
// IMPORTED from the app's own source (e.g. lib/claudeOverview.ts), never retyped.

import type { Side } from '../../frontend/src/utils/anchorPosition.ts'
import { isGuideStep, isWanted, recipeById, type RecipeStep } from '../../frontend/src/lib/guides/recipes.ts'

/** A: before imaging (training session 2) · B: one recipe, as a showcase · C: using AI. */
export type Series = 'A' | 'B' | 'C'

/** guide: the reader follows steps (A, C). showcase: what a workflow gives, then the in-app guide for
 *  the doing (B). Never both (plan → D6). */
export type Kind = 'guide' | 'showcase'

/** A showcase's body. Outcome first; the doing is the recipe's in-app guide, never steps here. */
export interface Showcase {
  /** The result, shown right under the cover, with one line on how you get there. A `todo` in place of
   *  a shot prints as a marked placeholder saying what is still needed (work in progress); the build
   *  lists every one, and `--strict` refuses them. */
  outcome: { shot?: string; todo?: Text; text: Text }
  /** Rough time at the workstation, e.g. "About an hour for one image". */
  time: Text
  /** Three or four things the workflow gives, one shot each. `step` = the recipe step it shows (its
   *  guide id, or a page step's label): the build prints that step's own why-line, so the booklet and
   *  the in-app guide cannot drift, and `text` adds only what the recipe cannot say (what you get). */
  shows: { title: string; step?: string; text?: Text; shot?: string; todo?: Text }[]
  /** What it does not do, or where a person must judge. */
  limits: Text[]
  /** `recipes.ts` id: **Do it** names that recipe's in-app guide. */
  recipe: string
}

/** Inline text. `**bold**` marks an app label the reader will look for; `` `code` `` a command.
 *  `{{project.image.fact}}` is a number the fixture run measured (`FACTS`), never a typed one. */
export type Text = string

/** The numbers a caption may cite, measured by booklet-fixture on the copy and printed by the build:
 *  `{{<project>.<image>.planes}}`, `.channels`, and per fixture gate `.<gate>.count` / `.<gate>.pct`.
 *  A count typed into the text instead is refused (`HAND_COUNT`): it goes stale against the fixture. */
export const FACT_TOKEN = /\{\{([\w-]+)\.([\w-]+)\.([\w-]+)(?:\.(count|pct))?\}\}/g
const HAND_COUNT = /\b\d[\d,.]*\s*(?:%|z-planes?|planes?|channels?|cells?|images?|timepoints?)(?!\w)/i

export type Block =
  | { kind: 'para'; text: Text }
  | { kind: 'list'; items: Text[] }
  /** Side-by-side cards, e.g. the two ways in. `tone` picks the app's colour for that tool. */
  | { kind: 'cards'; cards: { title: string; tone?: 'kiwi' | 'claude' | 'neutral'; lead?: Text; lines: Text[]; foot?: Text }[] }
  /** One-line callout. `warn` is for what the reader must not miss (data leaving the machine). */
  | { kind: 'note'; tone: 'info' | 'warn'; text: Text }
  | { kind: 'table'; head: string[]; rows: Text[][] }
  /** Words this booklet uses that a newcomer won't know, defined where they first appear (STYLE.md). */
  | { kind: 'terms'; terms: { term: string; means: Text }[] }

/** One step: Do → You'll see → If it goes wrong, plus Why for a non-obvious setting. */
export interface Step {
  title: string
  do: Text
  see: Text
  ifWrong: Text
  why?: Text
  /** Past the booklet's end point: worth doing, not needed to get there. Printed beside the title. */
  optional?: boolean
  /** Screenshot id from the capture manifest (P2). A step without one renders text only. */
  shot?: string
  /** Free blocks under the step (a bad example, a table). */
  more?: Block[]
}

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

/** One screenshot, taken by scripts/booklets/capture.mjs from an ISOLATED instance and composed into the
 *  PDF by the build (crop + pointer drawn on top, so the look can change without recapturing). */
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
   *  `at` (fractions of the canvas), so single cells read in print; `channels` = the ones shown (by
   *  name), every other switched off; `gated` = the gated populations drawn. */
  viewer?: { plane?: number; outlines?: boolean; gated?: boolean; zoom?: number; at?: [number, number]; channels?: string[] }
  /** Window height in CSS px when the target is taller than the default 900 (a gate plot). */
  height?: number
  /** What the user does first. A step shows the state BEFORE its action unless the result is the point. */
  actions?: Action[]
  /** The control the reader acts on, drawn as the pointer with this label. */
  /** No `label` = the ring alone, for a narrow panel where any label would cover its text. */
  point?: { anchor: Anchor; label?: string; side?: Side }
  /** The figure is the union of these boxes (and the pointer's target). */
  crop: Anchor[]
  /** Text blanked before capture: names, ids, times. */
  masks?: Anchor[]
  /** The app's labels the caption cites: the capture fails when an anchor's text lacks one. */
  expect?: { anchor: Anchor; text: string }[]
}

/** Where booklet-fixture records fixture key → the copy's uids, in the instance's config dir. */
export const FIXTURE_RECORD = 'booklet-fixtures.json'

/** What `pixi run booklet-fixture` sets up on the capture instance before any shot: lab images copied
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
    /** Gates recreated through the app's own gate route (a person's gate, not a ✦ proposal), each a
     *  box in RAW channel values on logicle axes; `null` = the axis' own end. */
    gates?: { image: string; name: string; x: string; y: string; xRange: [number | null, number | null]; yRange: [number | null, number | null] }[]
  }>
  /** Profiles created on the instance, each owning every fixture project. `ai` turns AI on for it. */
  profiles?: { name: string; ai: boolean }[]
}

/** "Before you image" row: a setting at the microscope, what it limits later, where Cecelia shows it. */
export interface ImagingRow { setting: string; limits: Text; shows: Text }

export interface Booklet {
  /** Stable id; also the PDF name. */
  id: string
  series: Series
  /** Default `guide`. */
  kind?: Kind
  showcase?: Showcase
  title: string
  /** One line under the title. */
  subtitle?: string
  /** Cover: who it is for, what you get, what you need, where it ends. */
  audience: Text
  get: Text
  need: Text[]
  /** Where a guide ends. A showcase has `showcase.time` instead. */
  endsAt?: Text
  /** The tissue / sample in one line (B-series). */
  sample?: Text
  beforeYouImage?: ImagingRow[]
  /** Free blocks before the steps (C-series one-pagers are mostly this). */
  intro?: Block[]
  steps: Step[]
  /** The "Ask AI" box; `null` only where AI is the subject (C-series has it inline). */
  askAi: { prompt: Text; where: Text } | null
  /** Booklet id that follows this one; rendered as its generated number + title. */
  next?: string
  /** Screenshots its steps name by id. */
  shots?: Shot[]
  fixture?: Fixture
}

export interface BuildIssue { booklet: string; problem: string }

/** Frame rules the build enforces (STYLE.md → *Fixed*). Pure, so it is unit-testable. */
export function checkBooklets(booklets: Booklet[]): BuildIssue[] {
  const issues: BuildIssue[] = []
  const ids = new Set<string>()
  for (const b of booklets) {
    const bad = (problem: string) => issues.push({ booklet: b.id, problem })
    if (ids.has(b.id)) bad('duplicate id')
    ids.add(b.id)
    if (!/^[a-z0-9-]+$/.test(b.id)) bad('id must be kebab-case')
    if (b.need.length === 0) bad('"need" is empty — say "nothing" explicitly')
    const kind = b.kind ?? 'guide'
    if (b.series === 'B') {
      if (!b.beforeYouImage?.length) bad('B-series booklet has no "Before you image" rows')
      if (kind !== 'showcase') bad('B-series booklets are showcases (plan → D6)')
      if (!b.sample) bad('B-series booklet does not name its sample')
    }
    if (kind === 'showcase') {
      const sc = b.showcase
      if (!sc) bad('showcase has no "showcase" body')
      else {
        if (b.steps.length) bad('a showcase has no steps: the doing is the in-app guide')
        if (sc.shows.length < 3 || sc.shows.length > 4) bad(`a showcase shows 3 or 4 things, not ${sc.shows.length}`)
        if (!sc.limits.length) bad('a showcase names its limits')
        if (!recipeTitle(sc.recipe)) bad(`showcase names unknown recipe "${sc.recipe}"`)
        if (!sc.outcome.shot && !sc.outcome.todo) bad('the outcome needs a shot, or a todo saying what is missing')
        sc.shows.forEach((x, i) => {
          if (x.step && sc.recipe && recipeTitle(sc.recipe) && !recipeStep(sc.recipe, x.step))
            bad(`show ${i + 1} names step "${x.step}", which recipe "${sc.recipe}" does not have`)
          if (!x.step && !x.text?.trim()) bad(`show ${i + 1} has neither a recipe step nor text`)
        })
      }
    } else if (b.showcase) bad('a guide has no "showcase" body')
    if (b.series !== 'C' && b.askAi === null) bad('only C-series booklets may drop the Ask AI box')
    if (kind === 'guide' && b.steps.length === 0 && !b.intro?.length) bad('no steps and no intro: nothing to print')
    if (kind === 'guide' && !b.endsAt?.trim()) bad('a guide says where it ends')
    for (const t of factTexts(b)) {
      const typed = t.replace(FACT_TOKEN, '').match(HAND_COUNT)
      if (typed) bad(`"${typed[0]}" is typed by hand: cite the fixture's number ({{project.image.fact}})`)
      for (const m of t.matchAll(FACT_TOKEN)) {
        const [tok, proj, img, what, stat] = m
        const p = b.fixture?.projects?.[proj]
        if (!p?.images[img]) bad(`${tok}: the fixture copies no image "${img}" in project "${proj}"`)
        else if (stat ? !p.gates?.some(g => g.image === img && g.name === what) : !['planes', 'channels'].includes(what))
          bad(`${tok}: not a fact the fixture measures (planes, channels, or <gate>.count / .pct)`)
      }
    }
    b.steps.forEach((s, i) => {
      for (const k of ['title', 'do', 'see', 'ifWrong'] as const) if (!s[k]?.trim()) bad(`step ${i + 1}: "${k}" is empty`)
    })
  }
  const shotIds = new Set<string>()
  for (const b of booklets) {
    const own = new Set((b.shots ?? []).map(x => x.id))
    for (const x of b.shots ?? []) {
      if (shotIds.has(x.id)) issues.push({ booklet: b.id, problem: `shot id "${x.id}" is used twice` })
      shotIds.add(x.id)
      if (x.crop.length === 0) issues.push({ booklet: b.id, problem: `shot "${x.id}" has nothing to crop to` })
      const fx = b.fixture ?? {}
      if (x.profile && x.profile !== 'default' && !fx.profiles?.some(p => p.name === x.profile))
        issues.push({ booklet: b.id, problem: `shot "${x.id}" runs as profile "${x.profile}", which the fixture does not create` })
      if (x.project && !fx.projects?.[x.project])
        issues.push({ booklet: b.id, problem: `shot "${x.id}" opens project "${x.project}", which the fixture does not copy` })
      if (x.image && !(x.project && fx.projects?.[x.project]?.images[x.image]))
        issues.push({ booklet: b.id, problem: `shot "${x.id}" focuses image "${x.image}", which its fixture project does not copy` })
      for (const a of shotAnchors(x)) {
        if (!/^(css:.+|nav:\/.*|[a-zA-Z][\w-]*\.[\w.-]+)(@@\d+)?$/.test(a))
          issues.push({ booklet: b.id, problem: `shot "${x.id}": "${a}" is not an anchor (data-guide id, nav:/route or css:…)` })
      }
    }
    b.steps.forEach((s, i) => {
      if (s.shot && !own.has(s.shot)) issues.push({ booklet: b.id, problem: `step ${i + 1} names shot "${s.shot}", which this booklet does not define` })
    })
    for (const id of b.showcase ? namedShots({ ...b, steps: [] }) : [])
      if (!own.has(id)) issues.push({ booklet: b.id, problem: `showcase names shot "${id}", which this booklet does not define` })
  }
  for (const b of booklets) {
    if (b.next && !ids.has(b.next)) issues.push({ booklet: b.id, problem: `"next" names unknown booklet "${b.next}"` })
  }
  return issues
}

/** `text` with its fact tokens filled from a fixture run's measurements (`<booklet>.facts.json`, keyed
 *  `project.image.fact`); a token it has no number for is listed in `missing` and printed as "?". */
export function fillFacts(text: Text, facts: Record<string, number | string>): { text: Text; missing: string[] } {
  const missing: string[] = []
  const out = text.replace(FACT_TOKEN, (tok, ...g: string[]) => {
    const v = facts[g.slice(0, 4).filter(Boolean).join('.')]
    if (v === undefined) { missing.push(tok); return '?' }
    return String(v)
  })
  return { text: out, missing }
}

/** The text a booklet prints in which a count could be cited: what FACT_TOKEN and HAND_COUNT read. */
function factTexts(b: Booklet): Text[] {
  const sc = b.showcase
  return [b.sample ?? '', b.get, ...b.need, ...(sc ? [sc.outcome.text, sc.time, ...sc.shows.map(x => x.text ?? ''), ...sc.limits] : []),
          ...b.steps.flatMap(s => [s.do, s.see])].filter(Boolean)
}

/** A written recipe's step by its guide id or, for a page step, its label. */
export function recipeStep(recipe: string, step: string): RecipeStep | undefined {
  const r = recipeById(recipe)
  return r && !isWanted(r) ? r.steps.find(s => (isGuideStep(s) ? s.guide : s.label) === step) : undefined
}

/** A written recipe's title (what the in-app Guides dialog lists), or '' for an unknown or wanted one. */
export function recipeTitle(id: string): string {
  const r = recipeById(id)
  return r && !isWanted(r) ? r.title : ''
}

/** Every shot a booklet names: its steps' and its showcase's. */
export function namedShots(b: Booklet): string[] {
  return [...b.steps.flatMap(s => (s.shot ? [s.shot] : [])),
          ...(b.showcase ? [b.showcase.outcome, ...b.showcase.shows].flatMap(x => (x.shot ? [x.shot] : [])) : [])]
}

/** Generated booklet numbers ("C1"), in registry order within each series. Never hand-typed. */
export function bookletNumbers(booklets: Booklet[]): Map<string, string> {
  const count: Record<string, number> = {}
  const out = new Map<string, string>()
  for (const b of booklets) {
    count[b.series] = (count[b.series] ?? 0) + 1
    out.set(b.id, `${b.series}${count[b.series]}`)
  }
  return out
}

/** Every anchor a shot uses (actions' `text:` targets are not anchors and are left out). */
export function shotAnchors(x: Shot): Anchor[] {
  const out: Anchor[] = [...x.crop, ...(x.masks ?? []), ...(x.expect ?? []).map(e => e.anchor)]
  if (x.point) out.push(x.point.anchor)
  for (const a of x.actions ?? []) {
    const t = 'click' in a ? a.click : 'fill' in a ? a.fill : 'hover' in a ? a.hover : 'waitFor' in a ? a.waitFor : 'scroll' in a ? a.scroll : null
    if (t && !t.startsWith('text:')) out.push(t)
  }
  return out
}

/** The data-guide ids among `anchors` (no `css:`, no `nav:`, no `@@n`) — what the test looks up in the source. */
export function guideIds(anchors: Anchor[]): string[] {
  return [...new Set(anchors.filter(a => !a.startsWith('css:') && !a.startsWith('nav:')).map(a => a.replace(/@@\d+$/, '')))]
}
