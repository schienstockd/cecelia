// The shape of a user booklet — one declarative entry per booklet, rendered to a PDF by
// scripts/booklets/build.mjs. Why each part exists and what is free vs fixed: STYLE.md beside this
// file, and docs/todo/BOOKLETS_PLAN.md → *The frame*.
//
// Plain TypeScript with no TS-only runtime syntax (no enums, no namespaces), so Node loads it
// directly (type stripping) — the build needs no bundler. Booklet text that the app also shows is
// IMPORTED from the app's own source (e.g. lib/claudeOverview.ts), never retyped.

/** A: before imaging (training session 2) · B: one recipe end to end · C: using AI. */
export type Series = 'A' | 'B' | 'C'

/** Inline text. `**bold**` marks an app label the reader will look for; `` `code` `` a command. */
export type Text = string

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

/** One screenshot, taken by scripts/booklets/capture.mjs from an ISOLATED instance and composed into the
 *  PDF by the build (crop + pointer drawn on top, so the look can change without recapturing). */
export interface Shot {
  id: string
  /** Project uid in the capture instance; none = no project open. */
  project?: string
  /** Image to focus, in the page's selection scope (its ModuleLayout `module`). */
  image?: string
  scope?: string
  route?: string
  /** What the user does first. A step shows the state BEFORE its action unless the result is the point. */
  actions?: Action[]
  /** The control the reader acts on, drawn as the pointer with this label. */
  point?: { anchor: Anchor; label: string }
  /** The figure is the union of these boxes (and the pointer's target). */
  crop: Anchor[]
  /** Text blanked before capture: names, ids, times. */
  masks?: Anchor[]
  /** The app's labels the caption cites: the capture fails when an anchor's text lacks one. */
  expect?: { anchor: Anchor; text: string }[]
}

/** "Before you image" row: a setting at the microscope, what it limits later, where Cecelia shows it. */
export interface ImagingRow { setting: string; limits: Text; shows: Text }

export interface Booklet {
  /** Stable id; also the PDF name. */
  id: string
  series: Series
  title: string
  /** One line under the title. */
  subtitle?: string
  /** Cover: who it is for, what you get, what you need, where it ends. */
  audience: Text
  get: Text
  need: Text[]
  endsAt: Text
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
    if (b.series === 'B') {
      if (!b.beforeYouImage?.length) bad('B-series booklet has no "Before you image" rows')
      if (b.steps.length === 0) bad('B-series booklet has no steps')
      if (!b.sample) bad('B-series booklet does not name its sample')
    }
    if (b.series !== 'C' && b.askAi === null) bad('only C-series booklets may drop the Ask AI box')
    if (b.steps.length === 0 && !b.intro?.length) bad('no steps and no intro: nothing to print')
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
      for (const a of shotAnchors(x)) {
        if (!/^(css:.+|nav:\/.*|[a-zA-Z][\w-]*\.[\w.-]+)(@@\d+)?$/.test(a))
          issues.push({ booklet: b.id, problem: `shot "${x.id}": "${a}" is not an anchor (data-guide id, nav:/route or css:…)` })
      }
    }
    b.steps.forEach((s, i) => {
      if (s.shot && !own.has(s.shot)) issues.push({ booklet: b.id, problem: `step ${i + 1} names shot "${s.shot}", which this booklet does not define` })
    })
  }
  for (const b of booklets) {
    if (b.next && !ids.has(b.next)) issues.push({ booklet: b.id, problem: `"next" names unknown booklet "${b.next}"` })
  }
  return issues
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
    const t = 'click' in a ? a.click : 'fill' in a ? a.fill : 'hover' in a ? a.hover : 'waitFor' in a ? a.waitFor : null
    if (t && !t.startsWith('text:')) out.push(t)
  }
  return out
}

/** The data-guide ids among `anchors` (no `css:`, no `nav:`, no `@@n`) — what the test looks up in the source. */
export function guideIds(anchors: Anchor[]): string[] {
  return [...new Set(anchors.filter(a => !a.startsWith('css:') && !a.startsWith('nav:')).map(a => a.replace(/@@\d+$/, '')))]
}
