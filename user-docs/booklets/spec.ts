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
