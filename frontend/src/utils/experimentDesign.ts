// The experiment design dialog's logic: the API shapes (`GET/POST /api/sets/experiment-design`), a
// first suggestion from what registered files already carry, and the confirm step's reservations.
// Plan: docs/todo/EXPERIMENT_DESIGN_PLAN.md (Decision 5, Target UX).
//
// The suggestion is a list of RULES, each giving one attribute its values — copied from an attribute
// the images already have, or extracted from the file name / original path with a regex in the
// Metadata panel's format (`regexBuilder.ts`). The user sees and edits the rule, not a guess hidden in
// code, and the same rule placed later files. Nothing here is specific to one technology: the folder
// above a file, a leading subject token (M1-…, D3_…) and existing attributes are where any lab keeps
// this.

import type { SearchGroup } from './searchSelect'
import { buildRegex, extractWith, regexSampleFor, type BuilderSpec, type RegexSource } from './regexBuilder'

export type DesignMode = 'designed' | 'exploratory'
export type DesignStatus = 'inferred' | 'confirmed'
export type DesignExpect = 'unsure' | 'same' | 'higher' | 'lower'

export interface ExperimentDesign {
  question: string
  expect: DesignExpect
  expectIn: string             // the group value for higher / lower
  mode: DesignMode
  status: DesignStatus
  group: string[]
  unit: string[]               // [] = the image is the unit
  parents: string[]            // outermost first
  plannedParents: number | null
  plannedUnits: number | null
}

export interface DesignGrid {
  cells: { group: string[]; parent: string[]; units: { unit: string[]; images: string[] }[] }[]
  groups: { group: string[]; nParents: number; nUnits: number; nImages: number }[]
  unassigned: { uid: string; name: string; missing: string[] }[]
  missingAttrs: string[]
  confounded: string[]
  /** parent levels every group shares ("Date", "Date × Mouse"): paired or blocked */
  paired: string[]
  planned: { parents: number | null; units: number | null }
}

export interface DesignReply {
  design: ExperimentDesign | null
  version: number
  hash: string | null
  grid: DesignGrid | null
  history: { version: number; hash: string | null; at: string; change: string; mode: DesignMode | null; status: DesignStatus | null }[]
}

export const emptyDesign = (): ExperimentDesign => ({
  question: '', expect: 'unsure', expectIn: '', mode: 'designed', status: 'inferred',
  group: [], unit: [], parents: [], plannedParents: null, plannedUnits: null,
})

export interface DesignImage { uid: string; name: string; oriPath?: string | null; attr?: Record<string, string> }

/** One attribute's values: another attribute renamed, or read off the name / path by a regex (with
 *  the builder settings that made it, so the rule reopens as the builder). */
export type DesignRule =
  | { attr: string; source: 'attr'; from: string }
  | { attr: string; source: RegexSource; regex: string; spec?: BuilderSpec }

/** The value `rule` gives `img` ("" = none). */
export function ruleValue(img: DesignImage, rule: DesignRule): string {
  if (rule.source === 'attr') return (img.attr?.[rule.from] ?? '').trim()
  return extractWith(rule.regex, regexSampleFor(img, rule.source)).trim()
}

/** uid → attribute → value for every rule, as the preview route's `attrs` and the values to write. */
export function applyRules(images: DesignImage[], rules: DesignRule[]): Record<string, Record<string, string>> {
  const out: Record<string, Record<string, string>> = {}
  for (const img of images) {
    const a: Record<string, string> = {}
    for (const r of rules) if (r.attr.trim()) a[r.attr.trim()] = ruleValue(img, r)
    out[img.uid] = a
  }
  return out
}

// ── The run suffix and names that repeat ──────────────────────────────────────────────────────────

// An acquisition software's run counter at the end of the name: `…_0004`, `…_0004.oir`.
const RUN_RE = /_(\d{3,})(?:\.[^.]+)?$/

/** The run counter at the end of a file name (`_0004`), or "". */
export function runOf(name: string): string {
  const m = name.match(RUN_RE)
  return m ? `_${m[1]}` : ''
}

/** The name without its run counter and extension. */
export function baseName(name: string): string {
  return name.replace(RUN_RE, '').replace(/\.[^./\\]+$/, '')
}

const folderOf = (img: DesignImage) => (img.oriPath ?? '').replace(/[/\\][^/\\]*$/, '')

/**
 * Names shared by more than one file in one folder once the run counter is dropped — one position imaged several
 * times, or a name not changed between positions. Only the user can say which, so it is a reservation.
 */
export function sameNameGroups(images: DesignImage[]): { name: string; uids: string[]; runs: string[] }[] {
  const by = new Map<string, DesignImage[]>()
  for (const img of images) {
    // within one folder: the same name on another day is another animal, not a repeat
    const k = `${folderOf(img)}\u0000${baseName(img.name)}`
    by.set(k, [...(by.get(k) ?? []), img])
  }
  return [...by.entries()]
    .filter(([, imgs]) => imgs.length > 1)
    .map(([k, imgs]) => ({ name: k.split('\u0000')[1], uids: imgs.map(i => i.uid), runs: imgs.map(i => runOf(i.name)).filter(Boolean) }))
    .sort((a, b) => a.name.localeCompare(b.name))
}

// ── The suggestion ────────────────────────────────────────────────────────────────────────────────

const DATE_RE = /^(\d{6}|\d{8}|\d{4}[-_.]\d{2}[-_.]\d{2})$/
// A leading subject token's letter → the attribute it usually names. Anything else is "Subject".
const SUBJECT_NAMES: Record<string, string> = { M: 'Mouse', D: 'Donor', P: 'Patient', R: 'Rat' }
// Attribute names that already say their role, for a set annotated before its design.
const SUBJECT_ATTR_NAMES = /^(mouse|mice|animal|donor|patient|subject|individual|rat)$/i
const DATE_NAMES = /^(date|day|session|batch|folder)$/i
const INSIDE_NAMES = /^(position|pos|field|fov|region|core|movie|stack)$/i
const escRe = (s: string) => s.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')

const distinct = (vals: string[]) => new Set(vals)
const varies = (vals: string[]) => vals.every(Boolean) && distinct(vals).size >= 2
// Every value on one image only: a name, not a grouping.
const allUnique = (vals: string[]) => distinct(vals).size === vals.length

/**
 * A first design for freshly registered files: rules for the attributes their names and folders
 * carry, and roles guessed from them. Every part is a suggestion the dialog shows as inferred.
 *
 * - The folder above each file (original path), when it varies and repeats → `Date` if every value
 *   reads as a date, else `Folder`.
 * - A leading letter + number shared by every name (`M1-2…`) → `Mouse` (`D` Donor, `P` Patient,
 *   else `Subject`), the number only.
 * - The number after it (`M1-2…` = position 2) → `Position`, inside a replicate.
 * - An attribute every image already has, with 2+ values that repeat → the group (fewest values).
 * Attributes already named Mouse / Date / Position (and their synonyms) take those roles without a rule.
 * The replicate: the subject, numbered per date when subject numbers restart each day.
 */
export function suggestDesign(images: DesignImage[]): { rules: DesignRule[]; group: string[]; unit: string[]; parents: string[] } {
  const rules: DesignRule[] = []
  const existing = new Set(images.flatMap(i => Object.keys(i.attr ?? {})))
  const free = (name: string) => !existing.has(name)
  // an attribute the images already carry, picked by its name (a set designed or annotated before)
  const named = (re: RegExp) => [...existing].sort().find(a => re.test(a.trim())) ?? ''

  // the folder above the file
  let dateAttr = named(DATE_NAMES)
  if (images.length && images.every(i => i.oriPath)) {
    const spec: BuilderSpec = { mode: 'field', sep: 'folder', customSep: '', pos: 'secondLast', stripExt: false }
    const rule: DesignRule = { attr: 'Folder', source: 'path', regex: buildRegex(spec), spec }
    const vals = images.map(i => ruleValue(i, rule))
    if (varies(vals) && !allUnique(vals)) {
      rule.attr = vals.every(v => DATE_RE.test(v)) ? 'Date' : 'Folder'
      if (free(rule.attr) && !dateAttr) { rules.push(rule); dateAttr = rule.attr }
    }
  }

  // a leading subject token, and the position number after it (M1-2… = mouse 1, position 2)
  let subjAttr = named(SUBJECT_ATTR_NAMES)
  let insideAttr = named(INSIDE_NAMES)
  const lead = images.map(i => i.name.match(/^([A-Za-z]+)\d+([-_])/) ?? null)
  if (images.length && lead.every(m => m && m[1] === lead[0]![1])) {
    const letters = lead[0]![1]
    if (!subjAttr) {
      // the digits after the leading letters, anchored at the start (a later "M3" must not match)
      const spec: BuilderSpec = { mode: 'around', before: { text: letters, cls: 'none' }, extract: { kind: 'digits', text: '' }, after: { text: '', cls: 'none' } }
      const rule: DesignRule = { attr: SUBJECT_NAMES[letters.toUpperCase()] ?? 'Subject', source: 'name', regex: `^${escRe(letters)}(\\d+)`, spec }
      if (images.every(i => ruleValue(i, rule)) && free(rule.attr)) { rules.push(rule); subjAttr = rule.attr }
    }
    const sep = lead[0]![2] as '-' | '_'
    if (!insideAttr && lead.every(m => m![2] === sep)) {
      const spec: BuilderSpec = { mode: 'field', sep, customSep: '', pos: 'second', stripExt: true }
      const rule: DesignRule = { attr: 'Position', source: 'name', regex: buildRegex(spec), spec }
      const vals = images.map(i => ruleValue(i, rule))
      if (vals.every(v => /^\d+$/.test(v)) && varies(vals) && free(rule.attr)) { rules.push(rule); insideAttr = rule.attr }
    }
  }

  // the group: an attribute already on every image, fewest repeating values, not one of the above
  const taken = new Set([dateAttr, subjAttr, insideAttr, ...rules.map(r => r.attr)])
  const candidates = [...existing]
    .filter(name => !taken.has(name) && !SUBJECT_ATTR_NAMES.test(name) && !DATE_NAMES.test(name) && !INSIDE_NAMES.test(name))
    .map(name => ({ name, vals: images.map(i => (i.attr?.[name] ?? '').trim()) }))
    .filter(c => varies(c.vals) && !allUnique(c.vals))
    .sort((a, b) => distinct(a.vals).size - distinct(b.vals).size || a.name.localeCompare(b.name))
  const group = candidates.length ? [candidates[0].name] : []

  // the replicate: the subject, numbered per date when the same number turns up on two dates
  const parents: string[] = []
  if (subjAttr) {
    const vals = applyRules(images, rules)
    const v = (img: DesignImage, a: string) => vals[img.uid][a] ?? img.attr?.[a] ?? ''
    const datesOf = new Map<string, Set<string>>()
    for (const img of images) {
      const s = v(img, subjAttr)
      datesOf.set(s, (datesOf.get(s) ?? new Set()).add(dateAttr ? v(img, dateAttr) : ''))
    }
    if (dateAttr && [...datesOf.values()].some(d => d.size > 1)) parents.push(dateAttr)
    parents.push(subjAttr)
  }
  return { rules, group, unit: insideAttr ? [insideAttr] : [], parents }
}

// ── What the layout can support ───────────────────────────────────────────────────────────────────

/** `about`: the attribute it is about, so the grid can point at what shows it. */
export interface DesignReservation { level: 'warn' | 'info'; short: string; long: string; about?: string }

// Fewer parents than this in a group: a test at that level has little power.
const FEW_PARENTS = 5

const lower = (s: string) => s.charAt(0).toLowerCase() + s.slice(1)
// The plural of a parent attribute's name, for "Few mice" (the irregular ones labs name parents by).
const IRREGULAR: Record<string, string> = { mouse: 'mice', person: 'people', child: 'children' }
const plural = (s: string) => IRREGULAR[lower(s)] ?? (/(s|x|ch|sh)$/i.test(s) ? `${lower(s)}es` : `${lower(s)}s`)

/**
 * The confirm step's reservations: what the grid can and cannot support for the question. Advisory —
 * the user confirms either way. Short line = the problem, long = what to do about it.
 */
export function designReservations(
  grid: DesignGrid, design: ExperimentDesign, sameNames: { name: string; uids: string[]; runs: string[] }[] = [],
): DesignReservation[] {
  const out: DesignReservation[] = []
  const groupName = design.group.join(' × ') || 'the group'
  if (design.group.length && grid.groups.length === 1)
    out.push({ level: 'warn', short: `One ${lower(groupName)} only`, long: 'Nothing to compare until a second group is added.' })
  for (const a of grid.confounded)
    out.push({ level: 'warn', short: `${groupName} is confounded with ${a}`,
               long: `A difference between groups cannot be told apart from ${a}.`, about: a })
  const parent = design.parents[design.parents.length - 1]
  if (parent && grid.groups.length >= 2) {
    const ns = grid.groups.map(g => g.nParents)
    if (Math.min(...ns) < FEW_PARENTS)
      out.push({ level: 'warn', short: `Few ${plural(parent)}: ${ns.join(' vs ')}`,
                 long: `At ${lower(parent)} level a test has little power. Finer levels stay descriptive.` })
  }
  // linked replicates: a paired test, or a two-way ANOVA with the replicate as a factor (Lord et al.
  // 2020, doi:10.1083/jcb.202001064; Zweifach 2024, doi:10.1083/jcb.202401074)
  for (const level of grid.paired ?? [])
    out.push({ level: 'info', short: `Paired on ${level}`,
               long: `Use a paired test, or a two-way ANOVA with ${level} as a factor.` })
  for (const a of grid.missingAttrs)
    out.push({ level: 'info', short: `No image has ${a} yet`, long: 'Add it in the rules or the Metadata panel.' })
  if (grid.unassigned.length)
    out.push({ level: 'warn', short: `${grid.unassigned.length} unassigned`, long: 'Not counted in any group until placed.' })
  for (const s of sameNames)
    out.push({ level: 'warn', short: `${s.uids.length} files named ${s.name}${s.runs.length ? ` (${s.runs.join(', ')})` : ''}`,
               long: 'Check whether these are separate positions.' })
  return out
}

// ── The confirm step's table ──────────────────────────────────────────────────────────────────────

export interface GridPivot {
  /** what the table shows, in the design's own words ("Which mice each panel has on each date") */
  caption: string
  /** the attribute the columns are, or "" (fewer than two parents: one column) */
  colAttr: string
  /** the outermost parent's values, one column each ([""] with one column), and their headers */
  cols: string[]
  colLabels: string[]
  rows: { group: string; cells: { label: string; count: string }[][]; summary: string }[]
}

const MONTHS = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']

/** A date-looking value as "25 Nov 2021" (YYYYMMDD or YYYY-MM-DD); anything else as it is. Six-digit
 *  dates are left alone: YYMMDD and DDMMYY cannot be told apart. */
export function dateLabel(v: string): string {
  const m = v.match(/^(\d{4})[-_.]?(\d{2})[-_.]?(\d{2})$/)
  if (!m) return v
  const [y, mo, d] = [Number(m[1]), Number(m[2]), Number(m[3])]
  if (mo < 1 || mo > 12 || d < 1 || d > 31) return v
  return `${d} ${MONTHS[mo - 1]} ${y}`
}

const singular = (s: string) => lower(s)

/**
 * The grid as the confirm step shows it: a row per group, a column per value of the outermost parent
 * (a date), and in each cell the replicates there with how many units each has ("Mouse 1 · 2
 * positions"). With fewer than two parents there is one column.
 */
export function gridPivot(grid: DesignGrid, design: ExperimentDesign): GridPivot {
  const split = design.parents.length >= 2
  const colAttr = split ? design.parents[0] : ''
  const innerAttrs = split ? design.parents.slice(1) : design.parents
  const colOf = (parent: string[]) => (split ? parent[0] : '')
  const inner = (parent: string[]) => {
    const vals = split ? parent.slice(1) : parent
    return innerAttrs.map((a, i) => `${a} ${vals[i]}`).join(' · ')
  }
  const cols = [...new Set(grid.cells.map(c => colOf(c.parent)))].sort()
  const rep = design.parents.at(-1) ?? ''
  const unitAttr = design.unit.at(-1) ?? ''
  const units = (n: number) => `${n} ${n === 1 ? (unitAttr ? singular(unitAttr) : 'image') : (unitAttr ? plural(unitAttr) : 'images')}`
  const reps = (n: number) => `${n} ${n === 1 ? singular(rep) : plural(rep)}`
  const groupNoun = design.group.length ? lower(design.group.join(' × ')) : 'group'
  const caption = rep
    ? `Which ${plural(rep)} each ${groupNoun} has${colAttr ? ` on each ${singular(colAttr)}` : ''}`
    : `How many ${unitAttr ? plural(unitAttr) : 'images'} each ${groupNoun} has`
  const rows = grid.groups.map(g => {
    const key = g.group.join(' ')
    const mine = grid.cells.filter(c => c.group.join(' ') === key)
    return {
      group: key || '—',
      cells: cols.map(col => mine.filter(c => colOf(c.parent) === col)
        .map(c => ({ label: inner(c.parent), count: units(c.units.length) }))),
      summary: [rep && reps(g.nParents), units(g.nUnits)].filter(Boolean).join(' · '),
    }
  })
  return { caption, colAttr, cols, colLabels: cols.map(dateLabel), rows }
}

// ── The set bar's design button ───────────────────────────────────────────────────────────────────

export type DesignState = 'none' | 'exploratory' | 'inferred' | 'confirmed'

/** The design button's state and tooltip for a set's design (`null` = none). */
export function designSummary(d: ExperimentDesign | null): { state: DesignState; tip: string } {
  if (!d) return { state: 'none', tip: 'Add the metadata, then set up the design' }
  if (d.mode === 'exploratory') return { state: 'exploratory', tip: 'Exploratory — no design' }
  return { state: d.status, tip: `Design: ${designRoles(d) || 'no roles'}${d.status === 'inferred' ? ' (not confirmed)' : ''}` }
}

/** A design's roles in words, "Panel in Date × Mouse" — the one phrasing (the Design check, a figure's record). */
export const designRoles = (d: Pick<ExperimentDesign, 'group' | 'parents'>): string =>
  [d.group.join(' × '), d.parents.length ? `in ${d.parents.join(' × ')}` : ''].filter(Boolean).join(' ')

/**
 * The draft the dialog opens with. A new design starts from the suggestion; an existing one from
 * itself; an exploratory set is being promoted, so it opens as designed, keeping any roles it has
 * and filling the rest from the suggestion.
 */
export function designDraft(existing: ExperimentDesign | null,
                            suggestion: { group: string[]; unit: string[]; parents: string[] }): ExperimentDesign {
  if (!existing) return { ...emptyDesign(), group: suggestion.group, unit: suggestion.unit, parents: suggestion.parents }
  if (existing.mode === 'designed') return { ...existing, group: [...existing.group], unit: [...existing.unit], parents: [...existing.parents] }
  const any = existing.group.length + existing.unit.length + existing.parents.length > 0
  const roles = any ? existing : suggestion
  return { ...existing, mode: 'designed', group: [...roles.group], unit: [...roles.unit], parents: [...roles.parents] }
}

/** The history note for saving `next` over `existing`. */
export function designChange(existing: ExperimentDesign | null, next: ExperimentDesign): string {
  if (!existing) return next.mode === 'exploratory' ? 'exploratory' : 'confirmed'
  if (existing.mode === 'exploratory' && next.mode === 'designed') return 'promoted to designed'
  if (existing.mode === 'designed' && next.mode === 'exploratory') return 'made exploratory'
  return 'edited'
}

// ── The dialog's three questions over the stored roles ────────────────────────────────────────────

/** What is compared; what n counts and what names it; what sits inside a replicate. */
export type RoleSlot = 'group' | 'replicate' | 'namedWithin' | 'inside'

/**
 * The design with `picked` as the answer to one question. Stored as group, parents = [...named
 * within, replicate] and unit (inside). An attribute plays one role, so picking it takes it from the
 * others; with no replicate there is nothing to name it within.
 */
export function assignRole(d: ExperimentDesign, slot: RoleSlot, picked: string[]): ExperimentDesign {
  const drop = (xs: string[]) => xs.filter(a => !picked.includes(a))
  const rep0 = d.parents.at(-1) ?? ''
  let group = drop(d.group), unit = drop(d.unit), within = drop(d.parents.slice(0, -1))
  let rep = picked.includes(rep0) ? '' : rep0
  if (slot === 'group') group = [...picked]
  else if (slot === 'inside') unit = [...picked]
  else if (slot === 'replicate') rep = picked[0] ?? ''
  else within = [...picked]
  return { ...d, group, unit, parents: rep ? [...within, rep] : [] }
}

/** The attributes answering one question. */
export function roleOf(d: ExperimentDesign, slot: RoleSlot): string[] {
  const rep = d.parents.at(-1)
  return slot === 'group' ? d.group : slot === 'inside' ? d.unit
    : slot === 'replicate' ? (rep ? [rep] : []) : d.parents.slice(0, -1)
}

// ── Where the design takes effect: plots (docs/todo/EXPERIMENT_DESIGN_PLAN.md → T1, T2) ──────────

/** A design that plots follow: designed, with something compared. Exploratory sets and no design → null. */
const plotted = (d: ExperimentDesign | null | undefined) => d && d.mode === 'designed' && d.group.length ? d : null

/** The compare row the design asks for: by attribute, on its group (two attributes at most). */
export function designCompare(d: ExperimentDesign | null | undefined): { compareAttr: string; compareAttr2: string } | null {
  const p = plotted(d)
  return p ? { compareAttr: p.group[0]!, compareAttr2: p.group[1] ?? '' } : null
}

/** The compare state a canvas keeps, plus whether the design was applied to it once. */
export interface CompareState { compareMode: string; compareAttr: string; compareAttr2: string; designApplied?: boolean }

/**
 * What to write to a canvas's compare state when its set's design arrives, or null for nothing. Once
 * per canvas: a canvas still on the untouched default (per image, no attribute) takes the design's
 * group; one the user set otherwise keeps it. Either way it is marked, so a later switch back to "per
 * image" sticks. Not marked while the design or its attribute is missing (the images have no Panel
 * yet), so it applies when they arrive.
 */
export function designCompareApply(s: CompareState, d: ExperimentDesign | null | undefined,
                                   attrNames: string[]): Partial<CompareState> | null {
  if (s.designApplied) return null
  const c = designCompare(d)
  if (!c || !attrNames.includes(c.compareAttr)) return null
  if (s.compareMode !== 'per_image' || s.compareAttr) return { designApplied: true }
  return { compareMode: 'by_attr', compareAttr: c.compareAttr,
           compareAttr2: c.compareAttr2 && attrNames.includes(c.compareAttr2) ? c.compareAttr2 : '', designApplied: true }
}

/** Whether the compare row shows the design's group (the row marks it). */
export const followsDesign = (d: ExperimentDesign | null | undefined, compareMode: string, attrs: string[]) => {
  const c = designCompare(d)
  return !!c && compareMode === 'by_attr' && attrs.join('\u0000') === [c.compareAttr, c.compareAttr2].filter(Boolean).join('\u0000')
}

/** The attributes that name a replicate (the design's parents, outermost first); [] = no replicate. */
export const designReplicateAttrs = (d: ExperimentDesign | null | undefined): string[] =>
  plotted(d)?.parents.length ? [...d!.parents] : []

/** The replicate's noun, singular and plural ("mouse" / "mice"), for "per mouse" and "n = 5 mice". */
export function replicateNoun(attrs: string[]): { one: string; many: string } | null {
  const a = attrs[attrs.length - 1]
  return a ? { one: singular(a), many: plural(a) } : null
}

/**
 * The compare row's bullseye: shown whenever the set's design can be plotted. `follows` = the row
 * shows the design's group; `differs` = the user compares something else (one click switches back);
 * null = no design here, or its attribute is on none of these images.
 */
export function designMark(d: ExperimentDesign | null | undefined, compareMode: string, attrs: string[],
                           attrNames: string[]): { state: 'follows' | 'differs'; tip: string } | null {
  const c = designCompare(d)
  if (!c || !attrNames.includes(c.compareAttr)) return null
  if (followsDesign(d, compareMode, attrs)) return { state: 'follows', tip: designPlotTip(d) }
  return { state: 'differs', tip: `The design compares ${[c.compareAttr, c.compareAttr2].filter(Boolean).join(' × ')} — click to use it` }
}

/** One line for the compare row's mark: what the design set here. */
export function designPlotTip(d: ExperimentDesign | null | undefined): string {
  const c = designCompare(d)
  if (!c) return ''
  const n = replicateNoun(designReplicateAttrs(d))
  return `From the design: ${[c.compareAttr, c.compareAttr2].filter(Boolean).join(' × ')}${n ? `, n = ${n.many}` : ''}`
}

// ── The acquisition check reads the question from here (T5) ──────────────────────────────────────

/** The expectation as words: "the same", "higher in A"; "" when unsure. Mirrors `design_expect_phrase`. */
export function designExpectPhrase(d: Pick<ExperimentDesign, 'expect' | 'expectIn'> | null | undefined): string {
  if (!d) return ''
  if (d.expect === 'same') return 'the same'
  if ((d.expect === 'higher' || d.expect === 'lower') && d.expectIn) return `${d.expect} in ${d.expectIn}`
  return ''
}

/** The question and expectation, the part of a design the acquisition plan also asks. */
export type DesignQuestion = Pick<ExperimentDesign, 'question' | 'expect' | 'expectIn'>

/** The design with `q` written in: an existing one keeps everything else; with none, a new inferred
 *  design holding only the question (the roles come later, from the Design check). */
export function withQuestion(existing: ExperimentDesign | null, q: DesignQuestion): ExperimentDesign {
  const expectIn = q.expect === 'higher' || q.expect === 'lower' ? q.expectIn : ''
  return { ...(existing ?? emptyDesign()), question: q.question.trim(), expect: q.expect, expectIn }
}

/** Whether `q` says something other than the design already does. */
export const questionChanged = (existing: ExperimentDesign | null, q: DesignQuestion) => {
  const n = withQuestion(existing, q), o = existing ?? emptyDesign()
  return n.question !== o.question || n.expect !== o.expect || n.expectIn !== o.expectIn
}

// ── Step 2 as one row per attribute, with a sketch of what the roles mean ─────────────────────────

/** An attribute's role in the design; '' = not part of it. */
export type AttrRole = RoleSlot | ''

export function attrRole(d: ExperimentDesign, a: string): AttrRole {
  for (const s of ['group', 'replicate', 'namedWithin', 'inside'] as const) if (roleOf(d, s).includes(a)) return s
  return ''
}

/** The design with `a` in `role` (one role per attribute; the n is one attribute, so picking another
 *  replaces it). '' takes it out of the design. */
export function setAttrRole(d: ExperimentDesign, a: string, role: AttrRole): ExperimentDesign {
  if (!role) {
    const now = attrRole(d, a)
    return now ? assignRole(d, now, roleOf(d, now).filter(x => x !== a)) : d
  }
  return assignRole(d, role, role === 'replicate' ? [a] : [...roleOf(d, role).filter(x => x !== a), a])
}

/** Step 2's rows and its "+ Attribute" picker. A row per attribute in the design, plus any the user added
 *  or touched (so clearing a role doesn't pull the row out from under the click); every other attribute
 *  waits in the picker, the ones that vary over the set first: one value on every image can be neither
 *  compared nor tell replicates apart. `values` = each attribute's distinct values over the set. */
export function designAttrRows(attrs: string[], d: ExperimentDesign, values: Record<string, number>, added: readonly string[]) {
  const rows = attrs.filter(a => attrRole(d, a) || added.includes(a))
  const rest = attrs.filter(a => !rows.includes(a))
  const vary = rest.filter(a => (values[a] ?? 0) > 1), same = rest.filter(a => (values[a] ?? 0) <= 1)
  const picker: SearchGroup[] = [
    ...(vary.length ? [{ title: 'Varies across the set', options: vary.map(value => ({ value })) }] : []),
    ...(same.length ? [{ title: 'Same on every image', options: same.map(value => ({ value })) }] : []),
  ]
  return { rows, picker }
}

/** What the sketch draws: per compared group, one mark per replicate (the n) with how many things sit
 *  inside it. No replicate → each image is the n, nothing inside. Capped per group (`more` = hidden). */
export interface DesignSketch {
  groups: { label: string; reps: { label: string; inside: number }[]; more: number }[]
  repNoun: { one: string; many: string }
  insideNoun: string
  caption: string
}

export function designSketch(grid: DesignGrid, d: ExperimentDesign, cap = 10): DesignSketch | null {
  if (!d.group.length || !grid.cells.length) return null
  const rep = d.parents.at(-1)
  const repNoun = replicateNoun(d.parents) ?? { one: 'image', many: 'images' }
  const insideAttr = d.unit.at(-1)
  const insideNoun = rep ? (insideAttr ? plural(insideAttr) : 'images') : ''
  const byGroup = new Map<string, { label: string; inside: number }[]>()
  for (const c of grid.cells) {
    const g = c.group.join(' · ')
    const list = byGroup.get(g) ?? []
    if (rep) list.push({ label: c.parent.join(' · '), inside: c.units.length })
    else for (const u of c.units) list.push({ label: u.unit.join(' · ') || (u.images[0] ?? ''), inside: 0 })
    byGroup.set(g, list)
  }
  const groups = [...byGroup.entries()].sort(([a], [b]) => a.localeCompare(b, undefined, { numeric: true }))
    .map(([label, reps]) => ({ label, reps: reps.slice(0, cap), more: Math.max(0, reps.length - cap) }))
  const ns = groups.map(g => g.reps.length + g.more)
  const caption = `${d.group.join(' × ')}: ${groups.map(g => g.label).join(' vs ')} · n = ${ns.join(' vs ')} ${repNoun.many}`
  return { groups, repNoun, insideNoun, caption }
}
