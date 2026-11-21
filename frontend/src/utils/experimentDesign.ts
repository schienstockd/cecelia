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

import { buildFieldRegex, extractWith, regexSampleFor, type RegexSource } from './regexBuilder'

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

/** One attribute's values: copied from another attribute (a rename), or extracted by a regex. */
export type DesignRule =
  | { attr: string; source: 'attr'; from: string }
  | { attr: string; source: RegexSource; regex: string }

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
 * - An attribute every image already has, with 2+ values that repeat → the group (fewest values).
 * Parents: the subject, nested in the date when subject numbers restart each day. Unit: the image.
 */
export function suggestDesign(images: DesignImage[]): { rules: DesignRule[]; group: string[]; unit: string[]; parents: string[] } {
  const rules: DesignRule[] = []
  const existing = new Set(images.flatMap(i => Object.keys(i.attr ?? {})))
  const free = (name: string) => !existing.has(name)

  // the folder above the file
  let dateAttr = ''
  if (images.length && images.every(i => i.oriPath)) {
    const rule: DesignRule = { attr: 'Folder', source: 'path', regex: buildFieldRegex('/\\', 'secondLast', false) }
    const vals = images.map(i => ruleValue(i, rule))
    if (varies(vals) && !allUnique(vals)) {
      rule.attr = vals.every(v => DATE_RE.test(v)) ? 'Date' : 'Folder'
      if (free(rule.attr)) { rules.push(rule); dateAttr = rule.attr }
    }
  }

  // a leading subject token
  let subjAttr = ''
  const lead = images.map(i => i.name.match(/^([A-Za-z]+)\d+[-_]/)?.[1] ?? '')
  if (images.length && lead.every(l => l && l === lead[0])) {
    const rule: DesignRule = { attr: SUBJECT_NAMES[lead[0].toUpperCase()] ?? 'Subject', source: 'name', regex: `^${escRe(lead[0])}(\\d+)` }
    const vals = images.map(i => ruleValue(i, rule))
    if (vals.every(Boolean) && free(rule.attr)) { rules.push(rule); subjAttr = rule.attr }
  }

  // the group: an attribute already on every image, fewest repeating values
  const candidates = [...existing]
    .map(name => ({ name, vals: images.map(i => (i.attr?.[name] ?? '').trim()) }))
    .filter(c => varies(c.vals) && !allUnique(c.vals))
    .sort((a, b) => distinct(a.vals).size - distinct(b.vals).size || a.name.localeCompare(b.name))
  const group = candidates.length ? [candidates[0].name] : []

  // parents: the subject, inside the date when the same subject number turns up on two dates
  const parents: string[] = []
  if (subjAttr) {
    const vals = applyRules(images, rules)
    const datesOf = new Map<string, Set<string>>()
    for (const img of images) {
      const s = vals[img.uid][subjAttr]
      datesOf.set(s, (datesOf.get(s) ?? new Set()).add(dateAttr ? vals[img.uid][dateAttr] : ''))
    }
    if (dateAttr && [...datesOf.values()].some(d => d.size > 1)) parents.push(dateAttr)
    parents.push(subjAttr)
  }
  return { rules, group, unit: [], parents }
}

// ── What the layout can support ───────────────────────────────────────────────────────────────────

export interface DesignReservation { level: 'warn' | 'info'; short: string; long: string }

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
               long: `A difference between groups cannot be told apart from ${a}.` })
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
  /** the outermost parent's values, one column each ([""] when there are fewer than two parents) */
  cols: string[]
  rows: { group: string; cells: { label: string; n: number }[][]; summary: string }[]
}

/**
 * The grid as the confirm step shows it: a row per group, a column per value of the outermost parent
 * (a date), and in each cell the inner parents with their unit counts ("1: 2 · 2: 3" = mouse 1 has two
 * units, mouse 2 three). With fewer than two parents there is one column.
 */
export function gridPivot(grid: DesignGrid, design: ExperimentDesign): GridPivot {
  const split = design.parents.length >= 2
  const colOf = (parent: string[]) => (split ? parent[0] : '')
  const inner = (parent: string[]) => (split ? parent.slice(1) : parent).join(' ')
  const cols = [...new Set(grid.cells.map(c => colOf(c.parent)))].sort()
  const parentNoun = design.parents.length ? plural(design.parents[design.parents.length - 1]) : ''
  const unitNoun = design.unit.length ? plural(design.unit[design.unit.length - 1]) : 'images'
  const rows = grid.groups.map(g => {
    const key = g.group.join(' ')
    const mine = grid.cells.filter(c => c.group.join(' ') === key)
    return {
      group: key || '—',
      cells: cols.map(col => mine.filter(c => colOf(c.parent) === col).map(c => ({ label: inner(c.parent), n: c.units.length }))),
      summary: [parentNoun && `${g.nParents} ${g.nParents === 1 ? lower(design.parents[design.parents.length - 1]) : parentNoun}`,
                `${g.nUnits} ${unitNoun}`].filter(Boolean).join(' · '),
    }
  })
  return { cols, rows }
}
