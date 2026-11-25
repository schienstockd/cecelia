// The "+ Plot…" picker's vocabulary: ONE set of categories every plot is filed under, and the schematic
// ("sketch") the picker draws for it — shared by every page's picker and the Analysis board, so a plot
// sits in the same group wherever it is offered (a user who found "Track measures" under "Compare
// groups" on the behaviour page finds it there on the board too).
//
// Categories are framed by the QUESTION a user brings ("compare a measure between treatments"), not by
// how the plot is built (summary vs interactive vs cluster panel) — that split is an implementation
// detail the old optgroups leaked. Every plot declares its category: summary specs in their JSON
// (`app/src/plotDefinitions/*.json` → `category`), views in `INTERACTIVE_VIEWS` / `CLUSTER_PANELS`.
// A plugin's spec without one lands in `other` rather than vanishing.
//
// Pure (no Vue) so the grouping, the sketch fallback and the search are unit-tested.

import { isPlotSketchKind, type PlotSketchKind } from './plotSketchArt'
import { matchesQuery } from '../utils/textQuery'

export const PLOT_CATEGORIES = [
  { id: 'compare',  label: 'Compare groups',        blurb: 'A measure per population, image or treatment' },
  { id: 'time',     label: 'Over time',             blurb: 'How something changes over the movie' },
  { id: 'makeup',   label: 'Make-up',               blurb: 'What fraction falls in each state, class or cluster' },
  { id: 'profiles', label: 'Clusters & matrices',   blurb: 'Cluster maps and markers, transitions, contacts' },
  { id: 'look',     label: 'Images & cards',        blurb: 'See the cells, tracks and gates themselves' },
  { id: 'checks',   label: 'Checks & diagnostics',  blurb: 'Did the segmentation, model or tracking work?' },
  { id: 'other',    label: 'Other',                 blurb: 'Plots a plugin added without a category' },
] as const

export type PlotCategory = typeof PLOT_CATEGORIES[number]['id']
export const PLOT_CATEGORY_IDS: readonly PlotCategory[] = PLOT_CATEGORIES.map(c => c.id)
export const isPlotCategory = (v: unknown): v is PlotCategory =>
  typeof v === 'string' && (PLOT_CATEGORY_IDS as readonly string[]).includes(v)

// What each card draws (the plot's subject) — plotSketchArt.ts owns the vocabulary and the drawings.
export { PLOT_SKETCH_KINDS, isPlotSketchKind, type PlotSketchKind } from './plotSketchArt'

/** One row of a picker: what it adds (`value`, opaque to the picker) and how it is shown. */
export interface PlotOption {
  value: string
  label: string
  answers?: string        // the question it answers — the card's subtitle and what search matches
  category: PlotCategory
  sketch: PlotSketchKind
}

export interface PlotOptionGroup {
  id: PlotCategory
  label: string
  blurb: string
  options: PlotOption[]
}

/** Options grouped in the fixed category order; empty groups dropped, input order kept within a group. */
export function groupPlotOptions(options: readonly PlotOption[]): PlotOptionGroup[] {
  return PLOT_CATEGORIES
    .map(c => ({ id: c.id, label: c.label, blurb: c.blurb, options: options.filter(o => o.category === c.id) }))
    .filter(g => g.options.length > 0)
}

// The words a user types for what the app calls something else. Only entries a user has actually
// reached for belong here — "velocity" is the one that sent someone hunting for Track measures.
const SYNONYMS: Record<string, string[]> = {
  velocity: ['speed'],
  motility: ['speed', 'displacement', 'straightness'],
  persistence: ['straightness'],
  proportion: ['fraction', 'percent', 'split', 'how big', 'size'],
  percentage: ['fraction', 'percent', 'split'],
  abundance: ['count', 'size', 'how big'],
  umap: ['cluster map'],
  embedding: ['cluster map'],
  contact: ['interaction', 'touch'],
  distance: ['spatial'],
  // words the question lines used to carry before they were cut to fit a card — still searchable
  fraction: ['split', 'share'],
  displacement: ['track speed'],
  duration: ['track speed'],
  marker: ['cell measure'],
  shape: ['cell measure', 'shapes'],
  turning: ['cell measure'],
  angle: ['cell measure'],
  neighbourhood: ['contact'],
  screenshot: ['still'],
  behaviour: ['umap', 'tracks split'],
}

/**
 * Does `o` match the search `query`? The house search rule (`utils/textQuery.ts` → `matchesQuery`:
 * every word, case-insensitive, over label + question), applied per word so a word can also be met by
 * one of its synonyms, and filler words ("mean", "between") never hide a match — "mean velocity
 * between treatments" finds Track measures.
 */
export function matchesPlotQuery(o: PlotOption, query: string): boolean {
  const fields = [o.label, o.answers]
  return query.toLowerCase().split(/\s+/).filter(Boolean).every(w =>
    STOP_WORDS.has(w) || [w, ...(SYNONYMS[w] ?? [])].some(t => matchesQuery(fields, t)))
}
const STOP_WORDS = new Set(['mean', 'median', 'average', 'plot', 'graph', 'chart', 'between', 'the', 'of',
  'per', 'by', 'and', 'a', 'in', 'vs', 'compare', 'comparison', 'treatments', 'treatment', 'groups'])

/** A summary spec as a picker row; `value` is what the host's add handler receives. */
export function specOption(
  s: { id: string; label: string; answers?: string; category?: string; sketch?: string },
  value: string = s.id,
): PlotOption {
  return {
    value, label: s.label, answers: s.answers,
    category: isPlotCategory(s.category) ? s.category : 'other',
    // plugin JSON is unvalidated: no sketch, or one this build doesn't know, draws the plain one
    sketch: isPlotSketchKind(s.sketch) ? s.sketch : 'generic',
  }
}
