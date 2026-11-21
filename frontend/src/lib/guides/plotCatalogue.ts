// "Which plot answers my question?" — the app's plots as text for a reader that cannot click, served by
// the MCP as get_guide("which-plot") (it rides in mcp/cecelia_mcp/guides.json beside the guides). When a
// user asks Claude "how do I plot X?", this is what Claude matches X against: every registry plot spec
// and every registered view, each with the question it answers (`answers`, required on all three
// registries), the page it lives on, its charts and how it compares.
//
// Pure over its inputs: the specs come from app/src/plotDefinitions (the test hands them in, so the app
// bundle never imports across the tree); the views are INTERACTIVE_VIEWS + CLUSTER_PANELS.

import { NAV_GROUPS, navItemFor } from '../navGroups'
import { moduleRouteFor } from '../../utils/moduleRoute'
import { INTERACTIVE_VIEWS } from '../../components/canvas/interactiveViews'
import { CLUSTER_PANELS } from '../../modules/cluster/clusterPanels'
import type { GuideTextEntry } from './guideText'

/** The fields of a plot spec (app/src/plotDefinitions/*.json) the catalogue reads. */
export interface PlotSpecText {
  id: string
  label: string
  answers?: string
  module?: string
  modules?: Record<string, string[]>
  chartTypes?: string[]
  scopeModes?: string[]
  whiteboardCompatible?: boolean
}

const ANALYSIS = '/analysis'
const pageName = (route: string) => navItemFor(NAV_GROUPS, route)?.label ?? route
const pageRef = (route: string) => `${pageName(route)} (\`${route}\`)`

const CHART_WORDS: Record<string, string> = {
  strip: 'beeswarm', stacked100: '100% stacked', count: 'count', percent: '% positive', elbow: 'fit vs states',
}
const SCOPE_WORDS: Record<string, string> = {
  per_image: 'per image', summarised: 'pooled over the selected images', by_attr: 'by image attribute (e.g. treatment)',
}

/** The pages a spec is on: its own page(s), then the board when it can go there. */
export function specPages(s: PlotSpecText): string[] {
  const keys = s.modules ? Object.keys(s.modules) : s.module ? [s.module] : []
  // a spec's `module` key → its page through THE module-tag map; a key it lacks shows as `?key`, which
  // the catalogue test fails on, so a new Explore page cannot go unlisted
  const pages = keys.map(k => moduleRouteFor(k) ?? `?${k}`)
  return s.whiteboardCompatible ? [...pages, ANALYSIS] : pages
}

function viewPages(v: { modules?: Record<string, string[]>; clusterPage?: boolean; trackPage?: boolean;
                        opticalFlowPage?: boolean; analysisBoard?: boolean },
                   cluster: { trackOnly?: boolean } | null): string[] {
  // the summary pages it is on (`modules`), through the same module-tag map a spec's pages use
  const out: string[] = Object.keys(v.modules ?? {}).map(k => moduleRouteFor(k) ?? `?${k}`)
  if (cluster) out.push(...(cluster.trackOnly ? ['/clust-tracks'] : ['/clust-cells', '/clust-tracks']))
  if (v.clusterPage) out.push('/clust-cells', '/clust-tracks')
  if (v.trackPage) out.push('/track')
  if (v.opticalFlowPage) out.push('/model-training')
  if (v.analysisBoard) out.push(ANALYSIS)
  return [...new Set(out)]
}

const HOW_TO = [
  '## Making one',
  '',
  'On the page: **+ Plot…** → pick the plot → pick the population family at the top of the panel ' +
  '(Gated, Tracked, Track clusters, …) → tick the populations in the rail → pick the measure and the chart → ' +
  'choose how to compare (one image, per image, pooled, or by an image attribute such as treatment). ' +
  'The panel remembers every choice. A plot listed for the Analysis board can also go there, beside plots ' +
  'from other pages, for one exported figure — Claude can lay out such a board with add_analysis_board ' +
  '(the spec ids and board keys below; a view showing a viewer screenshot or a vault model is added in ' +
  'the GUI only, and the tool says so).',
  '',
  'A plot whose task has not run yet shows empty (e.g. no HMM states before the HMM task): say which task ' +
  'makes its data (get_guide has the task guides).',
]

/** The catalogue entry: one section per page, each plot with what it answers. */
export function plotCatalogueEntry(specs: readonly PlotSpecText[]): GuideTextEntry {
  type Row = { title: string; answers: string; detail: string; pages: string[] }
  const rows: Row[] = []
  for (const s of [...specs].sort((a, b) => a.label.localeCompare(b.label))) {
    const charts = (s.chartTypes ?? []).map(c => CHART_WORDS[c] ?? c).join(', ')
    const scopes = (s.scopeModes ?? []).map(m => SCOPE_WORDS[m] ?? m).join('; ')
    rows.push({ title: s.label, answers: s.answers ?? '', pages: specPages(s),
                detail: `Charts: ${charts}. Compare: ${scopes}. Spec id \`${s.id}\`.` })
  }
  // a view goes on a board by its registry key (add_analysis_board takes it as `plot`)
  const boardKey = (key: string, onBoard?: boolean) => onBoard ? ` Board key \`${key}\`.` : ''
  for (const [key, v] of Object.entries(INTERACTIVE_VIEWS))
    rows.push({ title: v.label, answers: v.answers, pages: viewPages(v, null),
                detail: `Interactive view.${boardKey(key, v.analysisBoard)}` })
  for (const [key, p] of Object.entries(CLUSTER_PANELS))
    rows.push({ title: p.label, answers: p.answers, pages: viewPages(p, p),
                detail: `Clustering panel — reads one clustering run (pick the run on the page).${boardKey(key, p.analysisBoard)}` })

  // group under each page's FIRST listing, the board last; each row names every page it is on
  const order = [...new Set(rows.map(r => r.pages[0]))].sort((a, b) =>
    (a === ANALYSIS ? 1 : 0) - (b === ANALYSIS ? 1 : 0) || pageName(a).localeCompare(pageName(b)))
  const lines = ['# Which plot answers my question', '',
                 'Match the user\'s question to an **answers** line; tell them the page and the steps below.', '',
                 ...HOW_TO, '']
  for (const page of order) {
    lines.push(`## ${pageRef(page)}`, '')
    for (const r of rows.filter(x => x.pages[0] === page)) {
      const also = r.pages.slice(1).map(pageName)
      lines.push(`- **${r.title}** — ${r.answers} ${r.detail}${also.length ? ` Also on: ${also.join(', ')}.` : ''}`)
    }
    lines.push('')
  }
  return {
    id: 'which-plot', kind: 'plots', title: 'Which plot answers my question',
    summary: 'Every plot and view, the question it answers, its page and how to make it',
    text: lines.join('\n').trimEnd(),
  }
}
