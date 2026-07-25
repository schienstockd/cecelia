import { describe, it, expect } from 'vitest'
import {
  styleBlocks, cssRules, scenarioFor, findReimplementedScenarios, findRawValues,
} from './cssScenarios'

describe('styleBlocks', () => {
  it('extracts every style block of an SFC, and passes plain CSS through', () => {
    expect(styleBlocks('<template>x</template><style scoped>.a{}</style>')).toEqual(['.a{}'])
    expect(styleBlocks('<style>.a{}</style><style scoped>.b{}</style>')).toEqual(['.a{}', '.b{}'])
    expect(styleBlocks('.plain { color: red }')).toEqual(['.plain { color: red }'])
  })
})

describe('cssRules', () => {
  it('splits selectors from bodies', () => {
    expect(cssRules('.a { color: red; } .b, .c { top: 0 }')).toEqual([
      { selector: '.a', body: ' color: red; ' },
      { selector: '.b, .c', body: ' top: 0 ' },
    ])
  })

  it('does not let a comment leak into the next selector', () => {
    expect(cssRules('/* note */ .a { top: 0 }').map(r => r.selector)).toEqual(['.a'])
  })

  it('descends into at-rules instead of treating them as one rule', () => {
    expect(cssRules('@media (max-width: 5px) { .a { top: 0 } }').map(r => r.selector)).toEqual(['.a'])
  })
})

describe('scenarioFor', () => {
  const of = (selector: string, body: string) => scenarioFor({ selector, body })

  it('names the canonical utility a rule re-implements', () => {
    expect(of('.x-hint', 'color: var(--cc-text-dim); font-size: 0.7rem;')).toBe('muted')
    expect(of('.x-empty', 'color: var(--cc-text-dim); padding: 1rem;')).toBe('empty')
    expect(of('.x-head', 'text-transform: uppercase; letter-spacing: 0.06em; color: var(--cc-text-dim);'))
      .toBe('eyebrow')
  })

  it('ignores rules that already take colour from a utility', () => {
    expect(of('.x-hint', 'font-size: var(--cc-fs-xs); font-style: italic;')).toBeNull()
    expect(of('.x-empty p', 'margin: 0; font-size: 0.8rem;')).toBeNull()
  })

  // Using the scale tokens is necessary but not sufficient: this rule is still `.cc-muted` longhand.
  it('still flags a re-declaration built from tokens', () => {
    expect(of('.x', 'color: var(--cc-text-dim); font-size: var(--cc-fs-sm);')).toBe('muted')
  })

  // A dim colour + a size also describes every ghost/icon button, whose canonical form is
  // `.cc-btn-ghost` rather than `.cc-muted`. Flagging those would send readers to the wrong utility.
  it('does not flag controls', () => {
    expect(of('.x-btn', 'color: var(--cc-text-dim); font-size: 0.7rem;')).toBeNull()
    expect(of('.x-thing', 'color: var(--cc-text-dim); font-size: 0.7rem; cursor: pointer;')).toBeNull()
    expect(of('.x-thing', 'color: var(--cc-text-dim); font-size: 0.7rem; background: #000;')).toBeNull()
  })
})

// ── The ratchet ───────────────────────────────────────────────────────────────────────────────────
//
// ~130 rules still hand-roll a scenario that `docs/UI.md` has a utility for. Migrating them all at once
// would be a churn diff across 45 files, and the plan explicitly warns off that. But the thing actually
// worth preventing is NEW divergence, and that doesn't require the backlog to be empty first — it
// requires the count to never rise. So: a per-file baseline that may shrink and must never grow.
//
// Touching a file in this list? Migrate its rules and lower the number. Adding a file? Use the utility
// instead. The failure message tells you which.
const BASELINE: Record<string, number> = {
  'components/AppSidebar.vue': 2,
  'components/ClaudeOverviewDialog.vue': 2,
  'components/CohortCheckButton.vue': 1,
  'components/CopyDialog.vue': 1,
  'components/CropDialog.vue': 1,
  'components/CropPanel.vue': 1,
  'components/ErrorConsole.vue': 3,
  'components/FileBrowser.vue': 3,
  'components/ImageMetadataDialog.vue': 3,
  'components/ImageTable.vue': 3,
  'components/LabLogPanel.vue': 5,
  'components/LegacyMigrateDialog.vue': 2,
  'components/ModuleLayout.vue': 5,
  'components/PackagesDialog.vue': 5,
  'components/PhysicalSizeDialog.vue': 1,
  'components/PoolThrottle.vue': 2,
  'components/ProjectPanel.vue': 5,
  'components/SetBar.vue': 2,
  'components/ViewerPanel.vue': 5,
  'components/canvas/InteractivePanel.vue': 1,
  'components/canvas/LayoutCanvas.vue': 3,
  'components/canvas/PlateBuilder.vue': 3,
  'components/canvas/PlotOptions.vue': 1,
  'components/canvas/PopulationManager.vue': 4,
  'components/canvas/SummaryCanvas.vue': 1,
  'components/canvas/SummaryPanel.vue': 3,
  'components/plots/GateMontage.vue': 2,
  'components/plots/GateScatterCell.vue': 3,
  'components/plots/ImageStripView.vue': 2,
  'components/plots/PlotSpinner.vue': 1,
  'components/plots/UmapView.vue': 2,
  'modules/AnimationModule.vue': 6,
  'modules/ChainModule.vue': 11,
  'modules/MoviesModule.vue': 2,
  'modules/SettingsModule.vue': 3,
  'modules/SetupModule.vue': 2,
  'modules/TasksModule.vue': 6,
  'modules/batchmovies/BatchMoviesPanel.vue': 4,
  'modules/cluster/ClusterPlots.vue': 1,
  'modules/gate/GatePlotPanel.vue': 2,
  'modules/gate/GatingPlots.vue': 1,
  'modules/metadata/MetadataPanel.vue': 4,
  'tasks/ParamRenderer.vue': 5,
  'tasks/TaskList.vue': 5,
  'tasks/TaskRunner.vue': 2,
}

const RAW = import.meta.glob('/src/**/*.{vue,css}', {
  query: '?raw', import: 'default', eager: true,
}) as Record<string, string>

describe('hand-rolled UX scenarios', () => {
  it('never increase — see docs/UI.md for the canonical utility', () => {
    const sources = Object.entries(RAW)
      .filter(([path]) => path !== '/src/style.css')          // style.css DEFINES the utilities
      .map(([path, text]) => ({ path: path.replace('/src/', ''), text }))

    expect(sources.length).toBeGreaterThan(100)               // the glob resolved

    const counts: Record<string, number> = {}
    for (const hit of findReimplementedScenarios(sources)) {
      counts[hit.path] = (counts[hit.path] ?? 0) + 1
    }

    const regressions: string[] = []
    const improvements: string[] = []
    for (const path of new Set([...Object.keys(BASELINE), ...Object.keys(counts)])) {
      const was = BASELINE[path] ?? 0
      const now = counts[path] ?? 0
      if (now > was) regressions.push(`${path}: ${was} → ${now} (use the utility, don't re-declare it)`)
      if (now < was) improvements.push(`${path}: ${was} → ${now} (lower the BASELINE entry)`)
    }

    expect(regressions).toEqual([])
    // Improvements fail too, on purpose: an un-updated baseline silently stops ratcheting.
    expect(improvements).toEqual([])
  })
})

describe('raw sizes and radii', () => {
  // Unlike the scenario backlog above, this one is DONE: the ~770 literal font-sizes and radii are all
  // on the scales now, so the bar is an exact list rather than a shrinking count. One documented
  // exception survives — see the inline comment at that line for why it is genuinely off-scale.
  const ALLOWED = [
    'components/ChainQcNode.vue | .qc-bar | border-radius: 1px',
  ]

  it('are always scale tokens', () => {
    const sources = Object.entries(RAW).map(([path, text]) => ({ path: path.replace('/src/', ''), text }))
    const found = findRawValues(sources).map(r => `${r.path} | ${r.selector} | ${r.decl}`)
    expect(found.sort()).toEqual(ALLOWED.sort())
  })
})
