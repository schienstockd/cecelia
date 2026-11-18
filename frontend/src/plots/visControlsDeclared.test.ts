import { describe, it, expect } from 'vitest'
import { INTERACTIVE_VIEWS } from '../components/canvas/interactiveViews'
import { CLUSTER_PANELS } from '../modules/cluster/clusterPanels'
import { VIS_KEYS, visControlsFor, type VisControl } from './plotControls'

// The interactive views and cluster panels are not built by `buildPlotOptions`, so plotControls.test.ts
// cannot flip their settings. They DECLARE which rail rows they react to (`visControls` on their registry
// entry) and this pins the declaration against the source, both ways:
//   • a VisProps key the view reads must belong to a declared control — else the rail hides a live setting;
//   • a declared control must be read — else the rail shows a toggle that does nothing (the original bug).
// Read as source: a view reads its bag as `vis.x` / `vis?.x` / `props.vis?.x`, through a local alias
// (`const v = computed<VisProps>(() => props.vis ?? …)` then `const o = v.value`), or hands the whole bag
// to a helper that reads a known key (`paletteRange` → palette, `facetMode` → facetBy).

const SFC = import.meta.glob(['/src/components/plots/*.vue', '/src/modules/cluster/*.vue'], {
  query: '?raw', import: 'default', eager: true }) as Record<string, string>
const REGISTRY_SRC = import.meta.glob(
  ['/src/components/canvas/interactiveViews.ts', '/src/modules/cluster/clusterPanels.ts'], {
    query: '?raw', import: 'default', eager: true }) as Record<string, string>

// component identifier → its .vue path, from each registry's import lines
const fileOf: Record<string, string> = {}
for (const [regPath, src] of Object.entries(REGISTRY_SRC)) {
  const dir = regPath.slice(0, regPath.lastIndexOf('/'))
  for (const m of src.matchAll(/^import (\w+) from '(\.[^']+\.vue)'/gm)) {
    const parts = `${dir}/${m[2]}`.split('/'), out: string[] = []
    for (const p of parts) p === '..' ? out.pop() : p !== '.' && out.push(p)
    fileOf[m[1]] = out.join('/')
  }
}
// registry key → component identifier, from `component: X` in each entry's source
const componentOf = (src: string, key: string): string | undefined =>
  new RegExp(`\\b${key}: \\{[\\s\\S]*?component: (\\w+)`).exec(src)?.[1]

const KEY_TO_CONTROL = new Map<string, VisControl>(
  (Object.entries(VIS_KEYS) as [VisControl, string[]][]).flatMap(([c, ks]) => ks.map(k => [k, c] as const)))
const HELPERS: [RegExp, VisControl][] = [[/\bpaletteRange\(/, 'palette'], [/\bfacetMode\(/, 'facetBy']]

export function controlsRead(src: string): Set<VisControl> {
  const aliases = new Set(['vis'])
  for (const m of src.matchAll(/const (\w+)\s*=\s*computed(?:<VisProps>)?\(\(\) => props\.vis\b/g)) aliases.add(m[1])
  for (const m of src.matchAll(/const (\w+)\s*=\s*(\w+)\.value\b/g)) if (aliases.has(m[2])) aliases.add(m[1])
  const out = new Set<VisControl>()
  const alt = [...aliases].join('|')
  for (const m of src.matchAll(new RegExp(`\\b(?:${alt})(?:\\.value)?\\??\\.(\\w+)`, 'g'))) {
    const c = KEY_TO_CONTROL.get(m[1]); if (c) out.add(c)
  }
  // a helper only counts when it is handed the bag (an alias), not some other object
  for (const [re, c] of HELPERS)
    if (new RegExp(`${re.source}\\s*(?:props\\.)?(?:${alt})\\b`).test(src)) out.add(c)
  return out
}

const sorted = (xs: Iterable<VisControl>) => [...new Set(xs)].sort()

const ENTRIES: { reg: string; key: string; declared: VisControl[] }[] = [
  ...Object.entries(INTERACTIVE_VIEWS).map(([key, d]) =>
    ({ reg: '/src/components/canvas/interactiveViews.ts', key, declared: d.visControls ?? [] })),
  ...Object.entries(CLUSTER_PANELS).map(([key, d]) =>
    ({ reg: '/src/modules/cluster/clusterPanels.ts', key, declared: d.visControls ?? [] })),
]

describe('visControls — each registered plot declares exactly the rail settings it reads', () => {
  it.each(ENTRIES.filter(e => e.key !== 'heatmap'))('$key', ({ reg, key, declared }) => {
    const comp = componentOf(REGISTRY_SRC[reg], key)
    expect(comp, `no component: for ${key}`).toBeTruthy()
    const src = SFC[fileOf[comp!]]
    expect(src, `no source for ${comp}`).toBeTruthy()
    expect(sorted(declared)).toEqual(sorted(controlsRead(src)))
  })

  // the cluster heatmap hands its whole bag to `buildPlotOptions` as a heatmap — the renderer's own table
  // (pinned by plotControls.test.ts) is its declaration
  it('the cluster heatmap declares the heatmap chart\'s controls', () => {
    expect(CLUSTER_PANELS.heatmap.visControls).toEqual(visControlsFor({ chartType: 'heatmap' }))
    const src = SFC[fileOf[componentOf(REGISTRY_SRC['/src/modules/cluster/clusterPanels.ts'], 'heatmap')!]]
    expect(src).toMatch(/\.\.\.\(props\.vis \?\? \{\}\), chartType: 'heatmap'/)
  })
})

describe('controlsRead — the scan itself', () => {
  it('follows aliases and helpers, and ignores other objects', () => {
    const src = `const v = computed<VisProps>(() => props.vis ?? DEFAULT_VIS)
      const o = v.value
      x(o.legend, o.fontSize, props.vis?.pointSize, entry.title)
      paletteRange(o, 3); facetMode(other)`
    expect(sorted(controlsRead(src))).toEqual(['fontSize', 'legend', 'palette', 'pointSize'])
  })
})
