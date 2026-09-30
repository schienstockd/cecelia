import { describe, it, expect } from 'vitest'

// One gating store per canvas, owned by it (stores/gating.ts, docs/UI.md → *One gating store per canvas*).
// It used to be ONE app-wide store every page rebound to its own document — so returning to a page was
// a full reload, and every consumer carried guards against the store "flipping under us". These keep
// it from drifting back.
const OWNERS = ['modules/gate/GatingPlots.vue', 'modules/cluster/ClusterPlots.vue', 'components/canvas/LayoutCanvas.vue']

const RAW = import.meta.glob('/src/**/*.{vue,ts}', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
// code only — a comment naming `provideGatingStore()` is not a call
const stripComments = (t: string) => t.replace(/\/\*[\s\S]*?\*\//g, '').replace(/<!--[\s\S]*?-->/g, '').replace(/(^|[^:])\/\/.*$/gm, '$1')
const sources = Object.entries(RAW)
  .map(([path, text]) => ({ path: path.replace('/src/', ''), text: stripComments(text) }))
  .filter(s => !s.path.endsWith('.test.ts') && s.path !== 'stores/gating.ts')

describe('gating store ownership', () => {
  it('only the owning canvases create a store', () => {
    const creators = sources.filter(s => /provideGatingStore\(/.test(s.text)).map(s => s.path).sort()
    expect(creators).toEqual([...OWNERS].sort())
  })
  it('an owner never injects one — inject cannot see its own provide', () => {
    const bad = sources.filter(s => OWNERS.includes(s.path) && /useGatingStore\(/.test(s.text)).map(s => s.path)
    expect(bad).toEqual([])
  })
  it('nothing builds a gating store outside provideGatingStore', () => {
    const bad = sources.filter(s => /createGatingStore\(|defineStore\(\s*['"]gating/.test(s.text)).map(s => s.path)
    expect(bad).toEqual([])
  })
  it('the store module itself is per-canvas, not an app-wide Pinia store', () => {
    const src = stripComments(RAW['/src/stores/gating.ts'])
    expect(src).not.toMatch(/defineStore\(/)
    expect(src).toMatch(/reactive\(createGatingStore\(/)
  })
  it('no pop-type guard against a store another page moved', () => {
    const bad = sources.filter(s => /g\.popType\s*[!=]==\s*props\.popType|props\.popType\s*[!=]==\s*g\.popType/.test(s.text))
      .map(s => s.path)
    expect(bad).toEqual([])
  })
})
