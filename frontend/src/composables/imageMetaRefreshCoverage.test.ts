import { describe, it, expect } from 'vitest'

// COVERAGE ratchet for channel renames — the twin of dataRefreshCoverage.test.ts.
//
// A rename is a direct POST, not a task: it never bumps the data version, so `useDataRefresh` never
// fires, and plot pages are kept alive, so navigating back never re-runs their load. Anything that
// copies channel display names out of a server response therefore shows the OLD names until it is
// told — by `useImageMetaRefresh`, or (task forms, whose images are reactive store objects) by keying
// its reload on `image.channelNames`.
//
// Detector: a source that reads `channelNames` off a fetched response (`d.channelNames`, …).

const RAW = import.meta.glob('/src/**/*.{vue,ts}', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
const sources = Object.entries(RAW)
  .map(([path, text]) => ({ path: path.replace('/src/', ''), text }))
  .filter(s => !s.path.endsWith('.test.ts'))

const COPIES_NAMES = /\b(d|data|res|json|body|r)\.channelNames/

// subscribe to the rename signal
const SUBSCRIBES = [
  'components/plots/GatingStrategyView.vue',
  'composables/useClusterContext.ts',
  'stores/gating.ts',
]
// key their reload on the store image's `channelNames` instead (the form context IS the store image)
const KEYED: Record<string, RegExp> = {
  'tasks/ParamRenderer.vue': /images\?\.\[0\]\?\.channelNames/,
  'tasks/paramAdvisors.ts': /\(i\.channelNames \?\? \[\]\)/,  // popsCompatAdvisor.reloadOn (behaviour pinned in paramAdvisors.test.ts)
}

describe('channel-rename coverage', () => {
  it('the glob resolved', () => {
    expect(sources.length).toBeGreaterThan(100)
  })

  it('every response-copy of channel names is listed', () => {
    const copiers = sources.filter(s => COPIES_NAMES.test(s.text)).map(s => s.path).sort()
    expect(copiers).toEqual([...SUBSCRIBES, ...Object.keys(KEYED)].sort())
  })

  it('subscribers call useImageMetaRefresh', () => {
    const missing = SUBSCRIBES.filter(p => !sources.find(s => s.path === p)?.text.includes('useImageMetaRefresh('))
    expect(missing).toEqual([])
  })

  it('task-form sites key their reload on channelNames', () => {
    const missing = Object.entries(KEYED).filter(([p, re]) => !re.test(sources.find(s => s.path === p)?.text ?? ''))
    expect(missing.map(([p]) => p)).toEqual([])
  })
})
