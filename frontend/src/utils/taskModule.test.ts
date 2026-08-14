import { describe, it, expect } from 'vitest'
import { moduleKeyFromFun, moduleIdFromFun, moduleColor, moduleTagStyle, MODULE_COLORS } from './taskModule'
import { composite, contrastRatio, WCAG_AA } from './colour'

// The module key answers "which page owns this task", and it is compared against the string a page
// passes as `module=`. It used to exist three times — here, in runningTasks and inline in the tasks
// store — which is how the mismatches below survived.
describe('moduleKeyFromFun', () => {
  it('strips the category suffix', () => {
    expect(moduleKeyFromFun('cleanupImages.smooth')).toBe('cleanup')
    expect(moduleKeyFromFun('segment.cellpose')).toBe('segment')
    expect(moduleKeyFromFun('testTasks.image_task')).toBe('test')
  })

  it('preserves case, because page keys are camelCase', () => {
    // The old derivation lowercased, so `clustPops` (the string ClusterCellsModule passes as
    // `module=`) never matched the derived `clustpops` and those tasks could not adopt onto their
    // own page. Every multi-word page had the same silent mismatch.
    expect(moduleKeyFromFun('clustPops.cluster')).toBe('clustPops')
    expect(moduleKeyFromFun('clustTracks.cluster')).toBe('clustTracks')
    expect(moduleKeyFromFun('clustRegions.cluster')).toBe('clustRegions')
    expect(moduleKeyFromFun('spatialAnalysis.cellNeighbours')).toBe('spatialAnalysis')
    expect(moduleKeyFromFun('opticalFlow.train')).toBe('opticalFlow')
  })

  it('sends both import and export to the Manage images page', () => {
    // One page hosts several categories. Without the explicit map, `exportImages` would strip to
    // 'export' — a module with no page — so an export running in another tab would never adopt.
    expect(moduleKeyFromFun('importImages.omezarr')).toBe('manageImages')
    expect(moduleKeyFromFun('exportImages.ome_tiff')).toBe('manageImages')
  })

  it('falls back to chain for a bare fun_name', () => {
    expect(moduleKeyFromFun('')).toBe('chain')
  })
})

describe('moduleIdFromFun', () => {
  it('is the key lowercased, and every id it yields has a colour', () => {
    expect(moduleIdFromFun('clustPops.cluster')).toBe('clustpops')
    expect(moduleIdFromFun('exportImages.ome_tiff')).toBe('manageimages')
    // The colour map is keyed lowercase; a key whose lowercase form is missing renders grey, which
    // is how the Manage images rename could have silently dropped that page's accent colour.
    for (const fun of ['importImages.omezarr', 'exportImages.ome_tiff', 'cleanupImages.smooth',
                       'segment.cellpose', 'clustPops.cluster', 'spatialAnalysis.cellNeighbours']) {
      expect(MODULE_COLORS[moduleIdFromFun(fun)], fun).toBeDefined()
    }
  })

  it('falls back to grey for an unknown module', () => {
    expect(moduleColor('nope')).toBe('#52525b')
  })
})

describe('moduleTagStyle — the one tint for .cc-module-tag', () => {
  // The bug this closes: TasksModule passed the camelCase module KEY straight to `moduleColor`, whose
  // map is keyed lowercase, so every multi-word module's row pill rendered GREY while the same module's
  // run tag in the image table (which went through moduleIdFromFun) rendered in colour. Accepting
  // either spelling is what makes one helper safe for both call sites.
  it('accepts the camelCase key as well as the lowercased id', () => {
    expect(moduleTagStyle('clustPops')).toEqual(moduleTagStyle('clustpops'))
    // the accent is asserted on the FILL, not on `color` — the label is deliberately a lifted variant
    expect(moduleTagStyle('spatialAnalysis').background).toBe(MODULE_COLORS.spatialanalysis + '22')
    expect(moduleTagStyle('manageImages').borderColor).toBe(MODULE_COLORS.manageimages + '55')
    expect(moduleColor('clustPops')).not.toBe('#52525b')
  })

  it('is the single derivation of the pill tint', () => {
    const c = MODULE_COLORS.cleanup
    const s = moduleTagStyle('cleanup')
    expect(s.background).toBe(c + '22')
    expect(s.borderColor).toBe(c + '55')
  })

  // The measurement that motivated the lift: EVERY module colour used as its own label sat at
  // 1.84-2.70:1 against the pill's fill. The palette is data, so the guarantee has to be asserted over
  // all of it — adding a thirteenth colour must not be able to reintroduce an unreadable label.
  it('gives every module a label that clears WCAG AA on the pill', () => {
    for (const [mod, accent] of Object.entries(MODULE_COLORS)) {
      const fill = composite(accent, '#21262d', 0x22 / 255)
      expect(contrastRatio(accent, fill), `${mod} raw accent`).toBeLessThan(WCAG_AA)
      expect(contrastRatio(moduleTagStyle(mod).color, fill), mod).toBeGreaterThanOrEqual(WCAG_AA)
    }
  })

  // …and the unknown-module grey too, which is the one a new task hits before it gets a colour.
  it('covers the grey fallback', () => {
    const grey = moduleColor('nope')
    expect(contrastRatio(moduleTagStyle('nope').color, composite(grey, '#21262d', 0x22 / 255)))
      .toBeGreaterThanOrEqual(WCAG_AA)
  })
})

// The ratchet. `.mod-pill` and `.run-tag` were the same pill written twice, and what made them a
// *silent* duplicate is that both shared the palette — so a grep for `taskModule` found two legitimate
// importers and nothing looked wrong. What they did NOT share is the step after the lookup: appending
// an alpha to the hex. That concatenation is the tell, and it belongs in `moduleTagStyle` alone.
const SOURCES = import.meta.glob('/src/**/*.{vue,ts}', {
  query: '?raw', import: 'default', eager: true,
}) as Record<string, string>

// Matching on `moduleColor(x) + '22'` alone is NOT enough, and the proof is the duplicate that
// motivated this: ImageTable assigned `const colour = moduleColor(...)` and appended the alpha a line
// later, so an inline-only pattern would have missed the exact case it exists to catch. So the rule is
// about the FILE: if it reads the module palette, it must not be appending an alpha hex anywhere.
const APPENDS_ALPHA = /\+\s*['"][0-9a-f]{2}['"]/i
const READS_PALETTE = /from\s+['"][^'"]*\/taskModule['"]/

describe('the module tint has one derivation', () => {
  it('no site that reads the palette appends an alpha itself', () => {
    const offenders = Object.entries(SOURCES)
      .filter(([p]) => !p.endsWith('/utils/taskModule.ts') && !p.endsWith('/utils/taskModule.test.ts'))
      .filter(([, text]) => READS_PALETTE.test(text) && APPENDS_ALPHA.test(text))
      .map(([p]) => p.replace('/src/', ''))
    expect(offenders, 'use moduleTagStyle() + class="cc-module-tag" instead').toEqual([])
  })

  // A detector nobody has seen fail is a detector that might match nothing at all. Both halves are
  // exercised on the two shapes the real duplicates actually had.
  it('would have caught both spellings of the duplicate it replaces', () => {
    const inline = `import { moduleColor } from '../utils/taskModule'
      :style="{ background: moduleColor(t.module) + '33' }"`
    const viaLocal = `import { moduleColor, moduleIdFromFun } from '../utils/taskModule'
      const colour = moduleColor(moduleIdFromFun(e.fun))
      style: { background: colour + '22', borderColor: colour + '55' },`
    for (const src of [inline, viaLocal])
      expect(READS_PALETTE.test(src) && APPENDS_ALPHA.test(src)).toBe(true)
    // …and does not fire on a file that merely reads the palette for something else
    expect(APPENDS_ALPHA.test(`const c = moduleColor(m); return { color: c }`)).toBe(false)
  })

  it('the glob resolved', () => {
    expect(Object.keys(SOURCES).length).toBeGreaterThan(100)
  })
})
