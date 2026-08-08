import { describe, it, expect } from 'vitest'
import { openingTags, templateBlock, sinkOf, rangeControls, undeclaredControls, rearmedTimers } from './continuousControls'

describe('openingTags', () => {
  it('does not end a tag on a `>` inside an attribute value', () => {
    const tags = openingTags('<input type="range" @input="a = x => x + 1" /><span>')
    expect(tags[0]).toContain('x => x + 1')
    expect(tags[1]).toBe('<span>')
  })
  it('ignores a `<` that is not a tag start', () => {
    expect(openingTags('{{ a < b }}<span>')).toEqual(['<span>'])
  })
})

describe('templateBlock', () => {
  // the scan used to run over the whole SFC; a `<` in the script (a comparison, a TS generic) opened a
  // quote-tracking run that the next apostrophe in a comment swallowed, and the scanner silently found
  // nothing from there on. Two real components dropped out of the audit that way.
  it('scans the template, not the script — an apostrophe in a comment cannot blind it', () => {
    const sfc = `<script setup lang="ts">
// it doesn't matter if a < b here
const n = ref(0)
</script>
<template><input type="range" @input="go()" /></template>`
    expect(templateBlock(sfc).trim()).toBe('<input type="range" @input="go()" />')
    expect(undeclaredControls(sfc)).toHaveLength(1)
  })
})

describe('sinkOf', () => {
  it('a write is a write', () => {
    expect(sinkOf('t = Number(($event.target as HTMLInputElement).value)', false)).toBe('bind')
    expect(sinkOf('pools[name] = +($event.target as HTMLInputElement).value', false)).toBe('bind')
    expect(sinkOf('f.duration = Number(x)', false)).toBe('bind')
    expect(sinkOf('anything', true)).toBe('bind')                    // v-model
  })
  it('an emit hands the decision to the parent', () => {
    expect(sinkOf("emit('update:lineWidth', v)", false)).toBe('emit')
    expect(sinkOf("$emit('update:zSlice', v)", false)).toBe('emit')
  })
  it('anything else is a call, and must be declared', () => {
    expect(sinkOf('set({ fontSize: 12 })', false)).toBe('call')
    expect(sinkOf('layout.applyTemplate(k, uniform(2, 3))', false)).toBe('call')
    // a comparison is not an assignment
    expect(sinkOf('go(a === b)', false)).toBe('call')
  })
})

describe('rangeControls', () => {
  it('picks up the release-only escape hatch', () => {
    const [c] = rangeControls(
      `<input type="range" :value="n" @input="n = +$event.target.value" @change="apply(n)" />`)
    expect(c.sink).toBe('bind')
    expect(c.hasChange).toBe(true)
  })
  it('ignores non-range inputs', () => {
    expect(rangeControls('<input type="text" @input="go()" />')).toEqual([])
  })
  it('flags a side-effecting drag handler', () => {
    expect(undeclaredControls('<input type="range" @input="refreshNapari()" />')).toHaveLength(1)
  })
})

// ── The enforcement ────────────────────────────────────────────────────────────────────────────
// A continuous control (`<input type="range">`) fires per pixel of travel. Its `@input` may WRITE the
// value; anything more has to be coalesced or moved to `@change` (once, on release). See
// docs/UI.md → *Continuous controls* for the rule and utils/continuousControls.ts for the reasoning.
//
// A file appears below only if one of its sliders calls something on `@input` — i.e. the effect is not
// visibly a write and not visibly release-only, so somebody had to check where it lands. The value
// says where. Adding a slider whose handler does real work fails this test until the sink is named.
const DECLARED_SINKS: Record<string, string> = {
  'components/canvas/PlotOptions.vue':
    'emits a vis patch into panel state; the plots it restyles rebuild at most once per frame (PlotChart)',
  'components/canvas/LayoutCanvas.vue':
    'applyTemplate mutates the board layout store — debounced autosave, and the slot re-renders coalesce in PlotChart',
  'components/TitleCardControls.vue':
    'patches the local title-card config object; the card is only rendered at record time',
  'modules/MoviesModule.vue':
    'sets the CSS zoom of the <video> element + the persisted preference; no request, no re-decode',
}

const RAW = import.meta.glob('/src/**/*.vue', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
const sources = Object.entries(RAW).map(([path, text]) => ({ path: path.replace('/src/', ''), text }))

describe('continuous controls do not fire an un-coalesced effect per event', () => {
  it('the glob resolved', () => { expect(sources.length).toBeGreaterThan(50) })

  it('every side-effecting slider handler names where its effect lands', () => {
    const undeclared = sources
      .filter(s => undeclaredControls(s.text).length > 0)
      .filter(s => !(s.path in DECLARED_SINKS))
      .map(s => s.path)
    expect(undeclared).toEqual([])
  })

  it('the declaration list stays honest — every entry still has such a slider', () => {
    const stale = Object.keys(DECLARED_SINKS).filter(p => {
      const s = sources.find(x => x.path === p)
      return !s || undeclaredControls(s.text).length === 0
    })
    expect(stale).toEqual([])
  })

  it('no slider reaches the network straight from its @input', () => {
    const direct = sources.flatMap(s => rangeControls(s.text)
      .filter(c => /\bfetch\s*\(|\baxios\b/.test(c.handler))
      .map(() => s.path))
    expect(direct).toEqual([])
  })
})

describe('rearmedTimers', () => {
  it('matches the clear-then-re-arm shape', () => {
    expect(rearmedTimers('if (t) clearTimeout(t); t = setTimeout(go, 200)')).toEqual(['t'])
  })
  it('a one-shot timer is not a re-arm', () => {
    expect(rearmedTimers('const t = setTimeout(go, 200); onUnmounted(() => clearTimeout(t))')).toEqual([])
  })
})

// The coalescing rule again, from the other side. Five near-copies of the same scheduler had grown up
// across the app, each with its own answer to "what happens to the superseded run" — which is exactly
// the divergence a canonical helper exists to stop. There are three now (`debouncedLatest` for a
// request, `rafCoalesce` for a paint, `debouncedSave` for a write); a new hand-rolled one must say what
// it is. Legitimate re-arming timers exist — that's what this list is; it is not an exemption from the
// rule so much as the inventory that makes a NEW entry a visible decision.
const DECLARED_TIMERS: Record<string, string> = {
  'utils/debouncedLatest.ts': 'IS the canonical request scheduler',
  'utils/debouncedSave.ts':   'IS the canonical write-behind scheduler',
  'stores/ws.ts':             'connect timeout + reconnect backoff — a deadline per attempt, not a debounce',
  'composables/useCopyFlash.ts':      'clears the "copied" flash; re-armed so a second copy restarts the flash',
  'composables/useDelayedLoading.ts': 'LEADING delay before a spinner appears — the opposite shape',
  'composables/useCanvasZoom.ts':     'trailing "the drag has stopped" detector for the compositor layer, not the zoom itself',
  'composables/useTaskCompletionWatch.ts':
    'coalesces terminal WS frames over an 8 s window — no result, no restore; the canonical helper for that job (INVENTORY)',
  'components/ConfirmButton.vue':     'disarms a confirm button after a grace period',
  'stores/taskPreview.ts':            'a POLL loop while the preview worker warms up — each tick arms the next',
}

describe('nobody hand-rolls a fourth debounce', () => {
  const ALL_SRC = import.meta.glob('/src/**/*.{vue,ts}', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
  const srcs = Object.entries(ALL_SRC)
    .map(([path, text]) => ({ path: path.replace('/src/', ''), text }))
    // the scanner itself carries these patterns as regex literals
    .filter(s => !s.path.endsWith('.test.ts') && s.path !== 'utils/continuousControls.ts')

  it('every re-armed timer says what it is', () => {
    const undeclared = srcs
      .filter(s => rearmedTimers(s.text).length > 0)
      .filter(s => !(s.path in DECLARED_TIMERS))
      .map(s => s.path)
    expect(undeclared).toEqual([])
  })

  it('the list stays honest — every entry still has one', () => {
    const stale = Object.keys(DECLARED_TIMERS).filter(p => {
      const s = srcs.find(x => x.path === p)
      return !s || rearmedTimers(s.text).length === 0
    })
    expect(stale).toEqual([])
  })
})

// The live-viewer pushes are the ones this audit started from: the movie z slider and the mask-outline
// slider each landed a napari command per slider event, and the bridge runs one command at a time, so
// the viewer kept stepping through slices long after the mouse was released. The fix is at the SINK —
// `utils/napariOverlays` owns the coalescing — which only holds while it stays the sole owner.
describe('live napari view-property endpoints have exactly one owner', () => {
  const LIVE_ENDPOINTS = /\/api\/napari\/(set-z-view|apply-view-state)/
  const ALL = import.meta.glob('/src/**/*.{vue,ts}', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
  const all = Object.entries(ALL).map(([path, text]) => ({ path: path.replace('/src/', ''), text }))

  it('the glob resolved', () => { expect(all.length).toBeGreaterThan(sources.length) })

  it('nobody else POSTs to them', () => {
    const code = (t: string) => t.replace(/\/\*[\s\S]*?\*\//g, ' ').replace(/(^|[^:])\/\/[^\n]*/g, '$1')
    const others = all
      .filter(s => s.path !== 'utils/napariOverlays.ts' && !s.path.endsWith('.test.ts'))
      .filter(s => LIVE_ENDPOINTS.test(code(s.text)))
      .map(s => s.path)
    expect(others).toEqual([])
  })
})
