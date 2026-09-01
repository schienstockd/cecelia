import { describe, it, expect } from 'vitest'
import { openingTags, templateBlock, sinkOf, rangeControls, undeclaredControls, rearmedTimers,
         driftingTextFields } from './continuousControls'

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
    expect(undeclaredControls('<input type="range" @input="refreshOverlays()" />')).toHaveLength(1)
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
  'modules/ViewerWindow.vue':
    'the timepoint scrubber: paints through usePlotResize (rafCoalesce) and prefetches through debouncedLatest — the two canonical schedulers, one per half',
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

describe('driftingTextFields', () => {
  it('flags a text field bound with :value and committed on @change', () => {
    expect(driftingTextFields('<input type="text" :value="name" @change="save($event)" />')).toHaveLength(1)
    expect(driftingTextFields('<input :value="name" @change="save($event)" />')).toHaveLength(1)   // no type = text
  })
  it('a v-model draft is the fix, so it is not flagged', () => {
    expect(driftingTextFields('<input type="text" v-model="draft" @change="save(draft)" />')).toEqual([])
  })
  it('a select cannot drift — its value only changes when the user picks, which fires change', () => {
    expect(driftingTextFields('<select :value="v" @change="set($event)"><option/></select>')).toEqual([])
  })
  // REVISED (2026-08-24). This used to assert a range was NOT this rule's business — it was the
  // continuous-controls rule's. That drew the line in the wrong place, and a `:value` + `@change` range
  // fell through BOTH: `undeclaredControls` only looks at `@input` handlers, so a slider that commits on
  // release is invisible to it, and this rule excluded the type. The viewer's z slider then drifted
  // exactly as a text field does, and worse — a drag is far longer than a keystroke, so every re-render
  // in between patched the thumb back out from under the pointer.
  it('flags a range committed on @change with nothing writing the value mid-drag', () => {
    expect(driftingTextFields('<input type="range" :value="n" @change="apply(n)" />')).toHaveLength(1)
  })
  it('the canonical range shape — @input writes, @change commits — cannot drift', () => {
    // This is the pattern docs/UI.md prescribes and PoolThrottle/PopulationManager use. It is controlled
    // on every event, so widening the type list without this exemption over-reports it as a bug.
    expect(driftingTextFields(
      '<input type="range" :value="n" @input="n = +$event.target.value" @change="apply(n)" />')).toEqual([])
    expect(driftingTextFields('<input type="range" :value="n" @input="apply(n)" />')).toEqual([])
    expect(driftingTextFields('<input type="range" v-model="n" @change="apply(n)" />')).toEqual([])
  })
})

// The sibling of the coalescing rule, and the same underlying mistake: letting the DOM and the model
// disagree. A field bound with `:value` and committed on `@change` is uncontrolled while focused, and
// Vue force-patches `value` against the DOM's current text on every element patch — so a re-render
// mid-typing throws away what the user typed. Reported as "I enter a movie name and it reverts to the
// prefilled one"; the plot-styling panel had five more of them. `useFieldDraft` is the fix.
describe('no text field lets the DOM drift from its binding', () => {
  it('every :value + @change text field uses a v-model draft', () => {
    const drifting = sources
      .flatMap(s => driftingTextFields(s.text).map(tag => `${s.path}: ${tag.slice(0, 80)}`))
    expect(drifting).toEqual([])
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
  'stores/guide.ts':
    'two, both deliberate: a POLL that re-reads DOM-derived gate state while a guide is open (Vue cannot track a <select>\'s value), and a one-shot delay before advancing a satisfied step so the user sees it acknowledged',
  'modules/ViewerWindow.vue':
    'the playback clock — a chosen frame rate, so a timer and not rAF; each tick arms the next, and a tick that finds the frame uncached holds instead of advancing',
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
// `utils/viewerOverlays` owns the coalescing — which only holds while it stays the sole owner.
describe('live napari view-property endpoints have exactly one owner', () => {
  const LIVE_ENDPOINTS = /\/api\/napari\/(set-z-view|apply-view-state)/
  const ALL = import.meta.glob('/src/**/*.{vue,ts}', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
  const all = Object.entries(ALL).map(([path, text]) => ({ path: path.replace('/src/', ''), text }))

  it('the glob resolved', () => { expect(all.length).toBeGreaterThan(sources.length) })

  it('nobody else POSTs to them', () => {
    const code = (t: string) => t.replace(/\/\*[\s\S]*?\*\//g, ' ').replace(/(^|[^:])\/\/[^\n]*/g, '$1')
    const others = all
      .filter(s => s.path !== 'utils/viewerOverlays.ts' && !s.path.endsWith('.test.ts'))
      .filter(s => LIVE_ENDPOINTS.test(code(s.text)))
      .map(s => s.path)
    expect(others).toEqual([])
  })
})

// ── A ResizeObserver that re-renders INTO what it observes ────────────────────
//
// Same family as the rules above — an effect that outruns its cause — but the feedback is structural
// rather than merely wasteful. Five views had written `new ResizeObserver(() => render())` where
// `render` appends an `<svg>` into the observed element, and every one of them sized that svg with a
// floor (`Math.max(200, host.clientWidth)`): in a panel narrower than the floor the svg is wider than
// its host, the host grows, the observer fires again. The browser breaks the cycle and reports
// "ResizeObserver loop completed with undelivered notifications" — which is exactly what showed up in
// the log rail. `usePlotResize` is the fix (rAF coalescing + skip a render the size did not ask for).
const RO_EXEMPT: Record<string, string> = {
  'components/canvas/CanvasPanel.vue':
    'writes its OWN height to keep a square plot square — but through rafCoalesce, NOT in the callback (pinned below)',
  // it DOES size a child from the measurement (the zoom workspace), which is the Movies trap — but
  // that child is `position: absolute` inside a box with no `overflow`, so it cannot put a scrollbar
  // on the observed element and cannot move its box
  'composables/useCanvasWorkspace.ts': 'sizes an out-of-flow child in a non-scrolling box — cannot move what it observes',
  'composables/usePlotResize.ts': 'IS the fix',
  'components/TeleportPopover.vue':
    're-places a floating box: writes only fixed top/left, whose box size is position-independent — but through rafCoalesce, NOT in the callback (pinned below)',
  'modules/MoviesModule.vue':
    'measures the viewport into the video\'s box — but through rafCoalesce, NOT in the callback (pinned below)',
  'components/plots/PlotChart.vue': 'already coalesces through rafCoalesce (the pattern usePlotResize generalises)',
  'components/plots/GateOverlay.vue': 'draws to a <canvas> of fixed size — a canvas paint cannot change layout',
  'components/plots/PlotLayers.vue': 'draws to a <canvas> of fixed size — a canvas paint cannot change layout',
  'components/plots/UmapView.vue': 'redraws a WebGL canvas at the box size; no element is appended',
  // Observes the WebGPU canvas so the popout's `viewState.canvas.{width,height}` follows a resize —
  // the movie surfaces read those fields as the size fields' placeholder. Callback body is a single
  // `publishViewStateSink.schedule(undefined)`: no DOM write to the observed element, so no
  // self-resize loop is possible.
  'modules/ViewerWindow.vue': 'schedules a debouncedLatest publish; callback writes no DOM',
}

describe('no plot re-renders into the element it observes', () => {
  // `sources` above is .vue only; the observer pattern also lives in composables, so this block needs
  // both extensions
  const RO_RAW = import.meta.glob('/src/**/*.{vue,ts}', { query: '?raw', import: 'default', eager: true }) as Record<string, string>
  const roSources = Object.entries(RO_RAW).map(([path, text]) => ({ path: path.replace('/src/', ''), text }))

  it('every ResizeObserver that triggers a render goes through usePlotResize', () => {
    const offenders = roSources
      .filter(s => /new ResizeObserver\(/.test(s.text))
      .filter(s => !(s.path in RO_EXEMPT))
      .filter(s => !s.path.endsWith('.test.ts'))
      .map(s => s.path)
    expect(offenders).toEqual([])
  })

  // Two exemptions are CONDITIONAL, and the condition is the whole point: each writes layout that the
  // observed element's own box depends on. Inline in the callback that is a self-resize during
  // delivery — exactly what the browser reports as "ResizeObserver loop completed with undelivered
  // notifications", and both of them did, in the log rail. Prose was not enough to keep either right,
  // so the callback line itself is pinned. The value is the function that must NOT be called inline.
  const RO_SCHEDULED: Record<string, string> = {
    // writes `root.style.height` on the element it OBSERVES; the `>1px` guard bounds the loop, not the
    // message
    'components/canvas/CanvasPanel.vue': 'enforceSquare',
    // measures into `displaySize`, which sizes the video INSIDE the observed viewport: grown past the
    // box a scrollbar appears, and that shrinks the CONTENT box the observer reports. The border box
    // never moves — which is how "measures a video element; writes nothing" read as safe for a release,
    // and why `roLoopTrace` now measures the inner box too.
    'modules/MoviesModule.vue': 'measureViewport',
    // observes the popover it also MOVES. Moving a `position: fixed` box cannot resize it, so this is
    // not the self-resize the other two are — but a box that grows (async content, a collapsible
    // section) fires the observer per step, and a re-place is a paint, so it takes the same route.
    'components/TeleportPopover.vue': 'reposition',
  }
  it.each(Object.entries(RO_SCHEDULED))(
    '%s schedules its write instead of writing in the callback', (path, inlineFn) => {
    const src = roSources.find(s => s.path === path)!.text
    const ctorLine = src.split('\n').find(l => l.includes('new ResizeObserver(')) ?? ''
    expect(ctorLine).toContain('schedule')
    expect(ctorLine).not.toContain(inlineFn)
  })

  it('the exemption list stays honest — every entry still exists and still observes', () => {
    const stale = Object.keys(RO_EXEMPT).filter(p => {
      const s = roSources.find(x => x.path === p)
      return !s || !/new ResizeObserver\(/.test(s.text)
    })
    expect(stale).toEqual([])
  })
})
