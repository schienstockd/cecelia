import { describe, it, expect } from 'vitest'
import { setupOrderHazards } from './setupOrder'

const sfc = (body: string) => `<script setup lang="ts">\n${body}\n</script>\n<template><div /></template>`

describe('setupOrderHazards', () => {
  // The real one. `channels` was added to a watch source and declared thirty lines below it; the
  // ReferenceError took FlowMetricsView's setup down, which aborted the parent patch and blanked the
  // whole canvas — the plot panels AND the model vault, which is a sibling and entirely innocent.
  it('flags a watch source naming a const declared below it', () => {
    const src = sfc(`
watch(() => [state.t, channels.value.join(',')], () => load())
const channels = computed(() => [])
`)
    expect(setupOrderHazards(src)).toEqual([{ name: 'channels', line: 3 }])
  })

  it('is happy once the declaration moves above', () => {
    const src = sfc(`
const channels = computed(() => [])
watch(() => [state.t, channels.value.join(',')], () => load())
`)
    expect(setupOrderHazards(src)).toEqual([])
  })

  // The distinction that makes this checkable at all: the callback runs later, so it may name
  // anything. A first pass that searched the whole call reported two correct SummaryPanel watches.
  it('does not flag a CALLBACK naming a later const — that runs after setup', () => {
    const src = sfc(`
watch(source, () => { render(vis.value) })
const vis = computed(() => 1)
`)
    expect(setupOrderHazards(src)).toEqual([])
  })

  it('checks all of watchEffect — there is no callback to separate', () => {
    const src = sfc(`
watchEffect(() => { render(vis.value) })
const vis = computed(() => 1)
`)
    expect(setupOrderHazards(src)).toEqual([{ name: 'vis', line: 3 }])
  })

  it('ignores a watch inside a function — everything is initialised by the time it runs', () => {
    const src = sfc(`
function later() {
  watch(() => channels.value, () => {})
}
const channels = computed(() => [])
`)
    expect(setupOrderHazards(src)).toEqual([])
  })

  it('ignores computed — its getter is lazy, so a later const is fine', () => {
    const src = sfc(`
const a = computed(() => b.value)
const b = computed(() => 1)
`)
    expect(setupOrderHazards(src)).toEqual([])
  })

  // …UNLESS a watch source names it. That is what makes the lazy getter run at setup, and it is the
  // one that blanked the Movies page: `movieTableRows` was declared above the watch, so a direct check
  // saw nothing, while the `starredOnly` ref its body reads was sixty lines below.
  it('follows a watch source ONE HOP into the computed it names', () => {
    const src = sfc(`
const rows = computed(() => filter(all.value, starredOnly.value))
watch(rows, r => keep(r))
const starredOnly = ref(false)
`)
    expect(setupOrderHazards(src)).toEqual([{ name: 'starredOnly', line: 4 }])
  })

  it('is happy once that ref moves above the watch', () => {
    const src = sfc(`
const starredOnly = ref(false)
const rows = computed(() => filter(all.value, starredOnly.value))
watch(rows, r => keep(r))
`)
    expect(setupOrderHazards(src)).toEqual([])
  })

  it('does not follow a non-computed source — a plain ref reads nothing of its own', () => {
    const src = sfc(`
const plain = ref(0)
watch(plain, () => {})
const later = ref(1)
`)
    expect(setupOrderHazards(src)).toEqual([])
  })

  it('understands a destructured declaration', () => {
    const src = sfc(`
watch(() => scope.value, () => {})
const { scope, model } = useViewState(shared, {})
`)
    expect(setupOrderHazards(src).map(h => h.name)).toEqual(['scope'])
  })

  it('does not treat a property access as the binding', () => {
    // `props.vis` is not `vis`. Without this, SummaryPanel's (correct) fetch watch is reported.
    const src = sfc(`
watch([() => props.vis?.statsEnabled], () => {})
const vis = computed(() => 1)
`)
    expect(setupOrderHazards(src)).toEqual([])
  })

  it('does not confuse a substring for the identifier', () => {
    const src = sfc(`
watch(() => channelCount.value, () => {})
const channels = computed(() => [])
const channelCount = computed(() => 0)
`)
    expect(setupOrderHazards(src).map(h => h.name)).toEqual(['channelCount'])
  })

  it('does not split the first argument on a comma inside brackets or a string', () => {
    const src = sfc(`
watch(() => [a.value, later.value], () => {})
const later = computed(() => 1)
`)
    expect(setupOrderHazards(src).map(h => h.name)).toEqual(['later'])
  })

  it('is silent on a file with no script setup', () => {
    expect(setupOrderHazards('<template><div /></template>')).toEqual([])
  })
})

// The ratchet. Zero, with no allow-list: there is no legitimate reason for a watch source to name a
// binding that does not exist yet, and the failure mode is a blank page with a console-only clue.
const RAW = import.meta.glob('/src/**/*.vue', { query: '?raw', import: 'default', eager: true }) as
  Record<string, string>

describe('no setup-order hazards in the app', () => {
  it('found the sources it is meant to police', () => {
    expect(Object.keys(RAW).length).toBeGreaterThan(50)
  })

  it('no watch source names a const declared below it', () => {
    const hits: string[] = []
    for (const [path, src] of Object.entries(RAW))
      for (const h of setupOrderHazards(src))
        hits.push(`${path.replace('/src/', '')}:${h.line} — '${h.name}' is declared later`)
    expect(hits).toEqual([])
  })
})
