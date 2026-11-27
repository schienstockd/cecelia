// The viewer window following its own pick selection: a pick broadcast (`gating:pop-tree` with the
// transient `Pick selection` pop) loads the picked labels, a clear (a tree without it) drops them — the
// state `ViewerWindow` dims the dots and outlines the mask from. The pure rules are
// `utils/viewer/pickSelection.test.ts`; this pins the wiring to the WS push and the membership route.
import { beforeEach, describe, expect, it } from 'vitest'
import { defineComponent, h, type Ref } from 'vue'
import { mountInto, frames } from '../test/browser/mount'
import { useWsStore } from '../stores/ws'
import type { ViewerPickSelection } from '../utils/viewer/pickSelection'
import { useViewerPickSelection } from './useViewerPickSelection'

// The membership route, answered in the page: the setup file's network guard is replaced for this
// file only by a stub that serves `/api/gating/membership` and nothing else (a 404 = no selection,
// as the server answers).
const membership = { labels: [] as number[] }
function serveMembership() {
  window.fetch = async (input: RequestInfo | URL) => {
    const url = new URL(typeof input === 'string' ? input : input instanceof URL ? input.href : input.url, location.href)
    if (url.pathname !== '/api/gating/membership') throw new Error('unexpected ' + url.pathname)
    const pop = url.searchParams.get('pops') ?? ''
    return membership.labels.length
      ? new Response(JSON.stringify({ membership: { [pop]: membership.labels } }), { status: 200, headers: { 'Content-Type': 'application/json' } })
      : new Response(JSON.stringify({ error: 'pop not found' }), { status: 404, headers: { 'Content-Type': 'application/json' } })
  }
}

function mount() {
  let sel!: Ref<ViewerPickSelection | null>
  let push!: (d: Record<string, unknown>) => void
  mountInto(defineComponent({
    setup() {
      const ws = useWsStore()
      const on = ws.on
      ws.on = (t, fn) => { if (t === 'gating:pop-tree') push = fn; on(t, fn) }
      sel = useViewerPickSelection('P', 'I', () => 'seg')
      return () => h('div')
    },
  }))
  return { sel: () => sel.value, push: async (d: Record<string, unknown>) => { push(d); await frames() } }
}

const tree = (pick: boolean) => ({ value_name: 'seg', pop_type: 'flow',
  populations: [{ name: 'A' }, ...(pick ? [{ name: 'Pick selection', transient: true }] : [])] })

describe('useViewerPickSelection', () => {
  beforeEach(serveMembership)
  it('a pick shows up, a clear removes it, another image is ignored', async () => {
    membership.labels = []
    const m = mount()
    await frames()
    expect(m.sel()).toBeNull()                           // nothing picked yet
    membership.labels = [4, 9]
    await m.push({ imageUid: 'I', valueName: 'seg', tree: tree(true) })
    expect(m.sel()).toEqual({ valueName: 'seg', labels: [4, 9] })
    await m.push({ imageUid: 'other', valueName: 'seg', tree: tree(false) })
    expect(m.sel()).toEqual({ valueName: 'seg', labels: [4, 9] })
    membership.labels = []
    await m.push({ imageUid: 'I', valueName: 'seg', tree: tree(false) })   // the gating page's trash
    expect(m.sel()).toBeNull()
  })
  it('a selection already there when the viewer opens is loaded', async () => {
    membership.labels = [7]
    const m = mount()
    await frames()
    expect(m.sel()).toEqual({ valueName: 'seg', labels: [7] })
  })
})
