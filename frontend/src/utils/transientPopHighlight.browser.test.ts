// The gating page's plot highlight of the viewer's pick selection across successive picks: wired as
// `GatingPlots.vue` wires it — the appear-watch adds the pop, `watchPopRenames` drops a deleted one —
// over the trees the server broadcasts (`_inject_pick_pop!`, a fixed uid). Every pick after the first
// used to switch the highlight off; an eye the user turned off has to stay off through the next pick.
import { describe, expect, it } from 'vitest'
import { computed, nextTick, ref } from 'vue'
import { watchAppearingTransientPops } from './transientPopHighlight'
import { remapPopKeys, watchPopRenames } from './popRenameRemap'
import { addSelected } from './selection'

interface P { path: string; uid: string; transient?: boolean }
const PICK = '/Pick selection'
const gate: P = { path: '/A', uid: 'aaaaaa' }
const pick: P = { path: PICK, uid: 'pick_selection', transient: true }

function page() {
  const flat = ref<P[]>([gate])
  const hl = ref<string[]>([])
  const transientPaths = computed(() => flat.value.filter(p => p.transient).map(p => p.path))
  watchAppearingTransientPops(() => transientPaths.value, paths => { hl.value = [...addSelected(hl.value, paths)] })
  watchPopRenames(() => flat.value, remap => { hl.value = remapPopKeys(hl.value, remap) })
  // a broadcast: a fresh array of fresh objects, as the store's `flat` is after every tree push
  const broadcast = async (pops: P[]) => { flat.value = pops.map(p => ({ ...p })); await nextTick() }
  return { hl, broadcast }
}

describe('pick selection highlight on the gating plots', () => {
  it('stays on across successive picks', async () => {
    const { hl, broadcast } = page()
    await broadcast([gate, pick]);  expect(hl.value).toEqual([PICK])
    await broadcast([gate, pick]);  expect(hl.value).toEqual([PICK])
    await broadcast([gate, pick]);  expect(hl.value).toEqual([PICK])
  })
  it('an eye the user switched off stays off through the next pick and a gate edit', async () => {
    const { hl, broadcast } = page()
    await broadcast([gate, pick])
    hl.value = []                                              // the user's eye click
    await broadcast([gate, pick]);  expect(hl.value).toEqual([])
    await broadcast([{ ...gate, uid: gate.uid }, pick]);  expect(hl.value).toEqual([])
  })
  it('a cleared selection goes, and the next pick is shown again', async () => {
    const { hl, broadcast } = page()
    await broadcast([gate, pick]);  hl.value = []
    await broadcast([gate]);        expect(hl.value).toEqual([])
    await broadcast([gate, pick]);  expect(hl.value).toEqual([PICK])
  })
})
