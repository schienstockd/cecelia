// RESULT-REVIEW: result-5d777c5d
// task:done → refreshImageMeta → updateImageMeta merges the fresh metadata into the store image with
// Object.assign — the SAME object, same uid/labels keys/channels. The pickers must still re-read it
// (a task may have added populations or measure columns); watching the image object never fired.
import { it, expect } from 'vitest'
import { reactive, watch, nextTick } from 'vue'
import { popReloadSources, colsReloadSources, type ReloadContext } from './paramValues'
import type { ParamDef } from './types'

const params = [
  { key: 'valueName', type: 'valueNameSelection', field: 'labels' },
  { key: 'popsToCluster', type: 'popSelection', multiple: true },
  { key: 'measures', type: 'labelPropsColsSelection' },
] as unknown as ParamDef[]

it('an in-place image refresh reloads both pickers', async () => {
  const store = reactive({ images: [{ uid: 'i1', labels: { A: [] as unknown[] }, channelNames: ['CD4', 'CD8'] }] })
  const ctx = (): ReloadContext => ({ images: [store.images[0]!], params, values: { valueName: 'A', popsToCluster: ['A/T'] } })
  const fires = { pops: 0, cols: 0 }
  watch(popReloadSources(ctx), () => { fires.pops++ })
  watch(colsReloadSources(ctx), () => { fires.cols++ })
  Object.assign(store.images[0]!, { uid: 'i1', labels: { A: [] }, channelNames: ['CD4', 'CD8'] })  // fresh JSON, same content
  await nextTick()
  expect(fires).toEqual({ pops: 1, cols: 1 })
})
