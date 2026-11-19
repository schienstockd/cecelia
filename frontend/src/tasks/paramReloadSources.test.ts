// RESULT-REVIEW: result-284917fc
// The pop / measure pickers reload on the image, the segmentation and (measures) the picked pops —
// and on NOTHING else. One getter returning a fresh array fired on every param write, so a pick
// flashed the list empty; a joined-string key fixed that but stopped a re-delivered image (a task
// added pops/measures, same uid) from refreshing the list.
import { describe, it, expect } from 'vitest'
import { reactive, watch, nextTick } from 'vue'
import { popReloadSources, colsReloadSources, type ReloadContext } from './paramValues'
import type { ParamDef } from './types'

const params = [
  { key: 'valueName', type: 'valueNameSelection', field: 'labels' },
  { key: 'popsToCluster', type: 'popSelection', multiple: true },
  { key: 'measures', type: 'labelPropsColsSelection' },
  { key: 'resolution', type: 'float' },
] as unknown as ParamDef[]

function setup() {
  const ctx = reactive<ReloadContext>({
    images: [{ uid: 'i1', labels: { A: [] }, channelNames: ['CD4', 'CD8'] }],
    params,
    values: { valueName: 'A', popsToCluster: ['A/Tcells'], measures: ['area'], resolution: 1 },
  })
  const fires = { pops: 0, cols: 0 }
  watch(popReloadSources(() => ctx), () => { fires.pops++ })
  watch(colsReloadSources(() => ctx), () => { fires.cols++ })
  return { ctx, fires }
}

describe('picker reload sources', () => {
  it('an unrelated param write reloads neither picker', async () => {
    const { ctx, fires } = setup()
    ctx.values = { ...ctx.values, resolution: 2 }   // the form rebuilds `values` on every write
    ctx.images = [...ctx.images!]                   // the chain editor rebuilds the array too
    await nextTick()
    expect(fires).toEqual({ pops: 0, cols: 0 })
  })

  it('picking a population reloads the measures, not the populations', async () => {
    const { ctx, fires } = setup()
    ctx.values = { ...ctx.values, popsToCluster: ['A/Tcells', 'A/Bcells'] }
    await nextTick()
    expect(fires).toEqual({ pops: 0, cols: 1 })
  })

  it('a refreshed image (same uid, labels and channels, merged in place) reloads both', async () => {
    const { ctx, fires } = setup()
    Object.assign(ctx.images![0]!, { uid: 'i1', labels: { A: [] }, channelNames: ['CD4', 'CD8'] })
    await nextTick()
    expect(fires).toEqual({ pops: 1, cols: 1 })
  })

  it('a renamed channel reloads the measures only', async () => {
    const { ctx, fires } = setup()
    ctx.images![0]!.channelNames = ['CD4', 'CD8a']
    await nextTick()
    expect(fires).toEqual({ pops: 0, cols: 1 })
  })
})
