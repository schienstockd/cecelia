import { describe, it, expect, beforeEach } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'
import { useLinkedSelectionSource, useLinkedSelectionSubscriber } from './useLinkedSelection'
import { useLinkedSelectionStore, linkedSourceKey } from '../stores/linkedSelection'

describe('useLinkedSelection composables', () => {
  beforeEach(() => setActivePinia(createPinia()))

  it('source.set writes into the store and stamps sourcePlotId', () => {
    const src = useLinkedSelectionSource('umap-1', 'tracks')
    src.set([1, 2, 3])
    const s = useLinkedSelectionStore()
    expect(s.bag).toEqual({
      scope: 'tracks', ids: [1, 2, 3], source: 'umap-1', sourcePlotId: 'umap-1',
    })
  })

  it('source.set with an empty list clears the bag', () => {
    const src = useLinkedSelectionSource('umap-1', 'tracks')
    src.set([1])
    src.set([])
    expect(useLinkedSelectionStore().bag).toBeNull()
  })

  it('source.clear drops any active selection', () => {
    const src = useLinkedSelectionSource('umap-1', 'tracks')
    src.set([1])
    src.clear()
    expect(useLinkedSelectionStore().bag).toBeNull()
  })

  it('subscriber sees the active selection when scopes match', () => {
    const src = useLinkedSelectionSource('umap-1', 'tracks')
    const sub = useLinkedSelectionSubscriber('tracks')
    src.set([10, 20])
    expect(sub.anyActive.value).toBe(true)
    expect(sub.isSelected(10)).toBe(true)
    expect(sub.isSelected(20)).toBe(true)
    expect(sub.isSelected(99)).toBe(false)
    expect(sub.activeIds.value.size).toBe(2)
  })

  it('subscriber ignores a selection at a different scope', () => {
    // A `tracks` producer must not accidentally dim a `cells`-scoped consumer — track ids and
    // label ids share a numeric space but not a meaning; a track_id=1 selection shouldn't
    // highlight cell label 1 on a segmentation card.
    const src = useLinkedSelectionSource('umap-1', 'tracks')
    const cellsSub = useLinkedSelectionSubscriber('cells')
    src.set([1, 2, 3])
    expect(cellsSub.anyActive.value).toBe(false)
    expect(cellsSub.isSelected(1)).toBe(false)
    expect(cellsSub.activeIds.value.size).toBe(0)
  })

  it('subscriber goes idle when the store clears', () => {
    const src = useLinkedSelectionSource('umap-1', 'tracks')
    const sub = useLinkedSelectionSubscriber('tracks')
    src.set([1])
    expect(sub.anyActive.value).toBe(true)
    src.clear()
    expect(sub.anyActive.value).toBe(false)
    expect(sub.isSelected(1)).toBe(false)
  })

  it('two subscribers at the same scope both react to one writer', () => {
    // The whole point of the bag: N consumers, one selection.
    const src = useLinkedSelectionSource('umap-1', 'tracks')
    const a = useLinkedSelectionSubscriber('tracks')
    const b = useLinkedSelectionSubscriber('tracks')
    src.set([42])
    expect(a.isSelected(42)).toBe(true)
    expect(b.isSelected(42)).toBe(true)
  })

  it('activePerSource exposes the perSource map when the writer set one', () => {
    // A per-source writer (a plot's brush that knew which (uid, vn, pop) each id came from)
    // must be readable per-source by a subscriber — otherwise a cross-image pooled brush
    // collapses into the flat Set and the reader picks up numeric collisions from every
    // image. This is the reverse of the write-side contract the boxplot/strip renderer uses.
    const s = useLinkedSelectionStore()
    s.set({
      scope: 'tracks', ids: [3, 7, 11], source: 't', sourcePlotId: 't',
      perSource: {
        [linkedSourceKey('imgA', 'flowTom', 'root/B')]: [3, 7],
        [linkedSourceKey('imgB', 'flowTom')]: [11],
      },
    })
    const sub = useLinkedSelectionSubscriber('tracks')
    const perSrc = sub.activePerSource.value
    expect(perSrc.size).toBe(2)
    expect(perSrc.get(linkedSourceKey('imgA', 'flowTom', 'root/B'))?.has(7)).toBe(true)
    expect(perSrc.get(linkedSourceKey('imgB', 'flowTom'))?.has(11)).toBe(true)
    // Flat activeIds still works — it's the union across sources.
    expect(sub.activeIds.value.size).toBe(3)
  })

  it('activePerSource is empty when the writer only set flat ids', () => {
    // A legacy flat-Set writer (a Show button) leaves perSource undefined; the subscriber
    // still gets a valid (empty) Map so callers can uniformly branch on `size`, and consumers
    // fall back to the flat activeIds view for the actual selection.
    const src = useLinkedSelectionSource('umap-1', 'tracks')
    const sub = useLinkedSelectionSubscriber('tracks')
    src.set([1, 2])
    expect(sub.activePerSource.value.size).toBe(0)
    expect(sub.activeIds.value.size).toBe(2)
  })

  it('activePerSource is scope-gated too', () => {
    const s = useLinkedSelectionStore()
    s.set({ scope: 'tracks', ids: [1], source: 't', perSource: { [linkedSourceKey('imgA', 'flowTom')]: [1] } })
    expect(useLinkedSelectionSubscriber('cells').activePerSource.value.size).toBe(0)
  })

  it('a fresh source writer overwrites a previous writer at the same scope', () => {
    // Last-writer-wins — same contract as `TrackHighlight` shipped in PR #1165. A second brush
    // (or a Claude mark_* eventually) replaces the previous selection wholesale.
    const a = useLinkedSelectionSource('umap-1', 'tracks')
    const b = useLinkedSelectionSource('boxplot-2', 'tracks')
    const sub = useLinkedSelectionSubscriber('tracks')
    a.set([1, 2])
    b.set([9])
    expect(sub.isSelected(1)).toBe(false)
    expect(sub.isSelected(9)).toBe(true)
    expect(useLinkedSelectionStore().bag?.sourcePlotId).toBe('boxplot-2')
  })
})
