import { describe, it, expect, beforeEach, vi, afterEach } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'
import { useAnalysisTabsStore } from '../stores/analysisTabs'
import { pickBoardTab } from './boardNav'

const G = 'analysis:p1'

describe('pickBoardTab', () => {
  beforeEach(() => { setActivePinia(createPinia()); vi.useFakeTimers() })
  afterEach(() => vi.useRealTimers())

  it('selects a tab that is already registered', async () => {
    const tabs = useAnalysisTabsStore()
    tabs.ensure(G)
    const id = tabs.addTab(G, 'Speeds')
    tabs.setActive(G, tabs.entries[G].tabs[0].id)
    await expect(pickBoardTab('p1', 'Speeds')).resolves.toBe(true)
    expect(tabs.entries[G].activeId).toBe(id)
  })

  it('waits for a board that registers late (the slow boards:changed reload)', async () => {
    const tabs = useAnalysisTabsStore()
    tabs.ensure(G)
    const first = tabs.entries[G].activeId
    const p = pickBoardTab('p1', 'Kiwi · speed')
    vi.advanceTimersByTime(3000)                    // past the old 1 s give-up
    expect(tabs.entries[G].activeId).toBe(first)
    tabs.load(G, { tabs: [{ id: 1, name: 'Board 1' }, { id: 7, name: 'Kiwi · speed' }], activeId: 1, nextId: 7 })
    await expect(p).resolves.toBe(true)
    expect(tabs.entries[G].activeId).toBe(7)
  })

  it('resolves false on timeout and leaves the active board alone', async () => {
    const tabs = useAnalysisTabsStore()
    tabs.ensure(G)
    const first = tabs.entries[G].activeId
    const p = pickBoardTab('p1', 'Missing', 500)
    vi.advanceTimersByTime(600)
    await expect(p).resolves.toBe(false)
    expect(tabs.entries[G].activeId).toBe(first)
  })
})
