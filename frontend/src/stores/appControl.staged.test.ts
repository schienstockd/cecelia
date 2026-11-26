import { describe, it, expect, beforeEach } from 'vitest'
import { setActivePinia, createPinia } from 'pinia'
import { useAppControlStore } from './appControl'

// Staging an update or a revert in one window must show "Restart to finish" in every window: the
// others learn it from a broadcast, and land in the same state the staging window's POST answer gives.
describe('appControl — markStaged', () => {
  beforeEach(() => { setActivePinia(createPinia()) })

  it('a staged update: pending, nothing more to apply, a previous release to revert to', () => {
    const app = useAppControlStore()
    app.updateAvailable = true
    app.markStaged('update')
    expect([app.updatePending, app.updateAvailable, app.updateHasPrevious]).toEqual([true, false, true])
  })

  it('a staged revert: pending, and no previous release left to revert to', () => {
    const app = useAppControlStore()
    app.updateHasPrevious = true
    app.markStaged('revert')
    expect([app.updatePending, app.updateAvailable, app.updateHasPrevious]).toEqual([true, false, false])
  })
})
