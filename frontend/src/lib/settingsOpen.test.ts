// The sidebar footer and the old `/settings` URL open Settings; reopening returns to the last pane.
import { describe, it, expect } from 'vitest'
import { openSettings, closeSettings, isSettingsOpen, settingsPane } from './settingsOpen'

describe('openSettings', () => {
  it('opens on the named pane, else on the last one', () => {
    openSettings('storage')
    expect(isSettingsOpen.value).toBe(true)
    expect(settingsPane.value).toBe('storage')
    closeSettings()
    expect(isSettingsOpen.value).toBe(false)
    openSettings()
    expect(settingsPane.value).toBe('storage')
  })
})
