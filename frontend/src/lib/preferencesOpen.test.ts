// Settings → HPC's "Your HPC settings" opens Preferences on the HPC pane; the header chip on Profiles.
import { describe, it, expect } from 'vitest'
import { openPreferences, closePreferences, isPreferencesOpen, preferencesPane } from './preferencesOpen'

describe('openPreferences', () => {
  it('opens on the named pane, and on Profiles by default', () => {
    openPreferences('hpc')
    expect(isPreferencesOpen.value).toBe(true)
    expect(preferencesPane.value).toBe('hpc')
    closePreferences()
    expect(isPreferencesOpen.value).toBe(false)
    openPreferences()
    expect(preferencesPane.value).toBe('profiles')
  })
})
