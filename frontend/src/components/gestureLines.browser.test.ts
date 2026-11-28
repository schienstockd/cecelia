// Preferences → Interface → "Show gesture hints": off hides every `.cc-gesture-line` in the app through
// one root class (`settings.gestureLines` → `cc-hide-gestures` on <html> → style.css), on shows them.
import { afterEach, describe, expect, it } from 'vitest'
import { h } from 'vue'
import { frames, mountInto } from '../test/browser/mount'
import { useSettingsStore } from '../stores/settings'

let settings: ReturnType<typeof useSettingsStore>

function mount() {
  return mountInto({
    setup() { settings = useSettingsStore() },
    render: () => h('span', { class: 'cc-gesture-line' }, [h('kbd', 'Ctrl'), '-click adds']),
  })
}

const shown = (host: HTMLElement) =>
  getComputedStyle(host.querySelector('.cc-gesture-line')!).display !== 'none'

afterEach(() => { settings.gestureLines = true })

describe('gesture hints preference', () => {
  it('turning it off hides the gesture line; turning it back on shows it', async () => {
    const host = mount()
    await frames()
    expect(shown(host)).toBe(true)

    settings.gestureLines = false
    await frames()
    expect(shown(host)).toBe(false)

    settings.gestureLines = true
    await frames()
    expect(shown(host)).toBe(true)
  })
})
