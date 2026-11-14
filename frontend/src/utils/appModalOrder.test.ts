// App.vue mounts the app-level dialogs in stacking order — DOM order IS stacking order (BaseModal.vue →
// isTopmost) — so a dialog opened FROM another must be mounted after it, or it opens underneath, dimmed
// and unreachable, and Esc closes its parent instead. Preferences → Kiwi → AI features opened the AI
// opt-in dialog behind Preferences until this order was fixed. Each pair is [opener, opened].
import { describe, expect, it } from 'vitest'

const APP = Object.values(import.meta.glob('/src/App.vue', { query: '?raw', import: 'default', eager: true }))[0] as string
const at = (tag: string) => {
  const i = APP.indexOf(`<${tag} `)
  expect(i, `<${tag}> is not mounted in App.vue`).toBeGreaterThan(-1)
  return i
}

describe('App.vue dialog stacking order', () => {
  it.each([
    ['SettingsModal', 'PreferencesModal'],     // Settings → HPC opens Preferences
    ['PreferencesModal', 'AiOptInDialog'],     // Preferences → Kiwi → AI features
    ['SettingsModal', 'AiOptInDialog'],
  ])('%s is mounted before %s', (opener, opened) => {
    expect(at(opener)).toBeLessThan(at(opened))
  })
})
