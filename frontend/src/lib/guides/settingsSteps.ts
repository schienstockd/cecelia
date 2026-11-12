// Settings is a modal with one pane shown at a time (components/SettingsModal.vue), so a step that
// points inside it has two ways to be unreachable: the modal is closed, or another pane is showing.
// `settingsReveal(paneAnchor, paneLabel)` covers both, in that order — the nav button of a pane only
// exists while the modal is open, so "open Settings" has to come first.
import type { Reveal } from './types'

export const settingsClosed = (paneAnchor: string): Reveal => ({
  needed: c => !c.anchorExists(paneAnchor),
  anchor: 'sidebar.settings',
  text: 'Open Settings from the sidebar.',
  placement: 'right',
})

export const settingsReveal = (target: string, paneAnchor: string, paneLabel: string): Reveal[] => [
  settingsClosed(paneAnchor),
  {
    needed: c => !c.anchorExists(target),
    anchor: paneAnchor,
    text: `Open ${paneLabel} to see it.`,
    placement: 'right',
  },
]
