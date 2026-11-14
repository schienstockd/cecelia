// Settings and Preferences are modals with one pane shown at a time (SettingsModal.vue, PreferencesModal.vue),
// so a step that points inside one has two ways to be unreachable: the modal is closed, or another pane
// is showing. `settingsReveal(target, paneAnchor, paneLabel, closed?)` covers both, in that order — the nav button of a pane only
// exists while the modal is open, so "open Settings" has to come first.
import type { Reveal } from './types'

export const settingsClosed = (paneAnchor: string): Reveal => ({
  needed: c => !c.anchorExists(paneAnchor),
  anchor: 'sidebar.settings',
  text: 'Open Settings from the sidebar.',
  placement: 'right',
})

/** Preferences' "modal closed" reveal: it opens from the avatar menu (`header.profile`). */
export const preferencesClosed = (paneAnchor: string): Reveal => ({
  needed: c => !c.anchorExists(paneAnchor),
  anchor: 'header.profile',
  text: 'Open your avatar menu, then Preferences.',
  placement: 'bottom-end',
})

// `closed` is how to open the modal at all: Settings by default; Preferences passes its own
// (`preferencesClosed`), since it opens from the avatar menu, not the sidebar.
export const settingsReveal = (target: string, paneAnchor: string, paneLabel: string,
                               closed: Reveal = settingsClosed(paneAnchor)): Reveal[] => [
  closed,
  {
    needed: c => !c.anchorExists(target),
    anchor: paneAnchor,
    text: `Open ${paneLabel} to see it.`,
    placement: 'right',
  },
]
