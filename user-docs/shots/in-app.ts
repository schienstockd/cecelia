// The screenshots the app itself shows (lib/docShots.ts): the picture on a What's new feature card
// (lib/whatsNew.ts → FEATURE_CARDS) and on any tip given a `shot` (lib/tips.ts — none now; tips use
// feijoa sketches). Every scheme, so a card shows the one its user has and never mixes them; cropped
// to the card's picture slot.

import type { ShotSet } from './spec.ts'
import { SPLEEN_TIMELAPSE } from './fixtures.ts'

/** The card's picture: the dialog's 640 px less the card's padding. */
const CARD_PX = 576

export const IN_APP_SHOTS: ShotSet = {
  id: 'in-app',
  page: 'app',
  themes: ['dark', 'light'],
  column: CARD_PX,
  fixture: { projects: { spleen: SPLEEN_TIMELAPSE } },
  shots: [
    {
      // What's new, #2097: the Checks group in the action bar — on a page that checks a cohort, so all
      // three show (Cohort only appears where the page banks cohort metrics).
      id: 'app-checks',
      project: 'spleen',
      route: '/segment',
      crop: ['images.checks'],
      expect: [{ anchor: 'images.checks', text: 'Checks' }, { anchor: 'images.checks', text: 'Acquisition' }, { anchor: 'images.checks', text: 'Cohort' }],
    },
  ],
}
