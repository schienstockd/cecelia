// The screenshots the app itself shows (lib/docShots.ts): the picture on a What's new feature card
// (lib/whatsNew.ts → FEATURE_CARDS) and on tips (lib/tips.ts → `shot`). Every scheme, so a card shows
// the one its user has and never mixes them; cropped to the card's picture slot.

import type { ShotSet } from './spec.ts'
import { LYMPHOID_SECTION, SPLEEN_TIMELAPSE } from './fixtures.ts'

/** The card's picture: the dialog's 640 px less the card's padding. */
const CARD_PX = 576

export const IN_APP_SHOTS: ShotSet = {
  id: 'in-app',
  page: 'app',
  themes: ['dark', 'light'],
  column: CARD_PX,
  fixture: { projects: { spleen: SPLEEN_TIMELAPSE, lymphoid: LYMPHOID_SECTION } },
  shots: [
    {
      // What's new, #2097: the Checks group in the action bar.
      id: 'app-checks',
      project: 'spleen',
      route: '/manage-images',
      crop: ['images.checks'],
      expect: [{ anchor: 'images.checks', text: 'Checks' }, { anchor: 'images.checks', text: 'Acquisition' }],
    },
    {
      // Tip: segmentation.
      id: 'app-outlines',
      project: 'lymphoid',
      image: 'FtGoJO',
      viewer: { plane: 13, zoom: 5, at: [0.4, 0.75] },
      crop: ['css:canvas'],
      aspect: 2,
    },
    {
      // Tip: tracking.
      id: 'app-tracks',
      project: 'spleen',
      image: 'k3Tx90',
      viewer: { plane: 10, zoom: 4, at: [0.58, 0.5], outlines: false, tracks: ['T', 'B'], time: 74, tail: 75, channels: ['THG', 'Tcells-uGFP', 'SHG'] },
      crop: ['css:canvas'],
      aspect: 2,
    },
  ],
}
