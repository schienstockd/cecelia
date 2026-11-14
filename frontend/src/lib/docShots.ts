// The screenshots the app shows: a What's new feature card's picture (lib/whatsNew.ts → FEATURE_CARDS)
// and a tip's (lib/tips.ts). Each names a shot of the `in-app` set (user-docs/shots/in-app.ts), captured
// from lab data by scripts/docshots/ into user-docs/images/. That set is captured in every colour
// scheme, so a card shows the one its user has and the dialog never mixes schemes; a picture of the
// image (a viewer shot) has one file for every scheme.

import { IN_APP_SHOTS } from '../../../user-docs/shots/in-app.ts'
import { imageFile } from '../../../user-docs/shots/spec.ts'
import type { ColourScheme } from '../utils/colourScheme'

const DIR = '../../../user-docs/images/'
const URLS = import.meta.glob('../../../user-docs/images/app-*.{png,jpg}', { query: '?url', import: 'default', eager: true }) as Record<string, string>

/** The image of in-app shot `id` in `scheme`; null for an id the set has no shot for, or one not
 *  composed yet (`pixi run doc-images`), so the card falls back to its sketch. */
export function docShotUrl(id: string, scheme: ColourScheme): string | null {
  const shot = IN_APP_SHOTS.shots.find(s => s.id === id)
  return shot ? URLS[DIR + imageFile(shot, scheme)] ?? null : null
}
