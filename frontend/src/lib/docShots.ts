// The screenshots the app shows: a What's new feature card's picture (lib/whatsNew.ts → FEATURE_CARDS)
// and a tip's (lib/tips.ts). Each names a shot of the `in-app` set (user-docs/shots/in-app.ts), captured
// from lab data by scripts/docshots/ into user-docs/images/. That set is captured in every colour
// scheme, so a card shows the one its user has and the dialog never mixes schemes; a picture of the
// image (a viewer shot) has one file for every scheme.

import { IN_APP_SHOTS } from '../../../user-docs/shots/in-app.ts'
import { imageFile, imageStem } from '../../../user-docs/shots/spec.ts'
import type { ColourScheme } from '../utils/colourScheme'

const DIR = '../../../user-docs/images/'
const URLS = import.meta.glob('../../../user-docs/images/app-*.{png,jpg}', { query: '?url', import: 'default', eager: true }) as Record<string, string>
// file stem → the width it was composed for, in CSS px (compose.mjs): the files are 2× that, for sharp text
const WIDTHS = (import.meta.glob('../../../user-docs/images/images.json', { import: 'default', eager: true })[DIR + 'images.json'] ?? {}) as Record<string, number>

export interface DocShot { url: string; width: number | null }

/** The image of in-app shot `id` in `scheme` and the width to show it at (the file is 2× that; drawn at
 *  its own pixels it shows the app at twice its size); null for an id the set has no shot for, or one
 *  not composed yet (`pixi run doc-images`), so the card falls back to its sketch. */
export function docShot(id: string, scheme: ColourScheme): DocShot | null {
  const shot = IN_APP_SHOTS.shots.find(s => s.id === id)
  const file = shot ? imageFile(shot, scheme) : null
  const url = file ? URLS[DIR + file] : undefined
  return file && url ? { url, width: WIDTHS[imageStem(file)] ?? null } : null
}
