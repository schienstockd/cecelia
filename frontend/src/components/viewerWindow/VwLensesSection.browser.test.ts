// The Lenses section lists every lens whose scores are on this image; Go to belongs only on the image the
// lens was drawn on — elsewhere it opened another image, where the lens was not.
import { describe, expect, it } from 'vitest'
import { h } from 'vue'
import VwLensesSection from './VwLensesSection.vue'
import { mountInto } from '../../test/browser/mount'
import type { LensFigureRow } from '../../utils/viewer/lensQuery'

const row = (name: string, sourceUid: string): LensFigureRow => ({
  name, label: name, displayName: '',
  lens: { sourceUid, x: 10, y: 10, r: 5, column: `spatial.lens.${name}`, images: ['HERE', 'THERE'] },
})

describe('VwLensesSection', () => {
  it('offers Go to only on the image the lens was drawn on', () => {
    const host = mountInto({
      render: () => h(VwLensesSection, { lenses: [row('own', 'HERE'), row('other', 'THERE')], shownColumn: '', imageUid: 'HERE' }),
    })
    expect(host.querySelector('[aria-label="Go to own"]')).not.toBeNull()
    expect(host.querySelector('[aria-label="Go to other"]')).toBeNull()
    expect(host.querySelector('[aria-label="Show scores of other"]')).not.toBeNull()
  })
})
