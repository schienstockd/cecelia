// The viewer header's Side by side in a real browser. A page that cannot move its window (native Wayland,
// a maximised window — and this test page, which no browser lets move itself) used to fail silently;
// now it names the system's own tiling keys once the placement has had its chance.
import { afterEach, describe, expect, it, vi } from 'vitest'
import { h } from 'vue'
import VwHeader from './VwHeader.vue'
import ToastHost from '../ToastHost.vue'
import { PLACE_VERDICT_MS, snapKeys } from '../../lib/popout'
import { frames, mountInto } from '../../test/browser/mount'

afterEach(() => vi.unstubAllGlobals())

describe('VwHeader side by side', () => {
  it('a window that did not move → a toast naming the keys to snap it right', async () => {
    vi.stubGlobal('opener', {})   // a popout the app opened is the only window offered the button
    const host = mountInto({ render: () => h('div', [h(VwHeader, {
      imageName: 'img', valueName: '', labels: '', labelsState: '', versions: [], activeVersion: '',
      projectUid: 'p', imageUid: 'i', setUid: 's', kiwiRef: () => null, kiwiTip: '' }), h(ToastHost)]) })
    await frames(2)
    const btn = host.querySelector<HTMLButtonElement>('[data-guide="viewer.sideBySide"]')
    expect(btn, 'side-by-side button').not.toBeNull()
    btn!.click()
    await new Promise(r => setTimeout(r, PLACE_VERDICT_MS + 200))
    await frames(2)
    expect(document.body.textContent).toContain(`Press ${snapKeys('right')} to put the viewer on the right`)
  })
})
