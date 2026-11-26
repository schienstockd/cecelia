// ChecksCluster in a real browser: collapsed by default (icon + count, the name in the tooltip) so the
// set row stays one line beside the gesture hints; its "Checks" label expands it, and the choice is kept.
import { afterEach, describe, expect, it } from 'vitest'
import { h } from 'vue'
import { userEvent } from 'vitest/browser'
import ChecksCluster from './ChecksCluster.vue'
import { frames, mountInto } from '../test/browser/mount'
import { profileStorage } from '../utils/profileStorage'

const mount = () => mountInto({ render: () => h(ChecksCluster, { setUid: 'nope', qcFilterOn: false }) })
const width = (host: HTMLElement) => host.querySelector<HTMLElement>('.checks')!.getBoundingClientRect().width

afterEach(() => profileStorage.removeItem('cc.checksOpen'))

describe('ChecksCluster collapse', () => {
  it('starts collapsed: the check names are gone from the row but kept as accessible names', async () => {
    const host = mount()
    await frames()
    const qc = [...host.querySelectorAll<HTMLButtonElement>('.check-btn')].find(b => b.getAttribute('aria-label') === 'QC')!
    expect(qc).toBeTruthy()
    expect(qc.textContent!.trim()).toBe('')
    expect(host.querySelector('.checks')!.textContent).not.toContain('Acquisition')
  })

  it('the label expands it — names back, the group wider — and the choice survives a remount', async () => {
    const host = mount()
    await frames()
    const narrow = width(host)
    await userEvent.click(host.querySelector<HTMLElement>('[data-guide="checks.toggle"]')!)
    await frames()
    expect(host.querySelector('.checks')!.textContent).toContain('Acquisition')
    expect(width(host)).toBeGreaterThan(narrow + 50)

    const again = mount()
    await frames()
    expect(again.querySelector('[data-guide="checks.toggle"]')!.getAttribute('aria-expanded')).toBe('true')
    expect(again.querySelector('.checks')!.textContent).toContain('Acquisition')
  })
})
