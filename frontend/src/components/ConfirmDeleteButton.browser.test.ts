// The app's one arm→confirm affordance (ConfirmDeleteButton over ConfirmButton), with real clicks:
// the first arms, the second fires, a click elsewhere or the timeout disarms. docs/todo/GUI_TESTING_PLAN.md, P3b.
import { describe, expect, it } from 'vitest'
import { h, ref } from 'vue'
import { userEvent } from 'vitest/browser'
import ConfirmDeleteButton from './ConfirmDeleteButton.vue'
import { frames, mountInto } from '../test/browser/mount'

function setup(props: Record<string, unknown> = {}) {
  const fired = ref(0)
  const host = mountInto({
    render: () => h('div', [
      h(ConfirmDeleteButton, { onConfirm: () => { fired.value++ }, ...props }),
      h('button', { class: 't-elsewhere' }, 'elsewhere'),
    ]),
  })
  return { fired, btn: () => host.querySelector<HTMLElement>('.cc-del')!, host }
}

describe('ConfirmDeleteButton', () => {
  it('first click arms, second click fires once and disarms', async () => {
    const { fired, btn } = setup()
    await userEvent.click(btn())
    expect([btn().classList.contains('armed'), fired.value]).toEqual([true, 0])
    await userEvent.click(btn())
    expect([btn().classList.contains('armed'), fired.value]).toEqual([false, 1])
  })

  it('a click elsewhere disarms it, so the next click only arms again', async () => {
    const { fired, btn, host } = setup()
    await userEvent.click(btn())
    await userEvent.click(host.querySelector('.t-elsewhere')!)
    expect(btn().classList.contains('armed')).toBe(false)
    await userEvent.click(btn())
    expect(fired.value).toBe(0)
  })

  it('it disarms by itself after autoDismissMs', async () => {
    const { btn } = setup({ autoDismissMs: 150 })
    await userEvent.click(btn())
    expect(btn().classList.contains('armed')).toBe(true)
    await new Promise(r => setTimeout(r, 250))   // the timeout IS the behaviour under test
    await frames()
    expect(btn().classList.contains('armed')).toBe(false)
  })

  it('needsConfirm=false fires on the first click', async () => {
    const { fired, btn } = setup({ needsConfirm: false })
    await userEvent.click(btn())
    expect(fired.value).toBe(1)
  })
})
