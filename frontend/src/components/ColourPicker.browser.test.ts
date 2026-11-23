// ColourPicker in a real browser: its popover content sits at the shell's padding, not twice it.
// TeleportPopover pads its content; a content root that padded itself as well looked roomier than
// every other popover. The repo-wide rule is utils/popoverPadding.test.ts; this measures one on screen.
import { describe, expect, it } from 'vitest'
import { h, ref } from 'vue'
import ColourPicker from './ColourPicker.vue'
import { frames, mountInto } from '../test/browser/mount'

describe('ColourPicker popover', () => {
  it('content inset equals the shell padding — not doubled', async () => {
    const v = ref('#ff0000')
    const host = mountInto({ render: () => h('div', { style: 'margin:40px' },
      [h(ColourPicker, { modelValue: v.value, 'onUpdate:modelValue': (x: string) => { v.value = x } })]) })
    host.querySelector<HTMLButtonElement>('.cp-swatch')!.click()
    await frames(3)
    const shell = document.querySelector<HTMLElement>('.cc-popover')!
    const grid = shell.querySelector<HTMLElement>('.cp-grid')!
    const cs = getComputedStyle(shell)
    const inset = grid.getBoundingClientRect().left - shell.getBoundingClientRect().left - parseFloat(cs.borderLeftWidth)
    expect(parseFloat(cs.paddingLeft)).toBeGreaterThan(0)
    expect(inset).toBeCloseTo(parseFloat(cs.paddingLeft), 0)
  })
})
