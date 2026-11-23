// Two real canvas panels in a browser: with the canvas's Snap on, a panel dragged to just beside another
// lands touching it with the tops lined up; with Snap off it stays exactly where it was dropped.
import { describe, expect, it } from 'vitest'
import { defineComponent, h, provide } from 'vue'
import CanvasPanel from './CanvasPanel.vue'
import { CANVAS_SNAP_KEY } from '../../composables/useFloatingPanel'
import { frames, mountInto } from '../../test/browser/mount'

function canvas(snap: boolean) {
  const Host = defineComponent({
    setup() {
      provide(CANVAS_SNAP_KEY, () => snap)
      return () => h('div', { style: 'position:relative;width:1400px;height:900px' }, [
        h(CanvasPanel, { index: 0, title: 'A' }, () => 'a'),
        h(CanvasPanel, { index: 1, title: 'B' }, () => 'b'),
      ])
    },
  })
  return mountInto(Host)
}

async function dragBy(head: HTMLElement, dx: number, dy: number) {
  const r = head.getBoundingClientRect(), x = r.left + 20, y = r.top + 5
  head.dispatchEvent(new MouseEvent('mousedown', { bubbles: true, clientX: x, clientY: y }))
  window.dispatchEvent(new MouseEvent('mousemove', { clientX: x + dx, clientY: y + dy }))
  window.dispatchEvent(new MouseEvent('mouseup', { clientX: x + dx, clientY: y + dy }))
  await frames(2)
}

describe('CanvasPanel snap', () => {
  for (const snap of [true, false]) {
    it(snap ? 'Snap on: dropped beside A → touching it, tops level' : 'Snap off: stays where dropped', async () => {
      const host = canvas(snap)
      await frames(2)
      const [a, b] = [...host.querySelectorAll<HTMLElement>('.panel')]
      // A sits at its stagger (16,16); move B to ~7 px right of A's right edge and 6 px lower
      const wantX = a!.offsetLeft + a!.offsetWidth, wantY = a!.offsetTop
      await dragBy(b!.querySelector<HTMLElement>('.panel-head')!, wantX + 7 - b!.offsetLeft, wantY + 6 - b!.offsetTop)
      if (snap) expect([b!.offsetLeft, b!.offsetTop]).toEqual([wantX, wantY])
      else expect([b!.offsetLeft, b!.offsetTop]).toEqual([wantX + 7, wantY + 6])
    })
  }
})
