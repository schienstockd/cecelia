// PlotPicker's previews look in a real browser: nothing that hovering shows may cover where the pointer
// goes next — a kind's tip used to open over the cards beside the column, a card's over the card below.
// A kind's blurb now heads its cards instead.
import { describe, expect, it } from 'vitest'
import { h } from 'vue'
import { page, userEvent } from 'vitest/browser'
import PlotPicker from './PlotPicker.vue'
import { frames, mountInto } from '../../test/browser/mount'
import { PLOT_CATEGORIES, PLOT_SKETCH_KINDS, type PlotOption } from '../../plots/plotPicker'
import { useSettingsStore } from '../../stores/settings'
import { specOption } from '../../plots/plotPicker'
import { INTERACTIVE_VIEWS, viewOption } from './interactiveViews'
import { CLUSTER_PANELS } from '../../modules/cluster/clusterPanels'

const sketch = PLOT_SKETCH_KINDS[0]
const options: PlotOption[] = [
  { value: 'a', label: 'Box plot', answers: 'How does a measure differ between the populations here?', category: 'compare', sketch },
  { value: 'b', label: 'Violin', answers: 'What does the spread of a measure look like per group?', category: 'compare', sketch },
  { value: 'c', label: 'Strip', answers: 'Which points sit where?', category: 'compare', sketch },
  { value: 'd', label: 'Line', answers: 'How does a measure change over the movie, frame by frame?', category: 'time', sketch },
]

function mount() {
  return mountInto({
    setup() { useSettingsStore().plotPickerPreviews = true },
    render: () => h(PlotPicker, { options }),
  })
}

const overlaps = (a: DOMRect, b: DOMRect) =>
  a.left < b.right && b.left < a.right && a.top < b.bottom && b.top < a.bottom
const tip = () => document.querySelector<HTMLElement>('.cc-tooltip')

async function openPicker(host: HTMLElement) {
  await userEvent.click(host.querySelector<HTMLElement>('.plot-picker-btn')!)
  await frames()
}

describe('PlotPicker hover never covers the next level', () => {
  it('a kind shows no tip over the cards; its blurb heads them', async () => {
    const host = mount()
    await openPicker(host)
    const kinds = [...document.querySelectorAll<HTMLElement>('.pp-kind')]
    expect(kinds).toHaveLength(2)
    await userEvent.hover(kinds[1])
    await new Promise(r => setTimeout(r, 250))   // past the hover-intent pause
    await frames()
    const cards = document.querySelector<HTMLElement>('.pp-cards')!.getBoundingClientRect()
    const t = tip()
    if (t) expect(overlaps(t.getBoundingClientRect(), cards)).toBe(false)
    const time = PLOT_CATEGORIES.find(c => c.id === 'time')!
    expect(document.querySelector('.pp-blurb')!.textContent).toBe(time.blurb)
  })

  it('a card shows no tip over its neighbours', async () => {
    const host = mount()
    await openPicker(host)
    const cards = [...document.querySelectorAll<HTMLElement>('.pp-card')]
    expect(cards).toHaveLength(3)
    await userEvent.hover(cards[0])
    await frames()
    const t = tip()
    if (t) for (const c of cards.slice(1)) expect(overlaps(t.getBoundingClientRect(), c.getBoundingClientRect())).toBe(false)
  })
})

// Every real plot's question line fits its card (two lines) — a longer one is cut off mid-sentence.
type SpecJson = { id: string; label: string; answers?: string; category?: string; sketch?: string }
const SPECS = Object.values(import.meta.glob<SpecJson>('../../../../app/src/plotDefinitions/*.json',
                                                       { eager: true, import: 'default' }))
const ALL: PlotOption[] = [
  ...SPECS.map(s => specOption(s)),
  ...Object.entries(INTERACTIVE_VIEWS).map(([k, v]) => ({ ...viewOption(k, v), value: k })),
  ...Object.entries(CLUSTER_PANELS).map(([k, v]) => ({ ...viewOption(k, v), value: k })),
]

describe('PlotPicker card text', () => {
  // a full-width window and a narrow one (a small laptop window or zoomed-in browser)
  it.each([1280, 560])('every real plot\'s question fits its card at %ipx', async (w) => {
    await page.viewport(w, 800)
    expect(SPECS.length).toBeGreaterThan(10)
    const host = mountInto({
      setup() { useSettingsStore().plotPickerPreviews = true },
      render: () => h(PlotPicker, { options: ALL }),
    })
    await openPicker(host)
    const clipped: string[] = []
    for (const kind of document.querySelectorAll<HTMLElement>('.pp-kind')) {
      await userEvent.click(kind)
      await frames()
      for (const a of document.querySelectorAll<HTMLElement>('.pp-card-answers'))
        if (a.scrollHeight > a.clientHeight + 1) clipped.push(a.textContent ?? '')
    }
    expect(clipped).toEqual([])
  })
})

// Every kind's blurb is one line above its cards — a second line pushes the cards down and reads as a
// paragraph. The desktop's system font differs from this browser's (the old "Clusters & matrices" blurb
// measured 291px in Ubuntu Sans for ~290px of room), so a blurb must fit with room to spare.
const BLURB_HEADROOM = 0.85

describe('PlotPicker kind blurbs', () => {
  it.each([1280, 560])('every kind\'s blurb fits one line at %ipx', async (w) => {
    await page.viewport(w, 800)
    // one option per category, so every kind (plugin 'other' included) is in the column
    const perKind: PlotOption[] = PLOT_CATEGORIES.map(c => ({ value: c.id, label: c.label, category: c.id, sketch }))
    const host = mountInto({
      setup() { useSettingsStore().plotPickerPreviews = true },
      render: () => h(PlotPicker, { options: perKind }),
    })
    await openPicker(host)
    const kinds = [...document.querySelectorAll<HTMLElement>('.pp-kind')]
    expect(kinds).toHaveLength(PLOT_CATEGORIES.length)
    const tooLong: string[] = []
    for (const kind of kinds) {
      await userEvent.click(kind)
      await frames()
      const b = document.querySelector<HTMLElement>('.pp-blurb')!
      const cs = getComputedStyle(b)
      const avail = b.clientWidth - parseFloat(cs.paddingLeft) - parseFloat(cs.paddingRight)
      const range = document.createRange()
      range.selectNodeContents(b)
      const lines = new Set([...range.getClientRects()].map(r => Math.round(r.top))).size
      b.style.whiteSpace = 'nowrap'
      const natural = range.getBoundingClientRect().width
      b.style.whiteSpace = ''
      if (lines > 1 || natural > avail * BLURB_HEADROOM)
        tooLong.push(`${b.textContent} (${Math.round(natural)} of ${Math.round(avail)}px)`)
    }
    expect(tooLong).toEqual([])
  })
})
