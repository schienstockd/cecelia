// SelectionTable's pinned header and frozen columns, measured in a real browser.
//
// Both are placed by more than one mechanism — CSS sticky, the `fit` wrapper that traps it, the hand
// lift (`utils/stickyHead.ts`), a pinned CollapsibleSection bar above (`--cc-sticky-above`), a padded
// modal body (`--cc-scroll-pad-top`) — and three times those disagreed in a way only an eye caught
// (#2361 the last). Each scenario here is one of those arrangements. Expected positions come from the
// scroll box's own geometry, never from the code under test. docs/todo/GUI_TESTING_PLAN.md, PR 1.
import { describe, expect, it } from 'vitest'
import { h, ref, type VNode } from 'vue'
import { userEvent } from 'vitest/browser'
import SelectionTable from './SelectionTable.vue'
import CollapsibleSection from './CollapsibleSection.vue'
import BaseModal from './BaseModal.vue'
import { frames, mountInto, scrollTo } from '../test/browser/mount'

const ROWS = Array.from({ length: 40 }, (_, i) => {
  const row: Record<string, string> = { id: `r${i}`, name: `row ${i}` }
  for (let c = 0; c < 14; c++) row[`c${c}`] = `${i}.${c}`
  return row
})
const NARROW = [
  { key: 'name', label: 'Name', sticky: true, width: 160 },
  { key: 'c0', label: 'A', width: 100 },
  { key: 'c1', label: 'B', width: 100 },
]
// wider than the 600px box, so `fit="content"` puts the table in a sideways scroller
const WIDE = [
  { key: 'name', label: 'Name', sticky: true, width: 160 },
  ...Array.from({ length: 14 }, (_, c) => ({ key: `c${c}`, label: `Col ${c}`, width: 120 })),
]

let keyN = 0
function table(columns: typeof NARROW, fit: 'fill' | 'content'): VNode {
  return h(SelectionTable, {
    columns, rows: ROWS, idKey: 'id', selectionMode: 'multi', selected: [],
    columnWidthKey: `gui-test-${keyN++}`, fit, stickyHeader: true,
  })
}
// the page's own vertical scroll box: fixed size so every run measures the same layout
const box = (child: VNode) => h('div', { class: 't-box', style: 'height:300px;width:600px;overflow-y:auto' }, [child])
// what ImageTable does around a wide table (`.table-scroll`)
const sideways = (child: VNode) => h('div', { style: 'overflow-x:auto;width:100%' }, [child])

const top = (el: Element) => el.getBoundingClientRect().top
const els = (host: HTMLElement) => {
  const tableEl = host.querySelector<HTMLTableElement>('table.sel-table')!
  return {
    tableEl,
    th: tableEl.querySelector<HTMLElement>('thead th')!,
    frozenTh: tableEl.querySelector<HTMLElement>('thead th.sel-sticky:not(.sel-sticky-pick)')!,
    frozenTd: () => {
      // the first body row still below the header
      const headBottom = tableEl.tHead!.getBoundingClientRect().bottom
      const tr = [...tableEl.tBodies[0].rows].find(r => r.getBoundingClientRect().top >= headBottom)!
      return tr.querySelector<HTMLElement>('td.sel-sticky:not(.sel-sticky-pick)')!
    },
  }
}
/** The line a pinned header must sit on: the scroll box's padding-box top, plus a pinned bar's height. */
const pinLine = (scroller: HTMLElement, bar?: HTMLElement) =>
  top(scroller) + scroller.clientTop + (bar ? bar.getBoundingClientRect().height : 0)

async function checkPinned(host: HTMLElement, scroller: HTMLElement, bar?: HTMLElement) {
  const { tableEl, th } = els(host)
  await frames()
  // at rest the header is in the table's flow — not pushed into the rows
  expect(top(th)).toBeCloseTo(top(tableEl), 0)
  // scrolled: on the pin line, whatever the arrangement
  await scrollTo(scroller, 300)
  expect(Math.abs(top(th) - pinLine(scroller, bar))).toBeLessThanOrEqual(1)
  // scrolled past the end: it leaves with the table, never below its last row
  await scrollTo(scroller, 5000)
  expect(th.getBoundingClientRect().bottom).toBeLessThanOrEqual(tableEl.getBoundingClientRect().bottom + 1)
}

async function checkFrozen(host: HTMLElement, scroller: HTMLElement) {
  const { frozenTh, frozenTd } = els(host)
  await scrollTo(scroller, 300)
  const wrapper = host.querySelector<HTMLElement>('table.sel-table')!.parentElement!
  await scrollTo(wrapper, wrapper.scrollTop, 400)
  expect(wrapper.scrollLeft).toBeGreaterThan(0)              // the scenario really scrolled sideways
  expect(Math.abs(frozenTh.getBoundingClientRect().left - frozenTd().getBoundingClientRect().left)).toBeLessThanOrEqual(1)
}

describe('SelectionTable stickyHeader', () => {
  it('plain: pins to the scroll box it sits in (CSS sticky)', async () => {
    const host = mountInto({ render: () => box(table(NARROW, 'fill')) })
    await checkPinned(host, host.querySelector('.t-box')!)
  })

  it('inside an overflow-x wrapper: pins to the outer box, which is the one that scrolls', async () => {
    const host = mountInto({ render: () => box(sideways(table(WIDE, 'content'))) })
    const scroller = host.querySelector<HTMLElement>('.t-box')!
    await checkPinned(host, scroller)
    await checkFrozen(host, scroller)
  })

  it('under a pinned CollapsibleSection bar: stops below the bar, not under it or a bar-height lower', async () => {
    const host = mountInto({
      render: () => box(h(CollapsibleSection, { label: 'Images', maxHeight: 'none', sticky: true },
        () => sideways(table(WIDE, 'content')))),
    })
    const scroller = host.querySelector<HTMLElement>('.t-box')!
    await checkPinned(host, scroller, host.querySelector<HTMLElement>('.cs-toggle')!)
    await checkFrozen(host, scroller)
  })

  it('in a padded BaseModal body: no strip of rows above the header', async () => {
    const host = mountInto({ render: () => h(BaseModal, { title: 'Pick', height: '400px' }, () => table(NARROW, 'fill')) })
    const body = host.querySelector<HTMLElement>('.cc-modal-body')!
    expect(parseFloat(getComputedStyle(body).paddingTop)).toBeGreaterThan(0)   // the scenario has padding
    expect(body.scrollHeight).toBeGreaterThan(body.clientHeight)              // …and really scrolls
    await checkPinned(host, body)
  })
})

// ── Widths: what `fit` does with the declared column widths (P3a) ─────────────────────────────────
const width = (el: Element) => el.getBoundingClientRect().width
const pickWidth = (host: HTMLElement) => width(host.querySelector('thead th.sel-sticky-pick')!)
const headCell = (host: HTMLElement, label: string) =>
  [...host.querySelectorAll<HTMLElement>('thead th')].find(th => th.textContent?.trim().startsWith(label))!
const FILL_COLS = [
  { key: 'name', label: 'Name', sticky: true, width: 160 },
  { key: 'c0', label: 'Fixed', width: 100, fixed: true },
  { key: 'c1', label: 'Last', width: 100 },
]
const sized = (columns: typeof FILL_COLS, fit: 'fill' | 'content', extra: Record<string, unknown> = {}) =>
  h(SelectionTable, { columns, rows: ROWS.slice(0, 5), idKey: 'id', selectionMode: 'multi', selected: [],
    columnWidthKey: `gui-test-${keyN++}`, fit, ...extra })

describe('SelectionTable fit', () => {
  it('fill: the table is the container width, and the spare goes to the last resizable column only', async () => {
    const host = mountInto({ render: () => h('div', { style: 'width:900px' }, [sized(FILL_COLS, 'fill')]) })
    await frames(3)
    expect(width(host.querySelector('table')!)).toBeCloseTo(900, 0)
    expect(width(headCell(host, 'Fixed'))).toBeCloseTo(100, 0)            // `fixed` keeps its width
    expect(width(headCell(host, 'Name'))).toBeCloseTo(160, 0)
    expect(width(headCell(host, 'Last'))).toBeCloseTo(900 - pickWidth(host) - 160 - 100, 0)
  })

  it('content: never squeezed below the declared widths in a narrow box, still fills a wide one', async () => {
    const narrow = mountInto({ render: () => h('div', { style: 'width:300px;overflow-x:auto' }, [sized(FILL_COLS, 'content')]) })
    await frames(3)
    expect(width(headCell(narrow, 'Name'))).toBeCloseTo(160, 0)
    expect(width(headCell(narrow, 'Fixed'))).toBeCloseTo(100, 0)
    expect(width(narrow.querySelector('table')!)).toBeCloseTo(pickWidth(narrow) + 360, 0)
    const wide = mountInto({ render: () => h('div', { style: 'width:1200px' }, [sized(FILL_COLS, 'content')]) })
    await frames(3)
    expect(width(wide.querySelector('table')!)).toBeCloseTo(1200, 0)
  })
})

describe('SelectionTable column resize', () => {
  it("dragging a header's right edge widens that column by the drag, and a remount keeps it", async () => {
    const key = `gui-test-resize-${keyN++}`
    // narrower than the columns: a content table in a WIDER box fills it, spreading the spare over them
    const render = () => h('div', { style: 'width:400px;overflow-x:auto' },
      [h(SelectionTable, { columns: FILL_COLS, rows: ROWS.slice(0, 5), idKey: 'id', selectionMode: 'multi',
        selected: [], columnWidthKey: key, fit: 'content' })])
    const host = mountInto({ render })
    await frames(3)
    const name = headCell(host, 'Name')
    const grip = name.querySelector<HTMLElement>('.sel-col-resize')!
    const r = grip.getBoundingClientRect()
    const at = (x: number) => ({ bubbles: true, clientX: x, clientY: r.top + 2, button: 0 })
    grip.dispatchEvent(new MouseEvent('mousedown', at(r.left + 2)))
    document.dispatchEvent(new MouseEvent('mousemove', at(r.left + 62)))
    document.dispatchEvent(new MouseEvent('mouseup', at(r.left + 62)))
    await frames(3)
    expect(width(name)).toBeCloseTo(220, 0)
    // the frozen column's body cells moved with it
    const td = host.querySelector<HTMLElement>('tbody td.sel-sticky:not(.sel-sticky-pick)')!
    expect(width(td)).toBeCloseTo(220, 0)
    const again = mountInto({ render })
    await frames(3)
    expect(width(headCell(again, 'Name'))).toBeCloseTo(220, 0)
  })
})

// ── Picking rows with a real pointer (P3a) ────────────────────────────────────────────────────────
describe('SelectionTable picking', () => {
  function pickable() {
    const selected = ref<string[]>([])
    const pressed = ref(0)
    const host = mountInto({
      render: () => h(SelectionTable, {
        columns: NARROW, rows: ROWS.slice(0, 8), idKey: 'id', selectionMode: 'multi',
        selected: selected.value, 'onUpdate:selected': (v: string[]) => { selected.value = v },
      }, {
        // `#actions` is the documented home of row buttons: "clicks inside it do NOT select the row".
        // A `#cell-<key>` button stops its own click (ImageTable's do) — the table doesn't promise it.
        actions: ({ row }: { row: Record<string, string> }) =>
          h('button', { class: 't-btn', 'data-id': row.id, onClick: () => { pressed.value++ } }, 'go'),
      }),
    })
    const cell = (id: string) => host.querySelector<HTMLElement>(`tr[data-sel-id="${id}"] td:nth-child(2)`)!
    return { host, selected, pressed, cell }
  }

  it('click selects that row only; ctrl-click adds; shift-click takes the range from the anchor', async () => {
    const { selected, cell } = pickable()
    await frames()
    await userEvent.click(cell('r1'))
    expect(selected.value).toEqual(['r1'])
    await userEvent.click(cell('r3'), { modifiers: ['Control'] })
    expect([...selected.value].sort()).toEqual(['r1', 'r3'])
    await userEvent.click(cell('r1'))
    await userEvent.click(cell('r4'), { modifiers: ['Shift'] })
    expect(selected.value).toEqual(['r1', 'r2', 'r3', 'r4'])
  })

  it('a button in the #actions cell does its own thing and leaves the selection alone', async () => {
    const { host, selected, pressed, cell } = pickable()
    await frames()
    await userEvent.click(cell('r2'))
    await userEvent.click(host.querySelector<HTMLElement>('.t-btn[data-id="r5"]')!)
    expect(pressed.value).toBe(1)
    expect(selected.value).toEqual(['r2'])
  })

  it('pressing on a row and dragging sweeps the rows passed over', async () => {
    const { selected, cell } = pickable()
    await frames()
    await userEvent.dragAndDrop(cell('r1'), cell('r4'))
    expect(selected.value).toEqual(['r1', 'r2', 'r3', 'r4'])
  })
})
