// The table import dialog's two halves a user reads before Apply: the layout guide (before a table) and
// "Read as" (each pasted column's role), in a real browser. Logic is in utils/attrTable.test.ts; this
// checks the dialog shows it and that clicking a column makes it the image column.
import { describe, expect, it } from 'vitest'
import { h } from 'vue'
import { userEvent } from 'vitest/browser'
import AttrTableDialog from './AttrTableDialog.vue'
import { frames, mountInto } from '../test/browser/mount'
import type { TableImage } from '../utils/attrTable'

const IMAGES: TableImage[] = ['Core A1.oir', 'Core A2.oir', 'Core B1.oir']
  .map((name, i) => ({ uid: `u${i}`, name, oriPath: null, attr: (i ? {} : { Tissue: 'Liver' }) as Record<string, string> }))

const mount = () => mountInto(() => h(AttrTableDialog, { projectUid: 'p', setUid: 's', images: IMAGES }))
const roles = () => [...document.querySelectorAll<HTMLElement>('.at-raw thead th .at-role')].map(e => e.textContent?.trim())

describe('AttrTableDialog', () => {
  it('before a table: the guide shows the set\'s own images as the example', async () => {
    mount()
    await frames()
    const guide = document.querySelector('.at-guide')!
    expect(guide).not.toBeNull()
    expect(guide.textContent).toContain('One column names the image')
    expect([...guide.querySelectorAll('tbody td:first-child')].map(e => e.textContent)).toEqual(IMAGES.map(i => i.name))
    expect([...guide.querySelectorAll('thead th')].map(e => e.textContent)).toEqual(['ImageName', 'AttributeTissue'])
  })

  it('after a table: each column\'s role, and a click on a column matches by it', async () => {
    mount()
    await frames()
    const ta = document.querySelector<HTMLTextAreaElement>('.at-text')!
    await userEvent.fill(ta, 'Tissue\tCore\t\nSpleen\tCore A2\tx\nLung\tCore B1\ty\n')
    await frames()
    expect(document.querySelector('.at-guide')).toBeNull()
    expect(roles()).toEqual(['Tissue', 'Image · Name', 'Not used · no header'])
    expect(document.querySelector('.cc-readout')?.textContent).toContain('Tab-separated · 2 row(s) · 3 column(s)')

    document.querySelectorAll<HTMLElement>('.at-raw thead th')[0].click()
    await frames()
    expect(roles()).toEqual(['Image · Name', 'Core · new', 'Not used · no header'])
  })
})
