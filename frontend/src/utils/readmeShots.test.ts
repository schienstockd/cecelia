import { describe, expect, it } from 'vitest'
import { renderReadmeShots, spliceReadme } from './readmeShots'

// The composed screenshots' widths and the fixture's facts, written by scripts/docshots/compose.mjs.
const META = import.meta.glob('../../../user-docs/images/{images,facts}.json', { import: 'default', eager: true }) as
  Record<string, Record<string, unknown>>
const widths = (META['../../../user-docs/images/images.json'] ?? {}) as Record<string, number>
const facts = ((META['../../../user-docs/images/facts.json'] ?? {}) as Record<string, Record<string, number | string>>).readme ?? {}
const README = Object.values(import.meta.glob('../../../README.md', { query: '?raw', import: 'default', eager: true }))[0] as string

describe('README.md screenshots', () => {
  // Stale ⇒ `npx vitest run src/utils/readmeShots.test.ts -u` and commit README.md (only its block
  // changes). Edit the shots and captions in user-docs/shots/readme.ts, the layout in utils/readmeShots.ts.
  it('the README block is the generated one', async () => {
    await expect(spliceReadme(README, renderReadmeShots(facts, widths))).toMatchFileSnapshot('../../../README.md')
  })

  it('captions carry the fixture numbers, and a missing one fails', () => {
    const block = renderReadmeShots(facts, widths)
    expect(block).toContain(`its ${facts['tcells.TKufLV.Tcells.tracks']} tracks drawn as ribbons.`)
    // the public section cites the data source the fixture read off its record
    expect(block).toContain(`Data: ${facts['hubmap.2dR91B.citation']}, doi:${facts['hubmap.2dR91B.doi']}.`)
    expect(() => renderReadmeShots({}, widths)).toThrow(/no measured/)
  })

  it('the first shot and a heroRow shot share the top row, each with its own caption', () => {
    const top = renderReadmeShots(facts, widths).split('\n')[1]
    // side by side at equal widths, not a table (the mobile app sizes table columns by their text)
    expect(top.startsWith('<p align="center">')).toBe(true)
    expect(top).not.toContain('<table')
    expect(top.match(/<img [^>]*width="49%"/g)?.length).toBe(2)
    expect(top).toContain('<b>Left:</b> A PhenoCycler')
    expect(top).toContain('<b>Right:</b> A two-photon')
    expect(top.indexOf('readme-lens')).toBeLessThan(top.indexOf('readme-viewer'))
  })

  it('splices only between the markers', () => {
    const r = 'a\n<!-- docshots:start x -->\nold\n<!-- docshots:end -->\nb'
    expect(spliceReadme(r, 'NEW')).toBe('a\nNEW\nb')
    expect(() => spliceReadme('no block', 'x')).toThrow()
  })
})
