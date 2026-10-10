import { describe, it, expect } from 'vitest'
import { behindPhrase, mergedPrsMd, renderMarkdown } from './whatsNew'

describe('dev-channel update card', () => {
  it('says how far behind, with the PR count when there is one', () => {
    expect(behindPhrase(101, 39)).toBe('101 commits behind, 39 PRs merged')
    expect(behindPhrase(1, 1)).toBe('1 commit behind, 1 PR merged')
    expect(behindPhrase(3, 0)).toBe('3 commits behind')
  })

  it('says nothing when the gap is unknown', () => {
    expect(behindPhrase(null, 5)).toBe('')
    expect(behindPhrase(0, 0)).toBe('')
  })

  it('lists merged PRs one per line, capped', () => {
    const prs = [1, 2, 3].map(n => ({ number: n, title: `PR ${n}` }))
    expect(mergedPrsMd(prs)).toBe('- #1 PR 1\n- #2 PR 2\n- #3 PR 3')
    expect(mergedPrsMd(prs, 2)).toBe('- #1 PR 1\n- #2 PR 2\n- …and 1 more')
    expect(mergedPrsMd([])).toBe('')
  })

  it('renders as a list with titles kept as text', () => {
    const html = renderMarkdown(mergedPrsMd([{ number: 7, title: 'fix <div> leak' }]))
    expect(html).toContain('<li>#7 fix &lt;div&gt; leak</li>')
    expect(html).not.toContain('<div>')
  })
})
