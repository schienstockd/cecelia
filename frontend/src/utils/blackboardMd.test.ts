import { describe, it, expect } from 'vitest'
import {
  renderBlackboardMarkdown, mermaidBlocks, resolveBlackboardWikiLinks,
} from './blackboardMd'

describe('renderBlackboardMarkdown', () => {
  it('returns empty for empty input', () => {
    expect(renderBlackboardMarkdown('')).toBe('')
    expect(renderBlackboardMarkdown(undefined)).toBe('')
    expect(renderBlackboardMarkdown(null)).toBe('')
  })

  it('renders GFM basics', () => {
    const html = renderBlackboardMarkdown('# Title\n\n- one\n- two\n')
    expect(html).toContain('<h1')
    expect(html).toContain('<li>one</li>')
  })

  it('leaves mermaid fences as code blocks for the SFC to post-process', () => {
    const html = renderBlackboardMarkdown('```mermaid\ngraph TD\nA-->B\n```\n')
    // The SFC finds these via `.language-mermaid`. Marked emits either that class or a full
    // `<code class="language-mermaid">`; either is fine.
    expect(html).toContain('language-mermaid')
    expect(html).toContain('A--&gt;B')
  })
})

describe('mermaidBlocks', () => {
  it('extracts each fence body in order', () => {
    const src = '# Head\n```mermaid\ngraph LR\nA-->B\n```\nprose\n```mermaid\nflowchart\nX-->Y\n```\n'
    const blocks = mermaidBlocks(src)
    expect(blocks).toHaveLength(2)
    expect(blocks[0]).toContain('graph LR')
    expect(blocks[1]).toContain('flowchart')
  })

  it('ignores non-mermaid fences', () => {
    const src = '```ts\nconst x = 1\n```\n```mermaid\nA-->B\n```\n'
    expect(mermaidBlocks(src)).toHaveLength(1)
  })

  it('returns [] for empty input', () => {
    expect(mermaidBlocks('')).toEqual([])
    expect(mermaidBlocks(undefined)).toEqual([])
  })
})

describe('resolveBlackboardWikiLinks', () => {
  const titles = new Map<string, string>([
    ['bb-20260920T110010-b96579', 'Segmentation strategy for the bright cohort'],
    ['profile', 'Project profile'],
  ])

  it('rewrites a known bb-id to a markdown link with the title', () => {
    const out = resolveBlackboardWikiLinks(
      'See [[bb-20260920T110010-b96579]] for context.', titles)
    expect(out).toBe(
      'See [Segmentation strategy for the bright cohort](#bb:bb-20260920T110010-b96579) for context.')
  })

  it('rewrites the profile shorthand', () => {
    expect(resolveBlackboardWikiLinks('cf [[profile]]', titles))
      .toBe('cf [Project profile](#bb:profile)')
  })

  it('keeps unknown ids linkable (with a deleted? label)', () => {
    const out = resolveBlackboardWikiLinks('gone [[bb-20260101T000000-abcdef]]', titles)
    expect(out).toBe('gone [bb-20260101T000000-abcdef (deleted?)](#bb:bb-20260101T000000-abcdef)')
  })

  it('escapes brackets in the title so a rogue ] does not end the link early', () => {
    const t = new Map([['bb-20260101T000000-abcdef', 'Title with [square] brackets']])
    const out = resolveBlackboardWikiLinks('[[bb-20260101T000000-abcdef]]', t)
    expect(out).toBe('[Title with \\[square\\] brackets](#bb:bb-20260101T000000-abcdef)')
  })

  it('leaves malformed ids alone (guards against [[anything]] becoming a wiki-link)', () => {
    // Plain [[wiki]] with no bb-… shape must not match — it's an ordinary bracket pair.
    expect(resolveBlackboardWikiLinks('write [[notes]] here', titles))
      .toBe('write [[notes]] here')
  })
})

describe('renderBlackboardMarkdown — wiki-link integration', () => {
  it('renders a resolved link as an <a href="#bb:…">', () => {
    const titles = new Map([['bb-20260101T000000-abcdef', 'Cross-ref']])
    const html = renderBlackboardMarkdown('see [[bb-20260101T000000-abcdef]]', titles)
    expect(html).toContain('href="#bb:bb-20260101T000000-abcdef"')
    expect(html).toContain('Cross-ref')
  })

  it('leaves [[bb-…]] as literal text when no title map is passed', () => {
    const html = renderBlackboardMarkdown('see [[bb-20260101T000000-abcdef]]')
    expect(html).toContain('[[bb-20260101T000000-abcdef]]')
  })
})
