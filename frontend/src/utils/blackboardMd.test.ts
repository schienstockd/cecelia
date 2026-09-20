import { describe, it, expect } from 'vitest'
import { renderBlackboardMarkdown, mermaidBlocks } from './blackboardMd'

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
