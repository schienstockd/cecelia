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

describe('renderBlackboardMarkdown — wiki-link resolver', () => {
  const titles = new Map<string, string>([
    ['bb-20260920T110010-b96579', 'Segmentation strategy for the bright cohort'],
    ['profile', 'Project profile'],
  ])

  it('rewrites a known bb-id to <a href="#bb:…"> with the target title', () => {
    const html = renderBlackboardMarkdown(
      'See [[bb-20260920T110010-b96579]] for context.', titles)
    expect(html).toContain('href="#bb:bb-20260920T110010-b96579"')
    expect(html).toContain('Segmentation strategy for the bright cohort')
  })

  it('rewrites the profile shorthand', () => {
    const html = renderBlackboardMarkdown('cf [[profile]]', titles)
    expect(html).toContain('href="#bb:profile"')
    expect(html).toContain('Project profile')
  })

  it('links an unknown id with a "(deleted?)" label', () => {
    const html = renderBlackboardMarkdown('gone [[bb-20260101T000000-abcdef]]', titles)
    expect(html).toContain('href="#bb:bb-20260101T000000-abcdef"')
    expect(html).toContain('bb-20260101T000000-abcdef (deleted?)')
  })

  it('escapes HTML in a title so a &lt;script&gt; in an entry title cannot execute', () => {
    const t = new Map([['bb-20260101T000000-abcdef', 'Title <script>x</script>']])
    const html = renderBlackboardMarkdown('[[bb-20260101T000000-abcdef]]', t)
    expect(html).toContain('&lt;script&gt;')
    expect(html).not.toContain('<script>')
  })

  it('leaves malformed [[wiki]] pairs alone (only bb-<ts>-<hex> and profile match)', () => {
    const html = renderBlackboardMarkdown('write [[notes]] here', titles)
    expect(html).toContain('[[notes]]')
    expect(html).not.toContain('href="#bb:')
  })

  it('leaves [[bb-…]] inside a fenced code block untouched', () => {
    const html = renderBlackboardMarkdown(
      '```\nsee [[bb-20260920T110010-b96579]] inside code\n```', titles)
    // The wiki-token would have consumed the brackets; a plain string means the extension didn't fire.
    expect(html).toContain('[[bb-20260920T110010-b96579]]')
    expect(html).not.toContain('href="#bb:')
  })

  it('leaves [[bb-…]] inside an inline `codespan` untouched', () => {
    const html = renderBlackboardMarkdown(
      'inline `[[bb-20260920T110010-b96579]]` reference', titles)
    expect(html).toContain('[[bb-20260920T110010-b96579]]')
    expect(html).not.toContain('href="#bb:')
  })

  it('leaves [[bb-…]] as literal text when no title map is passed', () => {
    const html = renderBlackboardMarkdown('see [[bb-20260101T000000-abcdef]]')
    expect(html).toContain('[[bb-20260101T000000-abcdef]]')
    expect(html).not.toContain('href="#bb:')
  })

  it('handles two wiki links on the same line', () => {
    const html = renderBlackboardMarkdown(
      'compare [[bb-20260920T110010-b96579]] with [[profile]]', titles)
    expect(html).toContain('href="#bb:bb-20260920T110010-b96579"')
    expect(html).toContain('href="#bb:profile"')
  })
})
