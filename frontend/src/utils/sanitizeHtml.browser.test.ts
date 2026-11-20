// Security audit F2: markdown from outside this build reaches `v-html` — a blackboard entry (imported
// bundle, a steered Claude session) and release notes. DOMPurify only sanitizes where there is a DOM,
// so this runs in real Chromium — the Node suite would see the input handed back untouched.
import { describe, it, expect } from 'vitest'
import { renderBlackboardMarkdown } from './blackboardMd'
import { renderMarkdown } from '../lib/whatsNew'

const titles = new Map([['bb-20261120T101500-abc123', 'Pipeline notes']])

function render(md: string): HTMLDivElement {
  const div = document.createElement('div')
  div.innerHTML = renderBlackboardMarkdown(md, titles)
  return div
}

describe('renderBlackboardMarkdown sanitizes (browser)', () => {
  it('drops script tags and inline event handlers', () => {
    const el = render('hi\n\n<script>window.__pwn = 1</script>\n\n<img src="x" onerror="window.__pwn = 2">\n')
    expect(el.querySelector('script')).toBeNull()
    expect(el.querySelector('img')?.getAttribute('onerror')).toBeNull()
    expect((window as unknown as { __pwn?: number }).__pwn).toBeUndefined()
  })

  it('drops javascript: links from markdown and raw HTML', () => {
    const el = render('[click](javascript:alert(1)) <a href="javascript:alert(2)">raw</a>')
    for (const a of el.querySelectorAll('a')) {
      expect(a.getAttribute('href') ?? '').not.toMatch(/^\s*javascript:/i)
    }
  })

  it('drops iframes, forms and svg script payloads', () => {
    const el = render('<iframe src="https://example.org"></iframe><form action="/api/repl"><button>x</button></form><svg><script>1</script></svg>')
    expect(el.querySelector('iframe')).toBeNull()
    expect(el.querySelector('form')).toBeNull()
    expect(el.querySelector('svg script')).toBeNull()
  })

  it('keeps what an entry legitimately uses', () => {
    const el = render([
      '# Title', '', '| a | b |', '|---|---|', '| 1 | 2 |', '',
      'see [[bb-20261120T101500-abc123]] and [docs](https://example.org)', '',
      '<details><summary>more</summary>body</details>', '',
      '- [x] done', '',
      '```mermaid', 'graph TD', 'A-->B', '```',
    ].join('\n'))
    expect(el.querySelector('h1')?.textContent).toBe('Title')
    expect(el.querySelector('table')).not.toBeNull()
    expect(el.querySelector('a[href="#bb:bb-20261120T101500-abc123"]')?.textContent).toBe('Pipeline notes')
    expect(el.querySelector('a[href="https://example.org"]')).not.toBeNull()
    expect(el.querySelector('details summary')?.textContent).toBe('more')
    expect(el.querySelector('li input[type="checkbox"]')).not.toBeNull()
    // the SFC swaps these for rendered SVG — the class must survive sanitizing
    expect(el.querySelector('code.language-mermaid')).not.toBeNull()
  })
})

describe('What\'s New release notes are sanitized (browser)', () => {
  it('drops script and handlers but keeps the notes', () => {
    const div = document.createElement('div')
    div.innerHTML = renderMarkdown('## Fixes\n\n- <img src=x onerror="window.__pwn = 3"> [x](javascript:alert(1))\n')
    expect(div.querySelector('h2')?.textContent).toBe('Fixes')
    expect(div.querySelector('img')?.getAttribute('onerror')).toBeNull()
    for (const a of div.querySelectorAll('a')) expect(a.getAttribute('href') ?? '').not.toMatch(/^\s*javascript:/i)
  })
})
